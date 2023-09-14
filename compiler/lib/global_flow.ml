(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

(*
The goal of the analysis is to get a good idea of which function might
be called where, and of which functions might be called from some
unknown location (which function 'escapes'). We also keep track of
blocks, to track functions across modules.
*)

open! Stdlib

let debug = Debug.find "global-flow"

let times = Debug.find "times"

open Code

(****)

(* Compute the list of variables containing the return values of each
   function *)
let return_values p =
  Code.fold_closures
    p
    (fun name_opt _ (pc, _) rets ->
      match name_opt with
      | None -> rets
      | Some name ->
          let s =
            Code.traverse
              { fold = fold_children }
              (fun pc s ->
                let block = Addr.Map.find pc p.blocks in
                match fst block.branch with
                | Return x -> Var.Set.add x s
                | _ -> s)
              pc
              p.blocks
              Var.Set.empty
          in
          Var.Map.add name s rets)
    Var.Map.empty

(****)

(* A variable is either let-bound, or a parameter, to which we
   associate a set of possible arguments.
*)
type def =
  | Expr of Code.expr
  | Phi of
      { known : Var.Set.t (* Known arguments *)
      ; others : bool (* Can there be other arguments *)
      }

let undefined = Phi { known = Var.Set.empty; others = false }

let is_undefined d =
  match d with
  | Expr _ -> false
  | Phi { known; others } -> Var.Set.is_empty known && not others

type escape_status =
  | Escape
  | Escape_constant (* Escapes but we know the value is not modified *)
  | No

type state =
  { vars : Var.ISet.t (* Set of all veriables considered *)
  ; deps : Var.Set.t array (* Dependency between variables *)
  ; defs : def array (* Definition of each variable *)
  ; variable_may_escape : escape_status array
        (* Any value bound to this variable may escape *)
  ; variable_possibly_mutable : bool array
        (* Any value bound to this variable may be mutable *)
  ; may_escape : escape_status array (* This value may escape *)
  ; possibly_mutable : bool array (* This value may be mutable *)
  ; return_values : Var.Set.t Var.Map.t
        (* Set of variables holding return values of each function *)
  ; known_cases : (Var.t, int list) Hashtbl.t
        (* Possible tags for a block after a [switch]. This is used to
           get a more precise approximation of the effect of a field
           access [Field] *)
  ; applied_functions : (Var.t * Var.t, unit) Hashtbl.t
        (* Functions that have been already considered at a call site.
           This is to avoid repeated computations *)
  ; fast : bool
  }

let add_var st x = Var.ISet.add st.vars x

(* x depends on y *)
let add_dep st x y =
  let idx = Var.idx y in
  st.deps.(idx) <- Var.Set.add x st.deps.(idx)

let add_expr_def st x e =
  add_var st x;
  let idx = Var.idx x in
  assert (is_undefined st.defs.(idx));
  st.defs.(idx) <- Expr e

let add_assign_def st x y =
  add_var st x;
  add_dep st x y;
  let idx = Var.idx x in
  match st.defs.(idx) with
  | Expr _ -> assert false
  | Phi { known; others } -> st.defs.(idx) <- Phi { known = Var.Set.add y known; others }

let add_param_def st x =
  add_var st x;
  let idx = Var.idx x in
  assert (is_undefined st.defs.(idx));
  if st.fast then st.defs.(idx) <- Phi { known = Var.Set.empty; others = true }

let rec arg_deps st ?ignore params args =
  match params, args with
  | x :: params, y :: args ->
      (* This is to deal with the [else] clause of a conditional,
         where we know that the value of the tested variable is 0. *)
      (match ignore with
      | Some y' when Var.equal y y' -> ()
      | _ -> add_assign_def st x y);
      arg_deps st params args
  | _ -> ()

let cont_deps blocks st ?ignore (pc, args) =
  let block = Addr.Map.find pc blocks in
  arg_deps st ?ignore block.params args

let do_escape st level x = st.variable_may_escape.(Var.idx x) <- level

let possibly_mutable st x = st.variable_possibly_mutable.(Var.idx x) <- true

let expr_deps blocks st x e =
  match e with
  | Constant _ | Prim ((Vectlength | Not | IsInt | Eq | Neq | Lt | Le | Ult), _) | Block _
    -> ()
  | Prim
      ( ( Extern
            ("caml_check_bound" | "caml_array_unsafe_get" | "caml_floatarray_unsafe_get")
        | Array_get )
      , l ) ->
      (* The analysis knowns about these primitives, and will compute
         an approximation of the value they return based on an
         approximation of their arguments *)
      (if st.fast
       then
         match l with
         | Pv x :: _ -> do_escape st Escape x
         | Pc _ :: _ -> ()
         | [] -> assert false);
      List.iter
        ~f:(fun a ->
          match a with
          | Pc _ -> ()
          | Pv y -> add_dep st x y)
        l
  | Prim (Extern name, l) ->
      (* Set the escape status of the arguments *)
      let ka =
        match Primitive.kind_args name with
        | Some l -> l
        | None -> (
            match Primitive.kind name with
            | `Mutable | `Mutator -> []
            | `Pure -> List.map l ~f:(fun _ -> `Const))
      in
      let rec loop args ka =
        match args, ka with
        | [], _ -> ()
        | Pc _ :: ax, [] -> loop ax []
        | Pv a :: ax, [] ->
            do_escape st Escape a;
            loop ax []
        | a :: ax, k :: kx ->
            (match a, k with
            | Pc _, _ -> ()
            | Pv v, `Const -> do_escape st Escape_constant v
            | Pv v, `Shallow_const -> (
                match st.defs.(Var.idx v) with
                | Expr (Block (_, a, _)) ->
                    Array.iter a ~f:(fun x -> do_escape st Escape x)
                | _ -> do_escape st Escape v)
            | Pv v, `Object_literal -> (
                match st.defs.(Var.idx v) with
                | Expr (Block (_, a, _)) ->
                    Array.iter a ~f:(fun x ->
                        match st.defs.(Var.idx x) with
                        | Expr (Block (_, [| _k; v |], _)) -> do_escape st Escape v
                        | _ -> do_escape st Escape x)
                | _ -> do_escape st Escape v)
            | Pv v, `Mutable -> do_escape st Escape v);
            loop ax kx
      in
      loop l ka
  | Apply { f; args; _ } -> (
      add_dep st x f;
      (* If [f] is obviously a function, we can add appropriate
         dependencies right now. This speeds up the analysis
         significantly. *)
      match st.defs.(Var.idx f) with
      | Expr (Closure (params, _)) when List.length args = List.length params ->
          Hashtbl.add st.applied_functions (x, f) ();
          if not st.fast then List.iter2 ~f:(fun p a -> add_assign_def st p a) params args;
          Var.Set.iter (fun y -> add_dep st x y) (Var.Map.find f st.return_values)
      | _ -> ())
  | Closure (l, cont) ->
      List.iter l ~f:(fun x -> add_param_def st x);
      cont_deps blocks st cont
  | Field (y, _) -> add_dep st x y

let program_deps st { blocks; _ } =
  Addr.Map.iter
    (fun _ block ->
      List.iter block.body ~f:(fun (i, _) ->
          match i with
          | Let (x, e) ->
              add_expr_def st x e;
              expr_deps blocks st x e
          | Assign (x, y) -> add_assign_def st x y
          | Set_field (x, _, y) | Array_set (x, _, y) ->
              possibly_mutable st x;
              do_escape st Escape y
          | Offset_ref _ -> ());
      match fst block.branch with
      | Return _ | Stop -> ()
      | Raise (x, _) -> do_escape st Escape x
      | Branch cont | Poptrap cont -> cont_deps blocks st cont
      | Cond (x, cont1, cont2) ->
          cont_deps blocks st cont1;
          cont_deps blocks st ~ignore:x cont2
      | Switch (x, a1, a2) ->
          Array.iter a1 ~f:(fun cont -> cont_deps blocks st cont);
          Array.iter a2 ~f:(fun cont -> cont_deps blocks st cont);
          let h = Hashtbl.create 16 in
          Array.iteri
            ~f:(fun i (pc, _) ->
              Hashtbl.replace h pc (i :: (try Hashtbl.find h pc with Not_found -> [])))
            a2;
          if not st.fast
          then
            Hashtbl.iter
              (fun pc tags ->
                let block = Addr.Map.find pc blocks in
                List.iter
                  ~f:(fun (i, _) ->
                    match i with
                    | Let (y, Field (x', _)) when Var.equal x x' ->
                        Hashtbl.add st.known_cases y tags
                    | _ -> ())
                  block.body)
              h
      | Pushtrap (cont, x, cont_h, _) ->
          add_var st x;
          st.defs.(Var.idx x) <- Phi { known = Var.Set.empty; others = true };
          cont_deps blocks st cont_h;
          cont_deps blocks st cont)
    blocks

(* For each variable, we keep track of which values, function or
   block, it may contain. Other kinds of values are not relevant and
   just ignored. We loose a lot of information when going to [Top]
   since we have to assume that all functions might escape. So, having
   possibly unknown values does not move us to [Top]; we use a flag
   for that instead. *)
type approx =
  | Top
  | Values of
      { known : Var.Set.t (* List of possible values (functions and blocks) *)
      ; others : bool (* Whether other functions or blocks are possible *)
      }

module Domain = struct
  type t = approx

  let bot = Values { known = Var.Set.empty; others = false }

  let others = Values { known = Var.Set.empty; others = true }

  let singleton x = Values { known = Var.Set.singleton x; others = false }

  let equal x y =
    match x, y with
    | Top, Top -> true
    | Values { known; others }, Values { known = known'; others = others' } ->
        Var.Set.equal known known' && Bool.equal others others'
    | Top, Values _ | Values _, Top -> false

  let higher_escape_status s s' =
    match s, s' with
    | Escape, Escape -> false
    | Escape, (Escape_constant | No) -> true
    | Escape_constant, (Escape | Escape_constant) -> false
    | Escape_constant, No -> true
    | No, (Escape | Escape_constant | No) -> false

  let rec value_escape ~update ~st ~approx s x =
    let idx = Var.idx x in
    if higher_escape_status s st.may_escape.(idx)
    then (
      st.may_escape.(idx) <- s;
      match st.defs.(idx) with
      | Expr (Block (_, a, _)) ->
          Array.iter ~f:(fun y -> variable_escape ~update ~st ~approx s y) a;
          if Poly.equal s Escape
          then (
            st.possibly_mutable.(idx) <- true;
            update ~children:true x)
      | Expr (Closure (params, _)) ->
          List.iter
            ~f:(fun y ->
              (match st.defs.(Var.idx y) with
              | Phi { known; _ } -> st.defs.(Var.idx y) <- Phi { known; others = true }
              | Expr _ -> assert false);
              update ~children:false y)
            params;
          Var.Set.iter
            (fun y -> variable_escape ~update ~st ~approx s y)
            (Var.Map.find x st.return_values)
      | _ -> ())

  and variable_escape ~update ~st ~approx s x =
    if higher_escape_status s st.variable_may_escape.(Var.idx x)
    then (
      st.variable_may_escape.(Var.idx x) <- s;
      approx_escape ~update ~st ~approx s (Var.Tbl.get approx x))

  and approx_escape ~update ~st ~approx s a =
    match a with
    | Top -> ()
    | Values { known; _ } ->
        Var.Set.iter (fun x -> value_escape ~update ~st ~approx s x) known

  let join ~update ~st ~approx x y =
    match x, y with
    | Top, _ ->
        approx_escape ~update ~st ~approx Escape y;
        Top
    | _, Top ->
        approx_escape ~update ~st ~approx Escape x;
        Top
    | Values { known; others }, Values { known = known'; others = others' } ->
        Values { known = Var.Set.union known known'; others = others || others' }

  let join_set ~update ~st ~approx ?others:(o = false) f s =
    Var.Set.fold
      (fun x a -> join ~update ~st ~approx (f x) a)
      s
      (if o then others else bot)

  let mark_mutable ~update ~st a =
    match a with
    | Top -> ()
    | Values { known; _ } ->
        Var.Set.iter
          (fun x ->
            if not st.possibly_mutable.(Var.idx x)
            then (
              st.possibly_mutable.(Var.idx x) <- true;
              update ~children:true x))
          known
end

let propagate st ~update approx x =
  match st.defs.(Var.idx x) with
  | Phi { known; others } ->
      Domain.join_set ~update ~st ~approx ~others (fun y -> Var.Tbl.get approx y) known
  | Expr e -> (
      match e with
      | Constant _ ->
          (* A constant cannot contain a function *)
          Domain.bot
      | Closure _ | Block _ -> Domain.singleton x
      | Field (y, n) -> (
          match Var.Tbl.get approx y with
          | Values { known; others } ->
              let tags =
                try Some (Hashtbl.find st.known_cases x) with Not_found -> None
              in
              Domain.join_set
                ~others
                ~update
                ~st
                ~approx
                (fun z ->
                  match st.defs.(Var.idx z) with
                  | Expr (Block (t, a, _))
                    when n < Array.length a
                         &&
                         match tags with
                         | Some tags -> List.memq t ~set:tags
                         | None -> true ->
                      let t = a.(n) in
                      add_dep st x t;
                      let a = Var.Tbl.get approx t in
                      if st.possibly_mutable.(Var.idx z)
                      then Domain.join ~update ~st ~approx Domain.others a
                      else a
                  | Expr (Block _ | Closure _) -> Domain.bot
                  | Phi _ | Expr _ -> assert false)
                known
          | Top -> Top)
      | Prim (Extern "caml_check_bound", [ Pv y; _ ]) -> Var.Tbl.get approx y
      | Prim
          ( (Array_get | Extern ("caml_array_unsafe_get" | "caml_floatarray_unsafe_get"))
          , [ Pv y; _ ] ) -> (
          if st.fast
          then Domain.others
          else
            match Var.Tbl.get approx y with
            | Values { known; others } ->
                Domain.join_set
                  ~update
                  ~st
                  ~approx
                  ~others
                  (fun z ->
                    match st.defs.(Var.idx z) with
                    | Expr (Block (_, lst, _)) ->
                        Array.iter ~f:(fun t -> add_dep st x t) lst;
                        let a =
                          Array.fold_left
                            ~f:(fun acc t ->
                              Domain.join ~update ~st ~approx (Var.Tbl.get approx t) acc)
                            ~init:Domain.bot
                            lst
                        in
                        if st.possibly_mutable.(Var.idx z)
                        then Domain.join ~update ~st ~approx Domain.others a
                        else a
                    | Expr (Closure _) -> Domain.bot
                    | Phi _ | Expr _ -> assert false)
                  known
            | Top -> Top)
      | Prim (Array_get, _) -> Domain.others
      | Prim ((Vectlength | Not | IsInt | Eq | Neq | Lt | Le | Ult), _) ->
          (* The result of these primitive is neither a function nor a
             block *)
          Domain.bot
      | Prim (Extern _, _) -> Domain.others
      | Apply { f; args; _ } -> (
          match Var.Tbl.get approx f with
          | Values { known; others } ->
              if others
              then
                List.iter
                  ~f:(fun y -> Domain.variable_escape ~update ~st ~approx Escape y)
                  args;
              Domain.join_set
                ~update
                ~st
                ~approx
                ~others
                (fun g ->
                  match st.defs.(Var.idx g) with
                  | Expr (Closure (params, _)) when List.length args = List.length params
                    ->
                      if not (Hashtbl.mem st.applied_functions (x, g))
                      then (
                        Hashtbl.add st.applied_functions (x, g) ();
                        if not st.fast
                        then
                          List.iter2
                            ~f:(fun p a ->
                              add_assign_def st p a;
                              update ~children:false p)
                            params
                            args;
                        Var.Set.iter
                          (fun y -> add_dep st x y)
                          (Var.Map.find g st.return_values));
                      Domain.join_set
                        ~update
                        ~st
                        ~approx
                        (fun y -> Var.Tbl.get approx y)
                        (Var.Map.find g st.return_values)
                  | Expr (Closure (_, _)) ->
                      (* The funciton is partially applied or over applied *)
                      List.iter
                        ~f:(fun y -> Domain.variable_escape ~update ~st ~approx Escape y)
                        args;
                      Domain.variable_escape ~update ~st ~approx Escape g;
                      Domain.others
                  | Expr (Block _) -> Domain.bot
                  | Phi _ | Expr _ -> assert false)
                known
          | Top ->
              List.iter
                ~f:(fun y -> Domain.variable_escape ~update ~st ~approx Escape y)
                args;
              Top))

let propagate st ~update approx x =
  let res = propagate st ~update approx x in
  match res with
  | Values { known; _ } when Var.Set.cardinal known >= 200 ->
      (* When the set of possible values get to large, we give up and
         just forget about it. This is crucial to make the analysis
         terminates in a reasonable amount of time. This happens when
         our analysis is very imprecise (for instance, with
         [List.map]), so we may not loose too much by doing that. *)
      if debug () then Format.eprintf "TOP %a@." Var.print x;
      Domain.approx_escape ~update ~st ~approx Escape res;
      Top
  | Values _ ->
      (match st.variable_may_escape.(Var.idx x) with
      | (Escape | Escape_constant) as s -> Domain.approx_escape ~update ~st ~approx s res
      | No -> ());
      if st.variable_possibly_mutable.(Var.idx x) then Domain.mark_mutable ~update ~st res;
      res
  | Top -> Top

module G = Dgraph.Make_Imperative (Var) (Var.ISet) (Var.Tbl)
module Solver = G.Solver (Domain)

let solver st =
  let g =
    { G.domain = st.vars
    ; G.iter_children = (fun f x -> Var.Set.iter f st.deps.(Var.idx x))
    }
  in
  Solver.f' () g (propagate st)

(****)

type info =
  { info_defs : def array
  ; info_approximation : Domain.t Var.Tbl.t
  ; info_may_escape : Var.ISet.t
  }

let f ~fast p =
  let t = Timer.make () in
  let t1 = Timer.make () in
  let rets = return_values p in
  let nv = Var.count () in
  let vars = Var.ISet.empty () in
  let deps = Array.make nv Var.Set.empty in
  let defs = Array.make nv undefined in
  let variable_may_escape = Array.make nv No in
  let variable_possibly_mutable = Array.make nv false in
  let may_escape = Array.make nv No in
  let possibly_mutable = Array.make nv false in
  let st =
    { vars
    ; deps
    ; defs
    ; return_values = rets
    ; variable_may_escape
    ; variable_possibly_mutable
    ; may_escape
    ; possibly_mutable
    ; known_cases = Hashtbl.create 16
    ; applied_functions = Hashtbl.create 16
    ; fast
    }
  in
  program_deps st p;
  if times ()
  then Format.eprintf "      global flow analysis (initialize): %a@." Timer.print t1;
  let t2 = Timer.make () in
  let approximation = solver st in
  if times ()
  then Format.eprintf "      global flow analysis (solve): %a@." Timer.print t2;
  if times () then Format.eprintf "    global flow analysis: %a@." Timer.print t;
  if debug ()
  then
    Var.ISet.iter
      (fun x ->
        let s = Var.Tbl.get approximation x in
        if not (Domain.equal s Domain.bot)
        then
          Format.eprintf
            "%a: %a@."
            Var.print
            x
            (fun f a ->
              match a with
              | Top -> Format.fprintf f "top"
              | Values { known; others } ->
                  Format.fprintf
                    f
                    "{%a/%b} mut:%b vmut:%b esc:%s"
                    (Format.pp_print_list
                       ~pp_sep:(fun f () -> Format.fprintf f ", ")
                       (fun f x ->
                         Format.fprintf
                           f
                           "%a(%s)"
                           Var.print
                           x
                           (match st.defs.(Var.idx x) with
                           | Expr (Closure _) -> "C"
                           | Expr (Block _) ->
                               "B"
                               ^
                               if Poly.equal st.may_escape.(Var.idx x) Escape
                               then "X"
                               else ""
                           | _ -> "O")))
                    (Var.Set.elements known)
                    others
                    st.possibly_mutable.(Var.idx x)
                    st.variable_possibly_mutable.(Var.idx x)
                    (match st.may_escape.(Var.idx x) with
                    | Escape -> "Y"
                    | Escape_constant -> "y"
                    | No -> "n"))
            s)
      vars;
  let info_may_escape = Var.ISet.empty () in
  Array.iteri
    ~f:(fun i s -> if Poly.(s <> No) then Var.ISet.add info_may_escape (Var.of_idx i))
    may_escape;
  { info_defs = defs; info_approximation = approximation; info_may_escape }

let exact_call info f n =
  match Var.Tbl.get info.info_approximation f with
  | Top | Values { others = true; _ } -> false
  | Values { known; others = false } ->
      Var.Set.for_all
        (fun g ->
          match info.info_defs.(Var.idx g) with
          | Expr (Closure (params, _)) -> List.length params = n
          | Expr (Block _) -> true
          | Expr _ | Phi _ -> assert false)
        known

let function_arity info f =
  match Var.Tbl.get info.info_approximation f with
  | Top | Values { others = true; _ } -> None
  | Values { known; others = false } -> (
      match
        Var.Set.fold
          (fun g acc ->
            match info.info_defs.(Var.idx g) with
            | Expr (Closure (params, _)) -> (
                let n = List.length params in
                match acc with
                | None -> Some (Some n)
                | Some (Some n') when n <> n' -> Some None
                | Some _ -> acc)
            | Expr (Block _) -> acc
            | Expr _ | Phi _ -> assert false)
          known
          None
      with
      | Some v -> v
      | None -> None)
