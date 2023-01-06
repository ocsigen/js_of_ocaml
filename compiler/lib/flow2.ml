(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2010 Jérôme Vouillon
 * Laboratoire PPS - CNRS Université Paris Diderot
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

open! Stdlib

let debug = Debug.find "flow2"

let times = Debug.find "times"

open Code

(****)

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
                match block.branch with
                | Return x -> Var.Set.add x s
                | _ -> s)
              pc
              p.blocks
              Var.Set.empty
          in
          Var.Map.add name s rets)
    Var.Map.empty

(****)

let add_var = Var.ISet.add

type def =
  | Phi of
      { known : Var.Set.t
      ; others : bool
      }
  | Expr of Code.expr

(*
Status:
   escape/const (recursive)
   mutable (local)
   escape (recursive)
*)
type state =
  { vars : Var.ISet.t
  ; deps : Var.Set.t array
  ; defs : def array
  ; variable_may_escape : [ `Escape | `Escape_constant | `No ] array
  ; variable_possibly_mutable : bool array
  ; may_escape : [ `Escape | `Escape_constant | `No ] array
  ; possibly_mutable : bool array
  ; return_values : Var.Set.t Var.Map.t
  ; known_case : (Var.t, int list) Hashtbl.t
  }

let undefined = Phi { known = Var.Set.empty; others = false }

let is_undefined d =
  match d with
  | Phi { known; others } -> (not others) && Var.Set.is_empty known
  | _ -> false

let add_expr_def st x e =
  let idx = Var.idx x in
  assert (is_undefined st.defs.(idx));
  st.defs.(idx) <- Expr e

let add_assign_def st x y =
  add_var st.vars x;
  let idx = Var.idx x in
  match st.defs.(idx) with
  | Expr _ -> assert false
  | Phi { known; others } -> st.defs.(idx) <- Phi { known = Var.Set.add y known; others }

let add_param_def st x =
  add_var st.vars x;
  let idx = Var.idx x in
  assert (is_undefined st.defs.(idx))

(* x depends on y *)
let add_dep st x y =
  let idx = Var.idx y in
  st.deps.(idx) <- Var.Set.add x st.deps.(idx)

let rec arg_deps st ?ignore params args =
  match params, args with
  | x :: params, y :: args ->
      if not
           (match ignore with
           | Some y' -> Var.equal y y'
           | _ -> false)
      then (
        add_dep st x y;
        add_assign_def st x y);
      arg_deps st params args
  | _ -> ()

let cont_deps blocks st ?ignore (pc, args) =
  let block = Addr.Map.find pc blocks in
  arg_deps st ?ignore block.params args

let h = Hashtbl.create 16

let expr_deps blocks st x e =
  match e with
  | Constant _ -> ()
  | Prim (_, l) ->
      List.iter
        ~f:(fun a ->
          match a with
          | Pc _ -> ()
          | Pv y -> add_dep st x y)
        l
  | Apply { f; args; _ } -> (
      add_dep st x f;
      match st.defs.(Var.idx f) with
      | Expr (Closure (params, _)) when List.length args = List.length params ->
          Hashtbl.add h (x, f) ();
          List.iter2
            ~f:(fun x y ->
              add_dep st x y;
              let idx = Var.idx x in
              match st.defs.(idx) with
              | Expr _ -> assert false
              | Phi { known; others } ->
                  st.defs.(idx) <- Phi { known = Var.Set.add y known; others })
            params
            args;
          Var.Set.iter (fun y -> add_dep st x y) (Var.Map.find f st.return_values)
      | _ -> ())
  | Closure (l, cont) ->
      List.iter l ~f:(fun x -> add_param_def st x);
      cont_deps blocks st cont
  | Block (_, a, _) -> Array.iter a ~f:(fun y -> add_dep st x y)
  | Field (y, _) -> add_dep st x y

let do_escape st x = st.variable_may_escape.(Var.idx x) <- `Escape

let possibly_mutable st x = st.variable_possibly_mutable.(Var.idx x) <- true

let program_deps st { blocks; _ } =
  Addr.Map.iter
    (fun _ block ->
      List.iter block.body ~f:(fun i ->
          match i with
          | Let (x, e) ->
              add_var st.vars x;
              add_expr_def st x e;
              expr_deps blocks st x e
          | Assign (x, y) ->
              add_dep st x y;
              add_assign_def st x y
          | Set_field (x, _, y) | Array_set (x, _, y) ->
              possibly_mutable st x;
              do_escape st y
          | Offset_ref _ -> ());
      match block.branch with
      | Return _ | Stop -> ()
      | Raise (x, _) -> do_escape st x
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
          Hashtbl.iter
            (fun pc tags ->
              let block = Addr.Map.find pc blocks in
              List.iter
                ~f:(fun i ->
                  match i with
                  | Let (y, Field (x', _)) when Var.equal x x' ->
                      Hashtbl.add st.known_case y tags
                  | _ -> ())
                block.body)
            h
      | Pushtrap (cont, x, cont_h, _) ->
          add_var st.vars x;
          let idx = Var.idx x in
          st.defs.(idx) <- Phi { known = Var.Set.empty; others = true };
          cont_deps blocks st cont_h;
          cont_deps blocks st cont)
    blocks

module D = struct
  type approx =
    | Top
    | Values of
        { known : Var.Set.t
        ; others : bool
        }

  let higher_escape_status s s' =
    match s, s' with
    | `Escape, `Escape -> false
    | `Escape, (`Escape_constant | `No) -> true
    | `Escape_constant, (`Escape | `Escape_constant) -> false
    | `Escape_constant, `No -> true
    | `No, (`Escape | `Escape_constant | `No) -> false

  let rec value_escape ~update ~st ~st' s x =
    let idx = Var.idx x in
    if higher_escape_status s st.may_escape.(idx)
    then (
      st.may_escape.(idx) <- s;
      if Poly.equal s `Escape then st.possibly_mutable.(idx) <- true;
      match st.defs.(idx) with
      | Expr (Block (_, a, _)) ->
          Array.iter ~f:(fun y -> variable_escape ~update ~st ~st' s y) a;
          if Poly.equal s `Escape then update ~deps:true x
      | Expr (Closure (params, _)) ->
          List.iter
            ~f:(fun y ->
              (match st.defs.(Var.idx y) with
              | Phi { known; _ } -> st.defs.(Var.idx y) <- Phi { known; others = true }
              | Expr _ -> assert false);
              if Poly.equal s `Escape then update ~deps:false y)
            params;
          Var.Set.iter
            (fun y -> variable_escape ~update ~st ~st' s y)
            (Var.Map.find x st.return_values)
      | _ -> ())

  and variable_escape ~update ~st ~st' s x =
    if higher_escape_status s st.variable_may_escape.(Var.idx x)
    then st.variable_may_escape.(Var.idx x) <- s;
    approx_escape ~update ~st ~st' s (Var.Tbl.get st' x)

  and approx_escape ~update ~st ~st' s a =
    match a with
    | Top -> ()
    | Values { known; _ } ->
        Var.Set.iter (fun x -> value_escape ~update ~st ~st' s x) known

  let join ~update ~st ~st' x y =
    match x, y with
    | Top, _ ->
        approx_escape ~update ~st ~st' `Escape y;
        Top
    | _, Top ->
        approx_escape ~update ~st ~st' `Escape x;
        Top
    | Values { known; others }, Values { known = known'; others = others' } ->
        Values { known = Var.Set.union known known'; others = others || others' }

  let bottom = Values { known = Var.Set.empty; others = false }

  let others = Values { known = Var.Set.empty; others = true }

  let singleton x = Values { known = Var.Set.singleton x; others = false }

  let join_set ~update ~st ~st' ?others:(o = false) f s =
    Var.Set.fold
      (fun x a -> join ~update ~st ~st' (f x) a)
      s
      (if o then others else bottom)

  let mark_mutable ~update ~st ~st':_ a =
    match a with
    | Top -> ()
    | Values { known; _ } ->
        Var.Set.iter
          (fun x ->
            if not st.possibly_mutable.(Var.idx x)
            then (
              st.possibly_mutable.(Var.idx x) <- true;
              update ~deps:true x))
          known
end

let propagate1 st update st' x =
  let approx =
    match st.defs.(Var.idx x) with
    | Phi { known; others } ->
        D.join_set ~update ~st ~st' ~others (fun y -> Var.Tbl.get st' y) known
    | Expr e -> (
        match e with
        | Constant _ -> D.bottom
        | Closure _ -> D.singleton x
        | Block _ -> D.singleton x
        | Field (y, n) -> (
            match Var.Tbl.get st' y with
            | Values { known; others } ->
                D.join_set
                  ~others
                  ~update
                  ~st
                  ~st'
                  (fun z ->
                    match st.defs.(Var.idx z) with
                    | Expr (Block (t, a, _))
                      when n < Array.length a
                           &&
                           try List.mem t ~set:(Hashtbl.find st.known_case x)
                           with Not_found -> true ->
                        let t = a.(n) in
                        add_dep st x t;
                        let a = Var.Tbl.get st' t in
                        if st.possibly_mutable.(Var.idx z)
                        then D.join ~update ~st ~st' D.others a
                        else a
                    | Expr (Block _) | Expr (Closure _) -> D.bottom
                    | Phi _ | Expr _ -> assert false)
                  known
            | Top -> Top)
        | Prim (Extern "caml_check_bound", [ Pv y; _ ]) -> Var.Tbl.get st' y
        | Prim (Array_get, [ Pv y; _ ])
        | Prim (Extern "caml_array_unsafe_get", [ Pv y; _ ]) -> (
            match Var.Tbl.get st' y with
            | Values { known; others } ->
                D.join_set
                  ~update
                  ~st
                  ~st'
                  ~others
                  (fun z ->
                    match st.defs.(Var.idx z) with
                    | Expr (Block (_, a, _)) ->
                        Array.iter ~f:(fun t -> add_dep st x t) a;
                        let approx =
                          Array.fold_left
                            ~f:(fun acc t ->
                              D.join ~update ~st ~st' (Var.Tbl.get st' t) acc)
                            ~init:D.bottom
                            a
                        in
                        if st.possibly_mutable.(Var.idx z)
                        then D.join ~update ~st ~st' D.others approx
                        else approx
                    | Expr (Closure _) -> D.bottom
                    | Phi _ | Expr _ -> assert false)
                  known
            | Top -> Top)
        | Prim ((Array_get | Vectlength | Not | IsInt | Eq | Neq | Lt | Le | Ult), _) ->
            D.bottom
        | Prim (Extern name, l) ->
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
                  D.variable_escape ~update ~st ~st' `Escape a;
                  loop ax []
              | a :: ax, k :: kx ->
                  (match a, k with
                  | Pc _, _ -> ()
                  | Pv v, `Const -> D.variable_escape ~update ~st ~st' `Escape_constant v
                  | Pv v, `Shallow_const -> (
                      match st.defs.(Var.idx v) with
                      | Expr (Block (_, a, _)) ->
                          Array.iter a ~f:(fun x ->
                              D.variable_escape ~update ~st ~st' `Escape x)
                      | _ -> D.variable_escape ~update ~st ~st' `Escape v)
                  | Pv v, `Object_literal -> (
                      match st.defs.(Var.idx v) with
                      | Expr (Block (_, a, _)) ->
                          Array.iter a ~f:(fun x ->
                              match st.defs.(Var.idx x) with
                              | Expr (Block (_, [| _k; v |], _)) ->
                                  D.variable_escape ~update ~st ~st' `Escape v
                              | _ -> D.variable_escape ~update ~st ~st' `Escape x)
                      | _ -> D.variable_escape ~update ~st ~st' `Escape v)
                  | Pv v, `Mutable -> D.variable_escape ~update ~st ~st' `Escape v);
                  loop ax kx
            in
            loop l ka;
            D.others
        | Apply { f; args; _ } -> (
            match Var.Tbl.get st' f with
            | Values { known; others } ->
                if others
                then
                  List.iter
                    ~f:(fun y -> D.variable_escape ~update ~st ~st' `Escape y)
                    args;
                D.join_set
                  ~update
                  ~st
                  ~st'
                  ~others
                  (fun g ->
                    match st.defs.(Var.idx g) with
                    | Expr (Closure (params, _))
                      when List.length args = List.length params ->
                        if not (Hashtbl.mem h (x, g))
                        then (
                          Hashtbl.add h (x, g) ();
                          let escape = st.may_escape.(Var.idx g) in
                          List.iter2
                            ~f:(fun x y ->
                              add_dep st x y;
                              let idx = Var.idx x in
                              match st.defs.(idx) with
                              | Expr _ -> assert false
                              | Phi { known; others } ->
                                  update ~deps:false x;
                                  st.defs.(idx) <-
                                    Phi
                                      { known = Var.Set.add y known
                                      ; others = others || Poly.equal escape `Escape
                                      })
                            params
                            args;
                          Var.Set.iter
                            (fun y -> add_dep st x y)
                            (Var.Map.find g st.return_values));
                        D.join_set
                          ~update
                          ~st
                          ~st'
                          (fun y -> Var.Tbl.get st' y)
                          (Var.Map.find g st.return_values)
                    | Expr (Closure (_params, _)) ->
                        (*
Overapplied:
   the value of this expression depends on the return values
   get the return values and apply the extra parameters to them
Underapplied:
   match known args to params; 
*)
                        (*
                        Format.eprintf
                          "ZZZ params:%d args:%d %b %a@."
                          (List.length params)
                          (List.length args)
                          (List.length params > List.length args)
                          Var.print
                          g;
*)
                        (*ZZZ Arguments escape ? *)
                        (* Partially applied or over applied *)
                        List.iter
                          ~f:(fun y -> D.variable_escape ~update ~st ~st' `Escape y)
                          args;
                        D.variable_escape ~update ~st ~st' `Escape g;
                        D.others
                    | Expr (Block _) -> D.bottom
                    | Phi _ | Expr _ -> assert false)
                  known
            | Top ->
                List.iter ~f:(fun y -> D.variable_escape ~update ~st ~st' `Escape y) args;

                D.Top))
  in
  (match st.variable_may_escape.(Var.idx x) with
  | (`Escape | `Escape_constant) as s ->
      (*      Format.eprintf "ESCAPE %a@." Var.print x;*)
      D.approx_escape ~update ~st ~st' s approx
  | `No -> ());
  if st.variable_possibly_mutable.(Var.idx x) then D.mark_mutable ~update ~st ~st' approx;
  approx

let propagate1 st update st' x =
  let res = propagate1 st update st' x in
  (*
  (match res, Var.Tbl.get st' x with
  | Values s, Values s' ->
      Format.eprintf
        "%a (%b) : %a@."
        Var.print
        x
        (Var.Set.equal s s')
        Print.var_list
        (Var.Set.elements s)
  | _ -> ());
 *)
  match res with
  | Values { known; _ } when Var.Set.cardinal known < 200 -> res
  | Values _ ->
      (*      Format.eprintf "TOP %a@." Var.print x;*)
      D.approx_escape ~update ~st ~st' `Escape res;
      Top
  | Top -> Top

module G = Dgraph.Make_Imperative (Var) (Var.ISet) (Var.Tbl)

module Domain1 = struct
  type t = D.approx

  let equal x y =
    match x, y with
    | D.Top, D.Top -> true
    | Values { known; others }, Values { known = known'; others = others' } ->
        Var.Set.equal known known' && Bool.equal others others'
    | Top, Values _ | Values _, Top -> false

  let bot = D.bottom
end

module Solver1 = G.Solver (Domain1)

let solver1 st =
  let g =
    { G.domain = st.vars
    ; G.iter_children = (fun f x -> Var.Set.iter f st.deps.(Var.idx x))
    }
  in
  Solver1.f' () g (propagate1 st)

(****)

type info =
  { info_defs : def array
  ; info_approximation : D.approx Var.Tbl.t
  ; info_may_escape : bool array
  ; info_possibly_mutable : bool array
  }

let f p =
  Code.invariant p;
  (*  let t = Timer.make () in*)
  (*  Format.eprintf "vvvvv@.";*)
  let t1 = Timer.make () in
  let rets = return_values p in
  let nv = Var.count () in
  let vars = Var.ISet.empty () in
  let deps = Array.make nv Var.Set.empty in
  let defs = Array.make nv undefined in
  let variable_may_escape = Array.make nv `No in
  let variable_possibly_mutable = Array.make nv false in
  let may_escape = Array.make nv `No in
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
    ; known_case = Hashtbl.create 16
    }
  in
  program_deps st p;
  if times () then Format.eprintf "    flow analysis 1: %a@." Timer.print t1;
  let t2 = Timer.make () in
  let approximation = solver1 st in
  if times () then Format.eprintf "    flow analysis 2: %a@." Timer.print t2;
  if debug ()
  then
    Var.ISet.iter
      (fun x ->
        let s = Var.Tbl.get approximation x in
        if not (Poly.( = ) s D.bottom) (*&& Var.Set.choose s <> x*)
        then
          Format.eprintf
            "%a: %a@."
            Var.print
            x
            (fun f a ->
              match a with
              | D.Top -> Format.fprintf f "top"
              | Values { known; others } ->
                  Format.fprintf
                    f
                    "{%a/%b} mut:%b vmut:%b"
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
                               if Poly.equal st.may_escape.(Var.idx x) `Escape
                               then "X"
                               else ""
                           | _ -> "O")))
                    (Var.Set.elements known)
                    others
                    st.possibly_mutable.(Var.idx x)
                    st.variable_possibly_mutable.(Var.idx x))
            s)
      vars;
  { info_defs = defs
  ; info_approximation = approximation
  ; info_may_escape = Array.map ~f:(fun s -> Poly.(s <> `No)) may_escape
  ; info_possibly_mutable = possibly_mutable
  }

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
