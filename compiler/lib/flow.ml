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

let debug = Debug.find "flow"

let times = Debug.find "times"

let stats = Debug.find "stats"

let debug_stats = Debug.find "stats-debug"

open Code

(****)

let add_var = Var.ISet.add

type def =
  | Phi of Var.Set.t
  | Expr of Code.expr
  | Param

module Info = struct
  type t =
    { info_defs : def array
    ; info_known_origins : Code.Var.Set.t Code.Var.Tbl.t
    ; info_maybe_unknown : bool Code.Var.Tbl.t
    ; info_possibly_mutable : Var.ISet.t
    }

  let def t x =
    match t.info_defs.(Code.Var.idx x) with
    | Phi _ | Param -> None
    | Expr x -> Some x

  let possibly_mutable t x = Code.Var.ISet.mem t.info_possibly_mutable x

  let update_def { info_defs; _ } x exp =
    let idx = Code.Var.idx x in
    info_defs.(idx) <- Expr exp
end

let undefined = Phi Var.Set.empty

let is_undefined d =
  match d with
  | Phi s -> Var.Set.is_empty s
  | _ -> false

let add_expr_def defs x e =
  let idx = Var.idx x in
  assert (is_undefined defs.(idx));
  defs.(idx) <- Expr e

let add_assign_def vars defs x y =
  add_var vars x;
  let idx = Var.idx x in
  match defs.(idx) with
  | Expr _ | Param -> assert false
  | Phi s -> defs.(idx) <- Phi (Var.Set.add y s)

let add_param_def vars defs x =
  add_var vars x;
  let idx = Var.idx x in
  assert (
    match defs.(idx) with
    | Param -> true
    | x -> is_undefined x);
  defs.(idx) <- Param

(* x depends on y *)
let add_dep deps x y =
  let idx = Var.idx y in
  deps.(idx) <- Var.Set.add x deps.(idx)

let rec arg_deps vars deps defs params args =
  match params, args with
  | x :: params, y :: args ->
      add_dep deps x y;
      add_assign_def vars defs x y;
      arg_deps vars deps defs params args
  | [], [] -> ()
  | _ -> assert false

let cont_deps blocks vars deps defs (pc, args) =
  let block = Addr.Map.find pc blocks in
  arg_deps vars deps defs block.params args

let expr_deps blocks vars deps defs x e =
  match e with
  | Constant _ | Apply _ | Prim _ | Special _ -> ()
  | Closure (l, cont, _) ->
      List.iter l ~f:(fun x -> add_param_def vars defs x);
      cont_deps blocks vars deps defs cont
  | Block (_, a, _, _) -> Array.iter a ~f:(fun y -> add_dep deps x y)
  | Field (y, _, _) -> add_dep deps x y

let program_deps { blocks; _ } =
  let nv = Var.count () in
  let vars = Var.ISet.empty () in
  let deps = Array.make nv Var.Set.empty in
  let defs = Array.make nv undefined in
  Addr.Map.iter
    (fun _ block ->
      List.iter block.body ~f:(fun i ->
          match i with
          | Let (x, e) ->
              add_var vars x;
              add_expr_def defs x e;
              expr_deps blocks vars deps defs x e
          | Assign (x, y) ->
              add_dep deps x y;
              add_assign_def vars defs x y
          | Event _ | Set_field _ | Array_set _ | Offset_ref _ -> ());
      match block.branch with
      | Return _ | Raise _ | Stop -> ()
      | Branch cont | Poptrap cont -> cont_deps blocks vars deps defs cont
      | Cond (_, cont1, cont2) ->
          cont_deps blocks vars deps defs cont1;
          cont_deps blocks vars deps defs cont2
      | Switch (_, a1) ->
          Array.iter a1 ~f:(fun cont -> cont_deps blocks vars deps defs cont)
      | Pushtrap (cont, x, cont_h) ->
          add_param_def vars defs x;
          cont_deps blocks vars deps defs cont_h;
          cont_deps blocks vars deps defs cont)
    blocks;
  vars, deps, defs

let var_set_lift f s = Var.Set.fold (fun y s -> Var.Set.union (f y) s) s Var.Set.empty

let propagate1 deps defs st x =
  match defs.(Var.idx x) with
  | Param -> Var.Set.singleton x
  | Phi s -> var_set_lift (fun y -> Var.Tbl.get st y) s
  | Expr e -> (
      match e with
      | Constant _ | Apply _ | Prim _ | Special _ | Closure _ | Block _ ->
          Var.Set.singleton x
      | Field (y, n, _) ->
          if Shape.State.mem x
          then Var.Set.singleton x
          else
            var_set_lift
              (fun z ->
                match defs.(Var.idx z) with
                | Expr (Block (_, a, _, _)) when n < Array.length a ->
                    let t = a.(n) in
                    add_dep deps x t;
                    Var.Tbl.get st t
                | Phi _ | Param | Expr _ -> Var.Set.empty)
              (Var.Tbl.get st y))

module G = Dgraph.Make_Imperative (Var) (Var.ISet) (Var.Tbl)

module Domain1 = struct
  type t = Var.Set.t

  let equal = Var.Set.equal

  let bot = Var.Set.empty
end

module Solver1 = G.Solver (Domain1)

let solver1 vars deps defs =
  let g =
    { G.domain = vars; G.iter_children = (fun f x -> Var.Set.iter f deps.(Var.idx x)) }
  in
  Solver1.f () g (propagate1 deps defs)

(****)

type mutability_state =
  { defs : def array
  ; known_origins : Code.Var.Set.t Code.Var.Tbl.t
  ; may_escape : Code.Var.ISet.t
  ; possibly_mutable : Code.Var.ISet.t
  }

let rec block_escape st x =
  Var.Set.iter
    (fun y ->
      if not (Code.Var.ISet.mem st.may_escape y)
      then (
        Code.Var.ISet.add st.may_escape y;
        match st.defs.(Var.idx y) with
        | Expr (Block (_, l, _, mut)) ->
            (match mut with
            | Immutable -> ()
            | Maybe_mutable -> Code.Var.ISet.add st.possibly_mutable y);
            Array.iter l ~f:(fun z -> block_escape st z)
        | Expr
            (Prim (Extern ("caml_make_array" | "caml_array_of_uniform_array"), [ Pv y ]))
          -> block_escape st y
        | _ -> Code.Var.ISet.add st.possibly_mutable y))
    (Var.Tbl.get st.known_origins x)

let expr_escape st _x e =
  match e with
  | Special _ | Constant _ | Closure _ | Block _ | Field _ -> ()
  | Apply { args; _ } -> List.iter args ~f:(fun x -> block_escape st x)
  | Prim (Array_get, [ Pv x; _ ]) -> block_escape st x
  | Prim ((Vectlength | Array_get | Not | IsInt | Eq | Neq | Lt | Le | Ult), _) -> ()
  | Prim (Extern ("caml_make_array" | "caml_array_of_uniform_array"), [ Pv _ ]) -> ()
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
            block_escape st a;
            loop ax []
        | a :: ax, k :: kx ->
            (match a, k with
            | _, `Const | Pc _, _ -> ()
            | Pv v, `Shallow_const -> (
                match st.defs.(Var.idx v) with
                | Expr (Constant (Tuple _)) -> ()
                | Expr (Block (_, a, _, _)) ->
                    Array.iter a ~f:(fun x -> block_escape st x)
                | Expr
                    (Prim
                       ( Extern ("caml_make_array" | "caml_array_of_uniform_array")
                       , [ Pv y ] )) -> (
                    match st.defs.(Var.idx y) with
                    | Expr (Block (_, a, _, _)) ->
                        Array.iter a ~f:(fun x -> block_escape st x)
                    | _ -> assert false)
                | _ -> block_escape st v)
            | Pv v, `Object_literal -> (
                match st.defs.(Var.idx v) with
                | Expr (Constant (Tuple _)) -> ()
                | Expr (Block (_, a, _, _)) ->
                    Array.iter a ~f:(fun x ->
                        match st.defs.(Var.idx x) with
                        | Expr (Block (_, [| _k; v |], _, _)) -> block_escape st v
                        | Expr (Constant _) -> ()
                        | _ -> block_escape st x)
                | _ -> block_escape st v)
            | Pv v, `Mutable -> block_escape st v);
            loop ax kx
      in
      loop l ka

let program_escape defs known_origins { blocks; _ } =
  let may_escape = Var.ISet.empty () in
  let possibly_mutable = Var.ISet.empty () in
  let st = { defs; known_origins; may_escape; possibly_mutable } in
  Addr.Map.iter
    (fun _ block ->
      List.iter block.body ~f:(fun i ->
          match i with
          | Let (x, e) -> expr_escape st x e
          | Event _ | Assign _ -> ()
          | Set_field (x, _, _, y) | Array_set (x, _, y) ->
              Var.Set.iter
                (fun y -> Var.ISet.add possibly_mutable y)
                (Var.Tbl.get known_origins x);
              block_escape st y
          | Offset_ref (x, _) ->
              Var.Set.iter
                (fun y -> Var.ISet.add possibly_mutable y)
                (Var.Tbl.get known_origins x));
      match block.branch with
      | Return x | Raise (x, _) -> block_escape st x
      | Stop | Branch _ | Cond _ | Switch _ | Pushtrap _ | Poptrap _ -> ())
    blocks;
  possibly_mutable

(****)

let propagate2 defs known_origins possibly_mutable st x =
  match defs.(Var.idx x) with
  | Param -> false
  | Phi s -> Var.Set.exists (fun y -> Var.Tbl.get st y) s
  | Expr e -> (
      match e with
      | Constant _ | Closure _ | Apply _ | Prim _ | Block _ | Special _ -> false
      | Field (y, n, _) ->
          (not (Shape.State.mem x))
          && (Var.Tbl.get st y
             || Var.Set.exists
                  (fun z ->
                    match defs.(Var.idx z) with
                    | Expr (Block (_, a, _, _)) ->
                        n >= Array.length a
                        || Var.ISet.mem possibly_mutable z
                        || Var.Tbl.get st a.(n)
                    | Phi _ | Param | Expr _ -> true)
                  (Var.Tbl.get known_origins y)))

module Domain2 = struct
  type t = bool

  let equal = Bool.equal

  let bot = false
end

module Solver2 = G.Solver (Domain2)

let solver2 vars deps defs known_origins possibly_mutable =
  let g =
    { G.domain = vars; G.iter_children = (fun f x -> Var.Set.iter f deps.(Var.idx x)) }
  in
  Solver2.f () g (propagate2 defs known_origins possibly_mutable)

let get_approx
    { Info.info_defs = _; info_known_origins; info_maybe_unknown; _ }
    f
    top
    join
    x =
  let s = Var.Tbl.get info_known_origins x in
  if Var.Tbl.get info_maybe_unknown x
  then top
  else
    match Var.Set.cardinal s with
    | 0 -> top
    | 1 -> f (Var.Set.choose s)
    | _ -> Var.Set.fold (fun x u -> join (f x) u) s (f (Var.Set.choose s))

let the_def_of info x =
  match x with
  | Pv x ->
      get_approx
        info
        (fun x ->
          match info.info_defs.(Var.idx x) with
          | Expr (Constant (Float _ | Int _ | NativeString _) as e) -> Some e
          | Expr (Constant (Int32 _ | NativeInt _) as e) -> Some e
          | Expr (Constant _ as e) when Config.Flag.safe_string () -> Some e
          | Expr e -> if Var.ISet.mem info.info_possibly_mutable x then None else Some e
          | _ -> None)
        None
        (fun _ _ -> None)
        x
  | Pc c -> Some (Constant c)

let the_const_of ~eq info x =
  match x with
  | Pv x ->
      get_approx
        info
        (fun x ->
          match info.info_defs.(Var.idx x) with
          | Expr
              (Constant
                 (( Float _
                  | Int _
                  | Int32 _
                  | Int64 _
                  | NativeInt _
                  | NativeString _
                  | Float_array _ ) as c)) -> Some c
          | Expr (Constant (String _ as c))
            when not (Var.ISet.mem info.info_possibly_mutable x) -> Some c
          | Expr (Constant c) when Config.Flag.safe_string () -> Some c
          | _ -> None)
        None
        (fun u v ->
          match u, v with
          | Some i, Some j when eq i j -> u
          | _ -> None)
        x
  | Pc c -> Some c

let the_int info x =
  match x with
  | Pv x ->
      get_approx
        info
        (fun x ->
          match info.info_defs.(Var.idx x) with
          | Expr (Constant (Int c)) -> Some c
          | _ -> None)
        None
        (fun u v ->
          match u, v with
          | Some i, Some j when Targetint.equal i j -> u
          | _ -> None)
        x
  | Pc (Int c) -> Some c
  | Pc _ -> None

let string_equal a b =
  match a, b with
  | NativeString a, NativeString b -> Native_string.equal a b
  | String a, String b -> String.equal a b
  (* We don't need to compare other constants, so let's just return false. *)
  | _ -> false

let the_string_of info x =
  match the_const_of ~eq:string_equal info x with
  | Some (String i) -> Some i
  | _ -> None

let the_native_string_of info x =
  match the_const_of ~eq:string_equal info x with
  | Some (NativeString i) -> Some i
  | Some (String i) ->
      (* This function is used to optimize the primitives that access
         object properties. These primitives are expected to work with
         OCaml string as well, considered as byte strings. *)
      Some (Native_string.of_bytestring i)
  | _ -> None

let the_block_contents_of info x =
  match the_def_of info x with
  | Some (Block (_, a, _, _)) -> Some a
  | Some (Prim (Extern ("caml_make_array" | "caml_array_of_uniform_array"), [ x ])) -> (
      match the_def_of info x with
      | Some (Block (_, a, _, _)) -> Some a
      | _ -> None)
  | _ -> None

(*XXX Maybe we could iterate? *)
let direct_approx (info : Info.t) x =
  match info.info_defs.(Var.idx x) with
  | Expr (Field (y, n, _)) ->
      get_approx
        info
        (fun z ->
          if Var.ISet.mem info.info_possibly_mutable z
          then None
          else
            match info.info_defs.(Var.idx z) with
            | Expr (Block (_, a, _, _)) when n < Array.length a -> Some a.(n)
            | _ -> None)
        None
        (fun u v ->
          match u, v with
          | Some n, Some m when Var.compare n m = 0 -> u
          | _ -> None)
        y
  | _ -> None

let the_shape_of ~return_values ~pure info x =
  let rec loop info x acc : Shape.t =
    if Var.Set.mem x acc
    then Top
    else
      let acc = Var.Set.add x acc in
      get_approx
        info
        (fun x ->
          match Shape.State.get x with
          | Some shape -> shape
          | None -> (
              match info.info_defs.(Var.idx x) with
              | Expr (Block (_, a, _, Immutable)) ->
                  Shape.Block (List.map ~f:(fun x -> loop info x acc) (Array.to_list a))
              | Expr (Closure (l, _, _)) ->
                  let pure = Pure_fun.pure pure x in
                  let res =
                    match Var.Map.find x return_values with
                    | exception Not_found -> Shape.Top
                    | set ->
                        let set = Var.Set.remove x set in
                        if Var.Set.is_empty set
                        then Shape.Top
                        else
                          let first = Var.Set.choose set in
                          Var.Set.fold
                            (fun x s1 ->
                              let s2 = loop info x acc in
                              Shape.merge s1 s2)
                            set
                            (loop info first acc)
                  in
                  Shape.Function { arity = List.length l; pure; res }
              | Expr (Special (Alias_prim name)) -> (
                  try
                    let arity = Primitive.arity name in
                    let pure = Primitive.is_pure name in
                    Shape.Function { arity; pure; res = Top }
                  with _ -> Top)
              | Expr (Apply { f; args; _ }) ->
                  let shape = loop info f (Var.Set.add f acc) in
                  let rec loop n' shape =
                    match shape with
                    | Shape.Function { arity = n; pure; res } ->
                        if n = n'
                        then res
                        else if n' < n
                        then Shape.Function { arity = n - n'; pure; res }
                        else loop (n' - n) res
                    | Shape.Block _ | Shape.Top -> Shape.Top
                  in
                  loop (List.length args) shape
              | _ -> Shape.Top))
        Top
        (fun u v -> Shape.merge u v)
        x
  in
  loop info x Var.Set.empty

let build_subst (info : Info.t) vars =
  let nv = Var.count () in
  let subst = Array.init nv ~f:(fun i -> Var.of_idx i) in
  Var.ISet.iter
    (fun x ->
      let x_idx = Var.idx x in
      let u = Var.Tbl.get info.info_maybe_unknown x in
      (if not u
       then
         let s = Var.Tbl.get info.info_known_origins x in
         if Var.Set.cardinal s = 1 then subst.(x_idx) <- Var.Set.choose s);
      (if Var.equal subst.(x_idx) x
       then
         match direct_approx info x with
         | None -> ()
         | Some y -> subst.(x_idx) <- y);
      if Var.equal subst.(x_idx) x then () else Var.propagate_name x subst.(x_idx))
    vars;
  subst

(****)

let f p =
  let previous_p = p in
  Code.invariant p;
  let t = Timer.make () in
  let t1 = Timer.make () in
  let vars, deps, defs = program_deps p in
  if times () then Format.eprintf "    flow analysis 1: %a@." Timer.print t1;
  let t2 = Timer.make () in
  let known_origins = solver1 vars deps defs in
  if times () then Format.eprintf "    flow analysis 2: %a@." Timer.print t2;
  let t3 = Timer.make () in
  let possibly_mutable = program_escape defs known_origins p in
  if times () then Format.eprintf "    flow analysis 3: %a@." Timer.print t3;
  let t4 = Timer.make () in
  let maybe_unknown = solver2 vars deps defs known_origins possibly_mutable in
  if times () then Format.eprintf "    flow analysis 4: %a@." Timer.print t4;
  if debug ()
  then
    Var.ISet.iter
      (fun x ->
        let s = Var.Tbl.get known_origins x in
        if not (Var.Set.is_empty s) (*&& Var.Set.choose s <> x*)
        then
          Format.eprintf
            "%a: {%a} / %s@."
            Var.print
            x
            Code.Print.var_list
            (Var.Set.elements s)
            (if Var.Tbl.get maybe_unknown x then "any" else "known"))
      vars;
  let t5 = Timer.make () in
  let info =
    { Info.info_defs = defs
    ; info_known_origins = known_origins
    ; info_maybe_unknown = maybe_unknown
    ; info_possibly_mutable = possibly_mutable
    }
  in
  let s = build_subst info vars in
  let need_stats = stats () || debug_stats () in
  let count_uniq = ref 0 in
  let count_seen = BitSet.create' (if need_stats then Var.count () else 0) in
  let subst v1 =
    let idx1 = Code.Var.idx v1 in
    let v2 = s.(idx1) in
    if Code.Var.equal v1 v2
    then v1
    else (
      if need_stats && not (BitSet.mem count_seen idx1)
      then (
        incr count_uniq;
        BitSet.set count_seen idx1);
      v2)
  in
  let p = Subst.Excluding_Binders.program subst p in
  if times () then Format.eprintf "    flow analysis 5: %a@." Timer.print t5;
  if times () then Format.eprintf "  flow analysis: %a@." Timer.print t;
  if stats () then Format.eprintf "Stats - flow updates: %d@." !count_uniq;
  if debug_stats () then Code.check_updates ~name:"flow" previous_p p ~updates:!count_uniq;
  Code.invariant p;
  p, info
