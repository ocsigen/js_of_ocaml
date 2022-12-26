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

(*
x = Apply (f, [a, b,c])

===> x is the result of f (if number of parameters match)
     bind parameters of f to a,b,c
If f changes, needs to update


First step: propagate forward possible known values.
Second step: propagate backward whether the value escape, whether it may be modified.
Third step: propagate forward whether we know all possible values of a variable

Domain:
   set of functions
     or
   set of blocks
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
  | Phi of Var.Set.t
  | Expr of Code.expr

type info =
  { info_defs : def array
  ; info_known_origins : Code.Var.Set.t Code.Var.Tbl.t
  ; info_maybe_unknown : bool Code.Var.Tbl.t
  ; info_possibly_mutable : bool array
  }

let update_def { info_defs; _ } x exp =
  let idx = Code.Var.idx x in
  info_defs.(idx) <- Expr exp

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
  | Expr _ -> assert false
  | Phi s -> defs.(idx) <- Phi (Var.Set.add y s)

let add_param_def vars defs x =
  add_var vars x;
  let idx = Var.idx x in
  assert (is_undefined defs.(idx))

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
  | _ -> ()

let cont_deps blocks vars deps defs (pc, args) =
  let block = Addr.Map.find pc blocks in
  arg_deps vars deps defs block.params args

let expr_deps blocks vars deps defs x e =
  match e with
  | Constant _ | Prim _ -> ()
  | Apply { f; _ } -> add_dep deps x f
  | Closure (l, cont) ->
      List.iter l ~f:(fun x -> add_param_def vars defs x);
      cont_deps blocks vars deps defs cont
  | Block (_, a, _) -> Array.iter a ~f:(fun y -> add_dep deps x y)
  | Field (y, _) -> add_dep deps x y

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
          | Set_field _ | Array_set _ | Offset_ref _ -> ());
      match block.branch with
      | Return _ | Raise _ | Stop -> ()
      | Branch cont | Poptrap cont -> cont_deps blocks vars deps defs cont
      | Cond (_, cont1, cont2) ->
          cont_deps blocks vars deps defs cont1;
          cont_deps blocks vars deps defs cont2
      | Switch (_, a1, a2) ->
          Array.iter a1 ~f:(fun cont -> cont_deps blocks vars deps defs cont);
          Array.iter a2 ~f:(fun cont -> cont_deps blocks vars deps defs cont)
      | Pushtrap (cont, x, cont_h, _) ->
          add_param_def vars defs x;
          cont_deps blocks vars deps defs cont_h;
          cont_deps blocks vars deps defs cont)
    blocks;
  vars, deps, defs

(*
type approx =
  | Top
  | Functions of Var.Set.t
  | Blocks of Var.Set.t
  | Bottom

let join
*)

let var_set_lift f s = Var.Set.fold (fun y s -> Var.Set.union (f y) s) s Var.Set.empty

let h = Hashtbl.create 16

let propagate1 deps rets defs st x =
  match defs.(Var.idx x) with
  | Phi s -> var_set_lift (fun y -> Var.Tbl.get st y) s
  | Expr e -> (
      match e with
      | Constant _ | Prim _ | Closure _ | Block _ -> Var.Set.singleton x
      | Field (y, n) ->
          var_set_lift
            (fun z ->
              match defs.(Var.idx z) with
              | Expr (Block (_, a, _)) when n < Array.length a ->
                  let t = a.(n) in
                  add_dep deps x t;
                  Var.Tbl.get st t
              | Phi _ | Expr _ -> Var.Set.empty)
            (Var.Tbl.get st y)
      | Apply { f; args; _ } ->
          var_set_lift
            (fun g ->
              match defs.(Var.idx g) with
              | Expr (Closure (params, _)) when List.length args = List.length params ->
                  (*
                  Format.eprintf "ZZZ %d => %d@." (Var.idx x) (Var.idx g);*)
                  (* ZZZ Only if g has not yet been associated to x *)
                  if not (Hashtbl.mem h (x, g))
                  then (
                    Hashtbl.add h (x, g) ();
                    List.iter2
                      ~f:(fun x y ->
                        add_dep deps x y;
                        let idx = Var.idx x in
                        match defs.(idx) with
                        | Expr _ -> assert false
                        | Phi s ->
                            ();
                            (* ZZZ need to recompute x *)
                            defs.(idx) <- Phi (Var.Set.add y s))
                      params
                      args;
                    Var.Set.iter (fun y -> add_dep deps x y) (Var.Map.find g rets));
                  var_set_lift (fun y -> Var.Tbl.get st y) (Var.Map.find g rets)
              | Phi _ | Expr _ -> Var.Set.empty)
            (Var.Tbl.get st f))

module G = Dgraph.Make_Imperative (Var) (Var.ISet) (Var.Tbl)

module Domain1 = struct
  type t = Var.Set.t

  let equal = Var.Set.equal

  let bot = Var.Set.empty
end

module Solver1 = G.Solver (Domain1)

let solver1 vars deps rets defs =
  let g =
    { G.domain = vars; G.iter_children = (fun f x -> Var.Set.iter f deps.(Var.idx x)) }
  in
  Solver1.f () g (propagate1 deps rets defs)

(****)

type mutability_state =
  { defs : def array
  ; known_origins : Code.Var.Set.t Code.Var.Tbl.t
  ; may_escape : bool array
  ; possibly_mutable : bool array
  ; pessimistic : bool
  }

let rec block_escape st x =
  Var.Set.iter
    (fun y ->
      let idx = Var.idx y in
      if not st.may_escape.(idx)
      then (
        st.may_escape.(idx) <- true;
        st.possibly_mutable.(idx) <- true;
        match st.defs.(Var.idx y) with
        | Expr (Block (_, l, _)) -> Array.iter l ~f:(fun z -> block_escape st z)
        | _ -> ()))
    (Var.Tbl.get st.known_origins x)

let expr_escape st _x e =
  match e with
  | Constant _ | Closure _ | Block _ | Field _ -> ()
  | Apply { args; _ } -> List.iter args ~f:(fun x -> block_escape st x)
  | Prim ((Vectlength | Array_get | Not | IsInt | Eq | Neq | Lt | Le | Ult), _) -> ()
  | Prim (Extern name, l) ->
      let ka =
        if st.pessimistic
        then []
        else
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
                | Expr (Block (_, a, _)) -> Array.iter a ~f:(fun x -> block_escape st x)
                | _ -> block_escape st v)
            | Pv v, `Object_literal -> (
                match st.defs.(Var.idx v) with
                | Expr (Block (_, a, _)) ->
                    Array.iter a ~f:(fun x ->
                        match st.defs.(Var.idx x) with
                        | Expr (Block (_, [| _k; v |], _)) -> block_escape st v
                        | _ -> block_escape st x)
                | _ -> block_escape st v)
            | Pv v, `Mutable -> block_escape st v);
            loop ax kx
      in
      loop l ka

let program_escape ?(pessimistic = false) defs known_origins { blocks; _ } =
  let nv = Var.count () in
  let may_escape = Array.make nv false in
  let possibly_mutable = Array.make nv false in
  let st = { defs; known_origins; may_escape; possibly_mutable; pessimistic } in
  Addr.Map.iter
    (fun _ block ->
      List.iter block.body ~f:(fun i ->
          match i with
          | Let (x, e) -> expr_escape st x e
          | Assign _ -> ()
          | Set_field (x, _, y) | Array_set (x, _, y) ->
              Var.Set.iter
                (fun y -> possibly_mutable.(Var.idx y) <- true)
                (Var.Tbl.get known_origins x);
              block_escape st y
          | Offset_ref (x, _) ->
              Var.Set.iter
                (fun y -> possibly_mutable.(Var.idx y) <- true)
                (Var.Tbl.get known_origins x));
      match block.branch with
      | Return x | Raise (x, _) -> block_escape st x
      | Stop | Branch _ | Cond _ | Switch _ | Pushtrap _ | Poptrap _ -> ())
    blocks;
  possibly_mutable

(****)

let propagate2 defs known_origins possibly_mutable st x =
  match defs.(Var.idx x) with
  | Phi s -> Var.Set.exists (fun y -> Var.Tbl.get st y) s
  | Expr e -> (
      match e with
      | Constant _ | Closure _ | Apply _ | Prim _ | Block _ -> false
      | Field (y, n) ->
          Var.Tbl.get st y
          || Var.Set.exists
               (fun z ->
                 match defs.(Var.idx z) with
                 | Expr (Block (_, a, _)) ->
                     n >= Array.length a
                     || possibly_mutable.(Var.idx z)
                     || Var.Tbl.get st a.(n)
                 | Phi _ | Expr _ -> true)
               (Var.Tbl.get known_origins y))

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

let get_approx { info_defs = _; info_known_origins; info_maybe_unknown; _ } f top join x =
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
          | Expr (Constant (String _) as e) when Config.Flag.safe_string () -> Some e
          | Expr e -> if info.info_possibly_mutable.(Var.idx x) then None else Some e
          | _ -> None)
        None
        (fun _ _ -> None)
        x
  | Pc c -> Some (Constant c)

let the_const_of info x =
  match x with
  | Pv x ->
      get_approx
        info
        (fun x ->
          match info.info_defs.(Var.idx x) with
          | Expr (Constant ((Float _ | Int _ | NativeString _) as c)) -> Some c
          | Expr (Constant (String _ as c)) when Config.Flag.safe_string () -> Some c
          | Expr (Constant c) ->
              if info.info_possibly_mutable.(Var.idx x) then None else Some c
          | _ -> None)
        None
        (fun u v ->
          match u, v with
          | Some i, Some j when Poly.(Code.constant_equal i j = Some true) -> u
          | _ -> None)
        x
  | Pc c -> Some c

let the_int info x =
  match the_const_of info x with
  | Some (Int i) -> Some i
  | _ -> None

let the_string_of info x =
  match the_const_of info x with
  | Some (String i) -> Some i
  | _ -> None

let the_native_string_of info x =
  match the_const_of info x with
  | Some (NativeString i) -> Some i
  | _ -> None

(****)

let f ?pessimistic p =
  Code.invariant p;
  let t = Timer.make () in
  let t1 = Timer.make () in
  let vars, deps, defs = program_deps p in
  let rets = return_values p in
  if times () then Format.eprintf "    flow analysis 1: %a@." Timer.print t1;
  let t2 = Timer.make () in
  let known_origins = solver1 vars deps rets defs in
  if times () then Format.eprintf "    flow analysis 2: %a@." Timer.print t2;
  let t3 = Timer.make () in
  let possibly_mutable = program_escape ?pessimistic defs known_origins p in
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
            (fun f l ->
              let rec list pr f l =
                match l with
                | [] -> ()
                | [ x ] -> pr f x
                | x :: r -> Format.fprintf f "%a, %a" pr x (list pr) r
              in
              list
                (fun f x ->
                  Var.print f x;
                  Format.fprintf
                    f
                    "%s"
                    (match defs.(Var.idx x) with
                    | Expr (Closure _) -> "C"
                    | Expr (Block _) -> "B"
                    | Expr (Constant (Int _)) -> "I"
                    | _ -> "?"))
                f
                l)
            (Var.Set.elements s)
            (if Var.Tbl.get maybe_unknown x then "any" else "known"))
      vars;
  let t5 = Timer.make () in
  let info =
    { info_defs = defs
    ; info_known_origins = known_origins
    ; info_maybe_unknown = maybe_unknown
    ; info_possibly_mutable = possibly_mutable
    }
  in
  if times () then Format.eprintf "    flow analysis 5: %a@." Timer.print t5;
  if times () then Format.eprintf "  flow analysis: %a@." Timer.print t;
  Code.invariant p;
  p, info
