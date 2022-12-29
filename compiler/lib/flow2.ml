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
  | Phi of Var.Set.t
  | Unknown
  | Expr of Code.expr

type state =
  { vars : Var.ISet.t
  ; deps : Var.Set.t array
  ; defs : def array
  ; may_escape : bool array
  ; possibly_mutable : bool array
  ; return_values : Var.Set.t Var.Map.t
  }

let undefined = Phi Var.Set.empty

let is_undefined d =
  match d with
  | Phi s -> Var.Set.is_empty s
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
  | Phi s -> st.defs.(idx) <- Phi (Var.Set.add y s)
  | Unknown -> ()

let add_param_def st x =
  add_var st.vars x;
  let idx = Var.idx x in
  assert (is_undefined st.defs.(idx))

(* x depends on y *)
let add_dep st x y =
  let idx = Var.idx y in
  st.deps.(idx) <- Var.Set.add x st.deps.(idx)

let rec arg_deps st params args =
  match params, args with
  | x :: params, y :: args ->
      add_dep st x y;
      add_assign_def st x y;
      arg_deps st params args
  | _ -> ()

let cont_deps blocks st (pc, args) =
  let block = Addr.Map.find pc blocks in
  arg_deps st block.params args

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
          if st.may_escape.(Var.idx f)
          then
            List.iter
              ~f:(fun x ->
                let idx = Var.idx x in
                st.defs.(idx) <- Unknown)
              params
          else
            List.iter2
              ~f:(fun x y ->
                add_dep st x y;
                let idx = Var.idx x in
                match st.defs.(idx) with
                | Expr _ -> assert false
                | Phi s -> st.defs.(idx) <- Phi (Var.Set.add y s)
                | Unknown -> ())
              params
              args;
          Var.Set.iter (fun y -> add_dep st x y) (Var.Map.find f st.return_values)
      | _ -> ())
  | Closure (l, cont) ->
      List.iter l ~f:(fun x -> add_param_def st x);
      cont_deps blocks st cont
  | Block (_, a, _) -> Array.iter a ~f:(fun y -> add_dep st x y)
  | Field (y, _) -> add_dep st x y

let escape st x =
  let idx = Var.idx x in
  st.may_escape.(idx) <- true;
  st.possibly_mutable.(idx) <- true

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
              st.possibly_mutable.(Var.idx x) <- true;
              escape st y
          | Offset_ref _ -> ());
      match block.branch with
      | Return _ | Stop -> ()
      | Raise (x, _) -> escape st x
      | Branch cont | Poptrap cont -> cont_deps blocks st cont
      | Cond (_, cont1, cont2) ->
          cont_deps blocks st cont1;
          cont_deps blocks st cont2
      | Switch (_, a1, a2) ->
          Array.iter a1 ~f:(fun cont -> cont_deps blocks st cont);
          Array.iter a2 ~f:(fun cont -> cont_deps blocks st cont)
      | Pushtrap (cont, x, cont_h, _) ->
          add_var st.vars x;
          let idx = Var.idx x in
          st.defs.(idx) <- Unknown;
          cont_deps blocks st cont_h;
          cont_deps blocks st cont)
    blocks

module D = struct
  type approx =
    | Top
    | Values of Var.Set.t

  let rec var_escape ~update ~st ~st' x =
    let idx = Var.idx x in
    if not st.may_escape.(idx)
    then (
      st.may_escape.(idx) <- true;
      st.possibly_mutable.(idx) <- true;
      match st.defs.(idx) with
      | Expr (Block (_, a, _)) ->
          Array.iter ~f:(fun y -> escape ~update ~st ~st' (Var.Tbl.get st' y)) a;
          update ~deps:true x
      | Expr (Closure (params, _)) ->
          List.iter
            ~f:(fun y ->
              st.defs.(Var.idx y) <- Unknown;
              update ~deps:false y)
            params;
          Var.Set.iter
            (fun y ->
              let idx = Var.idx y in
              st.may_escape.(idx) <- true;
              st.possibly_mutable.(idx) <- true;
              escape ~update ~st ~st' (Var.Tbl.get st' y))
            (Var.Map.find x st.return_values)
      | _ -> ())

  and escape ~update ~st ~st' a =
    match a with
    | Top -> ()
    | Values s -> Var.Set.iter (fun x -> var_escape ~update ~st ~st' x) s

  let join ~update ~st ~st' x y =
    match x, y with
    | Top, _ ->
        escape ~update ~st ~st' y;
        Top
    | _, Top ->
        escape ~update ~st ~st' x;
        Top
    | Values s, Values s' -> Values (Var.Set.union s s')

  let bottom = Values Var.Set.empty

  let join_set ~update ~st ~st' f s =
    Var.Set.fold (fun x a -> join ~update ~st ~st' (f x) a) s bottom

  let inject x e =
    match e with
    | Constant _ -> bottom
    | Prim ((Vectlength | Not | IsInt | Eq | Neq | Lt | Le | Ult), _) -> bottom
    | Prim (Extern _, _) -> Top
    | Closure _ | Block _ | Prim (Array_get, _) -> Values (Var.Set.singleton x)
    | Field _ | Apply _ -> assert false
end

let mark_mutable st a =
  match a with
  | D.Top -> ()
  | Values s -> Var.Set.iter (fun y -> st.possibly_mutable.(Var.idx y) <- true) s

let propagate1 st update st' x =
  match st.defs.(Var.idx x) with
  | Phi s ->
      if st.may_escape.(Var.idx x) then D.escape ~update ~st ~st' (Var.Tbl.get st' x);
      if st.possibly_mutable.(Var.idx x) then mark_mutable st (Var.Tbl.get st' x);
      D.join_set ~update ~st ~st' (fun y -> Var.Tbl.get st' y) s
  | Expr e -> (
      match e with
      | Constant _ -> D.bottom
      | Closure _ -> Values (Var.Set.singleton x)
      | Block (_, a, _) ->
          if st.possibly_mutable.(Var.idx x)
          then Array.iter ~f:(fun y -> D.escape ~update ~st ~st' (Var.Tbl.get st' y)) a;
          Values (Var.Set.singleton x)
      | Field (y, n) -> (
          match Var.Tbl.get st' y with
          | Values s ->
              D.join_set
                ~update
                ~st
                ~st'
                (fun z ->
                  match st.defs.(Var.idx z) with
                  | Expr (Block (_, a, _)) when n < Array.length a ->
                      if st.possibly_mutable.(Var.idx z)
                      then Top
                      else
                        let t = a.(n) in
                        add_dep st x t;
                        Var.Tbl.get st' t
                  | Phi _ | Expr _ -> D.bottom
                  | Unknown -> Top)
                s
          | Top -> Top)
      | Prim (Array_get, [ Pv y; _; _ ]) -> (
          match Var.Tbl.get st' y with
          | Values s ->
              D.join_set
                ~update
                ~st
                ~st'
                (fun z ->
                  match st.defs.(Var.idx z) with
                  | Expr (Block (_, a, _)) ->
                      if st.possibly_mutable.(Var.idx z)
                      then Top
                      else (
                        Array.iter ~f:(fun t -> add_dep st x t) a;
                        Array.fold_left
                          ~f:(fun acc t ->
                            D.join ~update ~st ~st' (Var.Tbl.get st' t) acc)
                          ~init:D.bottom
                          a)
                  | Phi _ | Expr _ -> D.bottom
                  | Unknown -> Top)
                s
          | Top -> Top)
      | Prim (_, l) ->
          (*ZZZ Refine *)
          List.iter
            ~f:(fun a ->
              match a with
              | Pc _ -> ()
              | Pv y -> D.escape ~update ~st ~st' (Var.Tbl.get st' y))
            l;
          D.inject x e
      | Apply { f; args; _ } -> (
          match Var.Tbl.get st' f with
          | Values s ->
              D.join_set
                ~update
                ~st
                ~st'
                (fun g ->
                  match st.defs.(Var.idx g) with
                  | Expr (Closure (params, _)) when List.length args = List.length params
                    ->
                      if not (Hashtbl.mem h (x, g))
                      then (
                        Hashtbl.add h (x, g) ();
                        if st.may_escape.(Var.idx g)
                        then
                          List.iter
                            ~f:(fun x ->
                              let idx = Var.idx x in
                              st.defs.(idx) <- Unknown;
                              update ~deps:false x)
                            params
                        else
                          List.iter2
                            ~f:(fun x y ->
                              add_dep st x y;
                              let idx = Var.idx x in
                              match st.defs.(idx) with
                              | Expr _ -> assert false
                              | Phi s ->
                                  update ~deps:false x;
                                  st.defs.(idx) <- Phi (Var.Set.add y s)
                              | Unknown -> ())
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
                  | Phi _ | Expr _ -> D.bottom
                  | Unknown -> Top)
                s
          | Top -> D.Top))
  | Unknown -> Top

module G = Dgraph.Make_Imperative (Var) (Var.ISet) (Var.Tbl)

module Domain1 = struct
  type t = D.approx

  let equal x y =
    match x, y with
    | D.Top, D.Top -> true
    | Values s, Values s' -> Var.Set.equal s s'
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

let f ?pessimistic:_ p =
  Code.invariant p;
  (*  let t = Timer.make () in*)
  Format.eprintf "vvvvv";
  let t1 = Timer.make () in
  let rets = return_values p in
  let nv = Var.count () in
  let vars = Var.ISet.empty () in
  let deps = Array.make nv Var.Set.empty in
  let defs = Array.make nv undefined in
  let may_escape = Array.make nv false in
  let possibly_mutable = Array.make nv false in
  let st = { vars; deps; defs; return_values = rets; may_escape; possibly_mutable } in
  program_deps st p;
  if times () then Format.eprintf "    flow analysis 1: %a@." Timer.print t1;
  let t2 = Timer.make () in
  let known_origins = solver1 st in
  if times () then Format.eprintf "    flow analysis 2: %a@." Timer.print t2;
  if debug ()
  then
    Var.ISet.iter
      (fun x ->
        let s = Var.Tbl.get known_origins x in
        if not (Poly.( = ) s D.bottom) (*&& Var.Set.choose s <> x*)
        then
          Format.eprintf
            "%a: %a@."
            Var.print
            x
            (fun f a ->
              match a with
              | D.Top -> Format.fprintf f "top"
              | Values s -> Format.fprintf f "{%a}" Print.var_list (Var.Set.elements s))
            s)
      vars;
  ()
