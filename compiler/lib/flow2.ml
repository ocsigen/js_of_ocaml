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
ZZZ Raise ==> escape
ZZZ Return escape if function escape
Set_field, ...
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
  | Expr of
      { e : Code.expr
      ; mutable escape : bool
      }

let undefined = Phi Var.Set.empty

let is_undefined d =
  match d with
  | Phi s -> Var.Set.is_empty s
  | _ -> false

let add_expr_def defs x e =
  let idx = Var.idx x in
  assert (is_undefined defs.(idx));
  defs.(idx) <- Expr { e; escape = false }

let add_assign_def vars defs x y =
  add_var vars x;
  let idx = Var.idx x in
  match defs.(idx) with
  | Expr _ -> assert false
  | Phi s -> defs.(idx) <- Phi (Var.Set.add y s)
  | Unknown -> ()

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

let h = Hashtbl.create 16

let expr_deps blocks vars deps defs rets x e =
  match e with
  | Constant _ -> ()
  | Prim (_, l) ->
      List.iter
        ~f:(fun a ->
          match a with
          | Pc _ -> ()
          | Pv y -> add_dep deps x y)
        l
  | Apply { f; args; _ } -> (
      add_dep deps x f;
      match defs.(Var.idx f) with
      | Expr { e = Closure (params, _); escape }
        when List.length args = List.length params ->
          Hashtbl.add h (x, f) ();
          List.iter2
            ~f:(fun x y ->
              add_dep deps x y;
              let idx = Var.idx x in
              match defs.(idx) with
              | Expr _ -> assert false
              | Phi s -> defs.(idx) <- (if escape then Unknown else Phi (Var.Set.add y s))
              | Unknown -> ())
            params
            args;
          Var.Set.iter (fun y -> add_dep deps x y) (Var.Map.find f rets)
      | _ -> ())
  | Closure (l, cont) ->
      List.iter l ~f:(fun x -> add_param_def vars defs x);
      cont_deps blocks vars deps defs cont
  | Block (_, a, _) -> Array.iter a ~f:(fun y -> add_dep deps x y)
  | Field (y, _) -> add_dep deps x y

let program_deps rets { blocks; _ } =
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
              expr_deps blocks vars deps defs rets x e
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
          add_var vars x;
          let idx = Var.idx x in
          defs.(idx) <- Unknown;
          cont_deps blocks vars deps defs cont_h;
          cont_deps blocks vars deps defs cont)
    blocks;
  vars, deps, defs

module D = struct
  type approx =
    | Top
    | Closures of Var.Set.t
    | Blocks of Var.Set.t
    | Other
    | Bottom

  let var_escape ~update ~defs x =
    match defs.(Var.idx x) with
    | Expr ({ e = Block _; escape } as d) ->
        if not escape
        then (
          d.escape <- true;
          (*ZZZ All fields in e escape *)
          update ~deps:true x)
    | Expr ({ e = Closure (params, _); escape } as d) ->
        if not escape
        then (
          d.escape <- true;
          List.iter
            ~f:(fun y ->
              defs.(Var.idx y) <- Unknown;
              update ~deps:false y)
            params)
    | _ -> ()

  let escape ~update ~defs a =
    match a with
    | Top | Other | Bottom -> ()
    | Closures s | Blocks s -> Var.Set.iter (fun x -> var_escape ~update ~defs x) s

  let join ~update ~defs x y =
    match x, y with
    | Top, _ ->
        escape ~update ~defs y;
        Top
    | _, Top ->
        escape ~update ~defs x;
        Top
    | Blocks _, Closures _ | Closures _, Blocks _ ->
        escape ~update ~defs x;
        escape ~update ~defs y;
        Top
    | Closures s, Closures s' -> Closures (Var.Set.union s s')
    | Blocks b, Blocks b' -> Blocks (Var.Set.union b b')
    | Bottom, _ -> y
    | _, Bottom -> x
    | Other, _ -> y
    | _, Other -> x

  let join_set ~update ~defs f s =
    Var.Set.fold (fun x a -> join ~update ~defs (f x) a) s Bottom

  let inject x e =
    match e with
    (*
    | Constant (Int _) -> Other
    | Constant _ | Prim _ -> Top
*)
    | Constant _ | Prim _ -> Bottom (*Closures (Var.Set.singleton x)*)
    | Closure _ -> Closures (Var.Set.singleton x)
    (*    | Block (n, _, _) when n <> 0 -> Top*)
    | Block _ -> Closures (Var.Set.singleton x)
    | _ -> assert false
end

let propagate1 deps rets defs update st x =
  match defs.(Var.idx x) with
  | Phi s -> D.join_set ~defs ~update (fun y -> Var.Tbl.get st y) s
  | Expr { e; _ } -> (
      match e with
      | Prim (_, l) ->
          List.iter
            ~f:(fun a ->
              match a with
              | Pc _ -> ()
              | Pv y -> D.var_escape ~defs ~update y)
            l;
          D.inject x e
      | Constant _ | Closure _ | Block _ -> D.inject x e
      | Field (y, n) -> (
          match Var.Tbl.get st y with
          | Closures s ->
              D.join_set
                ~defs
                ~update
                (fun z ->
                  match defs.(Var.idx z) with
                  | Expr { e = Block (_, a, _); escape } when n < Array.length a ->
                      if escape
                      then Top
                      else
                        let t = a.(n) in
                        add_dep deps x t;
                        Var.Tbl.get st t
                  | Phi _ | Expr _ -> Bottom
                  | Unknown -> Top)
                s
          | Top -> Top
          | _ -> Bottom)
      | Apply { f; args; _ } -> (
          match Var.Tbl.get st f with
          | Closures s ->
              D.join_set
                ~defs
                ~update
                (fun g ->
                  match defs.(Var.idx g) with
                  | Expr { e = Closure (params, _); escape }
                    when List.length args = List.length params ->
                      (*
                  Format.eprintf "ZZZ %d => %d@." (Var.idx x) (Var.idx g);*)
                      (* ZZZ Only if g has not yet been associated to x *)
                      if not (Hashtbl.mem h (x, g))
                      then (
                        Hashtbl.add h (x, g) ();
                        if escape
                        then
                          List.iter
                            ~f:(fun x ->
                              let idx = Var.idx x in
                              defs.(idx) <- Unknown;
                              update ~deps:false x)
                            params
                        else
                          List.iter2
                            ~f:(fun x y ->
                              add_dep deps x y;
                              let idx = Var.idx x in
                              match defs.(idx) with
                              | Expr _ -> assert false
                              | Phi s ->
                                  update ~deps:false x;
                                  defs.(idx) <- Phi (Var.Set.add y s)
                              | Unknown -> ())
                            params
                            args;
                        Var.Set.iter (fun y -> add_dep deps x y) (Var.Map.find g rets));
                      D.join_set
                        ~defs
                        ~update
                        (fun y -> Var.Tbl.get st y)
                        (Var.Map.find g rets)
                  | Phi _ | Expr _ -> D.Bottom
                  | Unknown -> Top)
                s
          | Top -> D.Top
          | _ -> Bottom))
  | Unknown -> Top

module G = Dgraph.Make_Imperative (Var) (Var.ISet) (Var.Tbl)

module Domain1 = struct
  type t = D.approx

  let equal x y =
    match x, y with
    | D.Top, D.Top | Bottom, Bottom | Other, Other -> true
    | Closures s, Closures s' -> Var.Set.equal s s'
    | Blocks s, Blocks s' -> Var.Set.equal s s'
    | _ -> false

  let bot = D.Bottom
end

module Solver1 = G.Solver (Domain1)

let solver1 vars deps rets defs =
  let g =
    { G.domain = vars; G.iter_children = (fun f x -> Var.Set.iter f deps.(Var.idx x)) }
  in
  Solver1.f' () g (propagate1 deps rets defs)

(****)

let f ?pessimistic:_ p =
  Code.invariant p;
  (*  let t = Timer.make () in*)
  Format.eprintf "vvvvv";
  let t1 = Timer.make () in
  let rets = return_values p in
  let vars, deps, defs = program_deps rets p in
  if times () then Format.eprintf "    flow analysis 1: %a@." Timer.print t1;
  let t2 = Timer.make () in
  let known_origins = solver1 vars deps rets defs in
  if times () then Format.eprintf "    flow analysis 2: %a@." Timer.print t2;
  (*
  let t3 = Timer.make () in
  let possibly_mutable = program_escape ?pessimistic defs known_origins p in
  if times () then Format.eprintf "    flow analysis 3: %a@." Timer.print t3;
  let t4 = Timer.make () in
  let maybe_unknown = solver2 vars deps defs known_origins possibly_mutable in
  if times () then Format.eprintf "    flow analysis 4: %a@." Timer.print t4;
*)
  if debug ()
  then
    Var.ISet.iter
      (fun x ->
        let s = Var.Tbl.get known_origins x in
        if not (Poly.( = ) s Bottom) (*&& Var.Set.choose s <> x*)
        then
          Format.eprintf
            "%a: %a@."
            Var.print
            x
            (fun f a ->
              match a with
              | D.Bottom -> Format.fprintf f "bot"
              | Top -> Format.fprintf f "top"
              | Closures s -> Format.fprintf f "C{%a}" Print.var_list (Var.Set.elements s)
              | Blocks s -> Format.fprintf f "B{%a}" Print.var_list (Var.Set.elements s)
              | Other -> Format.fprintf f "other")
            s)
      vars;
  ()
