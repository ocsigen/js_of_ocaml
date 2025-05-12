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

let times = Debug.find "times"

let stats = Debug.find "stats"

let debug_stats = Debug.find "stats-debug"

open Code

(****)

let add_var = Var.ISet.add

let add_def vars defs x y =
  add_var vars x;
  let idx = Var.idx x in
  defs.(idx) <- Var.Set.add y defs.(idx)

let add_dep deps x y =
  let idx = Var.idx y in
  deps.(idx) <- Var.Set.add x deps.(idx)

let rec arg_deps vars deps defs params args =
  match params, args with
  | x :: params, y :: args ->
      add_dep deps x y;
      add_def vars defs x y;
      arg_deps vars deps defs params args
  | [], [] -> ()
  | _ -> assert false

let cont_deps blocks vars deps defs (pc, args) =
  let block = Addr.Map.find pc blocks in
  arg_deps vars deps defs block.params args

let expr_deps blocks vars deps defs x e =
  match e with
  | Constant _ | Apply _ | Prim _ | Special _ -> ()
  | Closure (_, cont, _) -> cont_deps blocks vars deps defs cont
  | Block (_, a, _, _) -> Array.iter a ~f:(fun y -> add_dep deps x y)
  | Field (y, _, _) -> add_dep deps x y

let program_deps { blocks; _ } =
  let nv = Var.count () in
  let vars = Var.ISet.empty () in
  let deps = Array.make nv Var.Set.empty in
  let defs = Array.make nv Var.Set.empty in
  Addr.Map.iter
    (fun _pc block ->
      List.iter block.body ~f:(fun i ->
          match i with
          | Let (x, e) ->
              add_var vars x;
              expr_deps blocks vars deps defs x e
          | Assign (x, y) ->
              add_dep deps x y;
              add_def vars defs x y
          | Event _ | Set_field _ | Array_set _ | Offset_ref _ -> ());
      match block.branch with
      | Return _ | Raise _ | Stop -> ()
      | Branch cont -> cont_deps blocks vars deps defs cont
      | Cond (_, cont1, cont2) ->
          cont_deps blocks vars deps defs cont1;
          cont_deps blocks vars deps defs cont2
      | Switch (_, a1) ->
          Array.iter a1 ~f:(fun cont -> cont_deps blocks vars deps defs cont)
      | Pushtrap (cont, _, cont_h) ->
          cont_deps blocks vars deps defs cont_h;
          cont_deps blocks vars deps defs cont
      | Poptrap cont -> cont_deps blocks vars deps defs cont)
    blocks;
  vars, deps, defs

let rec repr' reprs x acc =
  let idx = Var.idx x in
  match reprs.(idx) with
  | None -> x, acc
  | Some y -> repr' reprs y (x :: acc)

let repr reprs x =
  let last, l = repr' reprs x [] in
  List.iter l ~f:(fun v -> reprs.(Var.idx v) <- Some last);
  last

let replace deps reprs x y =
  let yidx = Var.idx y in
  let xidx = Var.idx x in
  deps.(yidx) <- Var.Set.union deps.(yidx) deps.(xidx);
  reprs.(xidx) <- Some y;
  true

let propagate1 deps defs reprs st x =
  let prev = Var.Tbl.get st x in
  if prev
  then prev
  else
    let idx = Var.idx x in
    let s =
      Var.Set.fold (fun x s -> Var.Set.add (repr reprs x) s) defs.(idx) Var.Set.empty
    in
    defs.(idx) <- s;
    match Var.Set.cardinal s with
    | 1 -> replace deps reprs x (Var.Set.choose s)
    | 2 -> (
        match Var.Set.elements s with
        | [ y; z ] when Var.compare x y = 0 -> replace deps reprs x z
        | [ z; y ] when Var.compare x y = 0 -> replace deps reprs x z
        | _ -> false)
    | _ -> false

module G = Dgraph.Make_Imperative (Var) (Var.ISet) (Var.Tbl)

module Domain1 = struct
  type t = bool

  let equal = Bool.equal

  let bot = false
end

module Solver1 = G.Solver (Domain1)

let solver1 vars deps defs =
  let nv = Var.count () in
  let reprs = Array.make nv None in
  let g =
    { G.domain = vars; G.iter_children = (fun f x -> Var.Set.iter f deps.(Var.idx x)) }
  in
  ignore (Solver1.f () g (propagate1 deps defs reprs));
  Array.mapi reprs ~f:(fun idx y ->
      match y with
      | Some y -> repr reprs y
      | None -> Var.of_idx idx)

let f p =
  let previous_p = p in
  Code.invariant p;
  let t = Timer.make () in
  let t' = Timer.make () in
  let vars, deps, defs = program_deps p in
  if times () then Format.eprintf "    phi-simpl. 1: %a@." Timer.print t';
  let t' = Timer.make () in
  let subst = solver1 vars deps defs in
  if times () then Format.eprintf "    phi-simpl. 2: %a@." Timer.print t';
  Array.iteri subst ~f:(fun idx y ->
      if Var.idx y = idx then () else Code.Var.propagate_name (Var.of_idx idx) y);
  let need_stats = stats () || debug_stats () in
  let count_uniq = ref 0 in
  let count_seen = BitSet.create' (if need_stats then Var.count () else 0) in
  let subst v1 =
    let idx1 = Code.Var.idx v1 in
    let v2 = subst.(idx1) in
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
  if times () then Format.eprintf "  phi-simpl.: %a@." Timer.print t;
  if stats () then Format.eprintf "Stats - phi updates: %d@." !count_uniq;
  if debug_stats () then Code.check_updates ~name:"phi" previous_p p ~updates:!count_uniq;
  Code.invariant p;
  p
