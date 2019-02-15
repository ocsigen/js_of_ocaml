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

open Stdlib
open Code

let subst_cont s (pc, arg) = pc, List.map arg ~f:(fun x -> s x)

let expr s e =
  match e with
  | Const _ | Constant _ -> e
  | Apply (f, l, n) -> Apply (s f, List.map l ~f:(fun x -> s x), n)
  | Block (n, a) -> Block (n, Array.map a ~f:(fun x -> s x))
  | Field (x, n) -> Field (s x, n)
  | Closure (l, pc) -> Closure (l, subst_cont s pc)
  | Prim (p, l) ->
      Prim (p, List.map l ~f:(fun x -> match x with Pv x -> Pv (s x) | Pc _ -> x))

let instr s i =
  match i with
  | Let (x, e) -> Let (x, expr s e)
  | Set_field (x, n, y) -> Set_field (s x, n, s y)
  | Offset_ref (x, n) -> Offset_ref (s x, n)
  | Array_set (x, y, z) -> Array_set (s x, s y, s z)

let instrs s l = List.map l ~f:(fun i -> instr s i)

let last s l =
  match l with
  | Stop -> l
  | Branch cont -> Branch (subst_cont s cont)
  | Pushtrap (cont1, x, cont2, pcs) ->
      Pushtrap (subst_cont s cont1, x, subst_cont s cont2, pcs)
  | Return x -> Return (s x)
  | Raise (x, k) -> Raise (s x, k)
  | Cond (c, x, cont1, cont2) -> Cond (c, s x, subst_cont s cont1, subst_cont s cont2)
  | Switch (x, a1, a2) ->
      Switch
        ( s x
        , Array.map a1 ~f:(fun cont -> subst_cont s cont)
        , Array.map a2 ~f:(fun cont -> subst_cont s cont) )
  | Poptrap (cont, addr) -> Poptrap (subst_cont s cont, addr)

let block s block =
  { params = block.params
  ; handler = Option.map block.handler ~f:(fun (x, cont) -> x, subst_cont s cont)
  ; body = instrs s block.body
  ; branch = last s block.branch }

let program s (pc, blocks, free_pc) =
  let blocks = Addr.Map.map (fun b -> block s b) blocks in
  pc, blocks, free_pc

let rec cont' s pc blocks visited =
  if Addr.Set.mem pc visited
  then blocks, visited
  else
    let visited = Addr.Set.add pc visited in
    let b = Addr.Map.find pc blocks in
    let b = block s b in
    let blocks = Addr.Map.add pc b blocks in
    let blocks, visited =
      List.fold_left b.body ~init:(blocks, visited) ~f:(fun (blocks, visited) instr ->
          match instr with
          | Let (_, Closure (_, (pc, _))) -> cont' s pc blocks visited
          | _ -> blocks, visited )
    in
    Code.fold_children
      blocks
      pc
      (fun pc (blocks, visited) -> cont' s pc blocks visited)
      (blocks, visited)

let cont s addr (pc, blocks, free_pc) =
  let blocks, _ = cont' s addr blocks Addr.Set.empty in
  pc, blocks, free_pc

(****)

let from_array s x = match s.(Var.idx x) with Some y -> y | None -> x

(****)

let rec build_mapping params args =
  match params, args with
  | x :: params, y :: args -> Var.Map.add x y (build_mapping params args)
  | [], [] -> Var.Map.empty
  | _ -> assert false

let from_map m x = try Var.Map.find x m with Not_found -> x
