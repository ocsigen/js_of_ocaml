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

open Code
module Primitive = Jsoo_primitive
(****)

let pure_expr pure_funs e =
  match e with
    Const _  | Block _ | Field _ | Closure _ | Constant _ ->
      true
  | Apply (f, _l, exact) ->
      exact && VarSet.mem f pure_funs
  | Prim (p, _l) ->
      match p with
        Extern f -> Primitive.is_pure f
      | _        -> true

let pure_instr pure_funs i =
  match i with
    Let (_, e) ->
      pure_expr pure_funs e
  | Set_field _ | Offset_ref _ | Array_set _ ->
      false

(****)

let rec traverse blocks pc visited funs =
  try
    (AddrMap.find pc visited, visited, funs)
  with Not_found ->
    let visited = AddrMap.add pc false visited in
    let (pure, visited, funs) =
      fold_children blocks pc
        (fun pc (pure, visited, funs) ->
           let (pure', visited, funs) = traverse blocks pc visited funs in
           (pure && pure', visited, funs))
        (true, visited, funs)
    in
    let (pure, visited, funs) = block blocks pc pure visited funs in
    (pure, AddrMap.add pc pure visited, funs)

and block blocks pc pure visited funs =
  let b = AddrMap.find pc blocks in
  let pure = match b.branch with Raise _ -> false | _ -> pure in
  List.fold_left
    (fun (pure, visited, funs) i ->
       let (visited, funs) =
         match i with
           Let (x, Closure (_, (pc, _))) ->
             let (pure, visited, funs) = traverse blocks pc visited funs in
             (visited, if pure then VarSet.add x funs else funs)
         | _ ->
             (visited, funs)
       in
       (pure && pure_instr funs i, visited, funs))
    (pure, visited, funs) b.body

let f (pc, blocks, _) =
  let (_, _, funs) = traverse blocks pc AddrMap.empty VarSet.empty in
  funs
