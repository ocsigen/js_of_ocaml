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
open Code

(****)

let pure_expr pure_funs e =
  match e with
  | Block _ | Field _ | Closure _ | Constant _ -> true
  | Apply { f; exact; _ } -> exact && Var.Set.mem f pure_funs
  | Prim (p, _l) -> (
      match p with
      | Extern f -> Primitive.is_pure f
      | _ -> true)

let pure_instr pure_funs i =
  match i with
  | Let (_, e) -> pure_expr pure_funs e
  | Assign _ -> true
  | Set_field _ | Offset_ref _ | Array_set _ -> false

(****)

let rec traverse blocks pc visited funs =
  try Addr.Map.find pc visited, visited, funs
  with Not_found ->
    let visited = Addr.Map.add pc false visited in
    let pure, visited, funs =
      fold_children
        blocks
        pc
        (fun pc (pure, visited, funs) ->
          let pure', visited, funs = traverse blocks pc visited funs in
          pure && pure', visited, funs)
        (true, visited, funs)
    in
    let pure, visited, funs = block blocks pc pure visited funs in
    pure, Addr.Map.add pc pure visited, funs

and block blocks pc pure visited funs =
  let b = Addr.Map.find pc blocks in
  let pure =
    match fst b.branch with
    | Raise _ -> false
    | _ -> pure
  in
  List.fold_left
    b.body
    ~init:(pure, visited, funs)
    ~f:(fun (pure, visited, funs) (i, _loc) ->
      let visited, funs =
        match i with
        | Let (x, Closure (_, (pc, _))) ->
            let pure, visited, funs = traverse blocks pc visited funs in
            visited, if pure then Var.Set.add x funs else funs
        | _ -> visited, funs
      in
      pure && pure_instr funs i, visited, funs)

let f p =
  let _, _, funs = traverse p.blocks p.start Addr.Map.empty Var.Set.empty in
  funs
