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

open Code

(****)

let pure_expr pure_funs e =
  match e with
  | Block _ | Field _ | Closure _ | Constant _ -> true
  | Special (Alias_prim _) -> true
  | Apply { f; exact; _ } -> exact && Var.Set.mem f pure_funs
  | Prim (p, _l) -> (
      match p with
      | Extern f -> Primitive.is_pure f
      | _ -> true)

let pure_instr pure_funs i =
  match i with
  | Let (_, e) -> pure_expr pure_funs e
  | Event _ | Assign _ -> true
  | Set_field _ | Offset_ref _ | Array_set _ -> false

(****)

let rec traverse blocks pc visited pure_blocks funs =
  if BitSet.mem visited pc
  then BitSet.mem pure_blocks pc
  else (
    BitSet.set visited pc;
    let pure = block blocks pc visited pure_blocks funs in
    let pure =
      fold_children
        blocks
        pc
        (fun pc pure ->
          let pure' = traverse blocks pc visited pure_blocks funs in
          pure && pure')
        pure
    in
    if pure then BitSet.set pure_blocks pc;
    pure)

and block blocks pc visited pure_blocks funs =
  let b = Addr.Map.find pc blocks in
  let pure =
    match b.branch with
    | Raise _ -> false
    | _ -> true
  in
  List.fold_left b.body ~init:pure ~f:(fun pure i ->
      (match i with
      | Let (x, Closure (_, (pc, _), _)) ->
          let pure = traverse blocks pc visited pure_blocks funs in
          if pure then funs := Var.Set.add x !funs
      | _ -> ());
      pure && pure_instr !funs i)

type t = Var.Set.t

let f p =
  let t = Timer.make () in
  let visited = BitSet.create' p.free_pc in
  let pure = BitSet.create' p.free_pc in
  let funs = ref Var.Set.empty in
  let _ = traverse p.blocks p.start visited pure funs in
  if times () then Format.eprintf "    pure funs.: %a@." Timer.print t;
  if stats () then Format.eprintf "Stats - pure functions: %d@." (Var.Set.cardinal !funs);
  !funs
