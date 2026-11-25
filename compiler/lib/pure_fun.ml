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

let pure pure_funs x = Var.Set.mem x pure_funs

let empty = Var.Set.empty

let pure_expr pure_funs e =
  match e with
  | Block _ | Field _ | Closure _ | Constant _ -> true
  | Special (Alias_prim _) -> true
  | Apply { f; exact; _ } ->
      exact && (Var.Set.mem f pure_funs || Shape.State.is_pure_fun f)
  | Prim (p, _l) -> (
      match p with
      | Extern (f, _) -> Primitive.is_pure f
      | _ -> true)

let pure_instr pure_funs i =
  match i with
  | Let (_, e) -> pure_expr pure_funs e
  | Event _ | Assign _ -> true
  | Set_field _ | Offset_ref _ | Array_set _ -> false

(****)

let rec traverse blocks pc visited pure_blocks funs =
  let rec loop stack =
    match stack with
    | [] -> ()
    | (pc, pure, []) :: rem ->
        if pure then BitSet.set pure_blocks pc;
        loop rem
    | (pc, pure, pc' :: children) :: rem ->
        if BitSet.mem visited pc'
        then
          (* Either [pc'] was already finished, or this is a back-edge (cycle in the CFG):
             in the latter case [pure_blocks] is unset so we conservatively treat it as
             impure. We cannot determine whether the loop terminates, so we treat it as
             potentially non-terminating. This is consistent with how we handle recursive
             function calls in [pure_expr], where a call to a function not yet known to be
             pure is considered impure. Since dead code elimination relies on purity to
             remove unused calls, incorrectly marking a non-terminating loop as pure could
             change program behavior. *)
          loop ((pc, pure && BitSet.mem pure_blocks pc', children) :: rem)
        else enter pc' ((pc, pure, pc' :: children) :: rem)
  and enter pc stack =
    BitSet.set visited pc;
    let pure = block blocks pc visited pure_blocks funs in
    (* Reverse the child list so blocks are visited in the same order as a
       recursive traversal. Child lists are small (typically 1-2 elements), so
       the extra cons cells are negligible compared to the per-block work. *)
    let children = List.rev (fold_children blocks pc (fun pc l -> pc :: l) []) in
    loop ((pc, pure, children) :: stack)
  in
  if not (BitSet.mem visited pc) then enter pc [];
  BitSet.mem pure_blocks pc

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
