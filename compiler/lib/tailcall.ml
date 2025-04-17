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

(* FIX: it should be possible to deal with tail-recursion in exception
   handlers, but we have to adapt the code generator for that *)

let rec remove_last l =
  match l with
  | [] -> assert false
  | [ _ ] -> []
  | x :: r -> x :: remove_last r

let rec tail_call x f l =
  match l with
  | [] -> None
  | [ Let (y, Apply { f = g; args; _ }) ] when Var.compare x y = 0 && Var.compare f g = 0
    -> Some args
  | _ :: rem -> tail_call x f rem

let rewrite_block update_count (f, f_params, f_pc, args) pc blocks =
  let block = Addr.Map.find pc blocks in
  match block.branch with
  | Return x -> (
      match tail_call x f block.body with
      | Some f_args when List.length f_params = List.length f_args ->
          let m = Subst.build_mapping f_params f_args in
          List.iter2 f_params f_args ~f:(fun p a -> Code.Var.propagate_name p a);
          incr update_count;
          Addr.Map.add
            pc
            { params = block.params
            ; body = remove_last block.body
            ; branch = Branch (f_pc, List.map args ~f:(fun x -> Var.Map.find x m))
            }
            blocks
      | _ -> blocks)
  | _ -> blocks

let rec traverse update_count f pc visited blocks =
  if not (Addr.Set.mem pc visited)
  then
    let visited = Addr.Set.add pc visited in
    let blocks = rewrite_block update_count f pc blocks in
    let visited, blocks =
      Code.fold_children_skip_try_body
        blocks
        pc
        (fun pc (visited, blocks) ->
          let visited, blocks = traverse update_count f pc visited blocks in
          visited, blocks)
        (visited, blocks)
    in
    visited, blocks
  else visited, blocks

let f p =
  let update_count = ref 0 in
  let t = Timer.make () in
  let blocks =
    fold_closures
      p
      (fun f params (pc, args) _ blocks ->
        match f with
        | Some f when List.length params = List.length args ->
            let _, blocks =
              traverse update_count (f, params, pc, args) pc Addr.Set.empty blocks
            in
            blocks
        | _ -> blocks)
      p.blocks
  in
  if times () then Format.eprintf "  tail calls: %a@." Timer.print t;
  if stats () then Format.eprintf "Stats - tail calls: %d optimizations@." !update_count;
  { p with blocks }
