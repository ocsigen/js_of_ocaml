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

let times = Debug.find "times"

open Code

(* FIX: it should be possible to deal with tail-recursion in exception
   handlers, but we have to adapt the code generator for that *)

let rec remove_last l =
  match l with [] -> assert false | [_] -> [] | x :: r -> x :: remove_last r

let rec tail_call x f l =
  match l with
  | [] -> None
  | [Let (y, Apply (g, args, _))] when Var.compare x y = 0 && Var.compare f g = 0 ->
      Some args
  | _ :: rem -> tail_call x f rem

let rewrite_block (f, f_params, f_pc, args) pc blocks =
  (*Format.eprintf "%d@." pc;*)
  let block = Addr.Map.find pc blocks in
  match block.branch with
  | Return x -> (
    match tail_call x f block.body with
    | Some f_args when List.length f_params = List.length f_args ->
        let m = Subst.build_mapping f_params f_args in
        List.iter2 f_params f_args ~f:(fun p a -> Code.Var.propagate_name p a);
        Addr.Map.add
          pc
          { params = block.params
          ; handler = block.handler
          ; body = remove_last block.body
          ; branch = Branch (f_pc, List.map args ~f:(fun x -> Var.Map.find x m)) }
          blocks
    | _ -> blocks )
  | _ -> blocks

let ( >> ) x f = f x

(* Skip try body *)
let fold_children blocks pc f accu =
  let block = Addr.Map.find pc blocks in
  match block.branch with
  | Return _ | Raise _ | Stop -> accu
  | Branch (pc', _) | Poptrap ((pc', _), _) -> f pc' accu
  | Pushtrap (_, _, (pc1, _), pcs) -> f pc1 (Addr.Set.fold f pcs accu)
  | Cond (_, _, (pc1, _), (pc2, _)) -> accu >> f pc1 >> f pc2
  | Switch (_, a1, a2) ->
      let accu = Array.fold_right a1 ~init:accu ~f:(fun (pc, _) accu -> f pc accu) in
      let accu = Array.fold_right a2 ~init:accu ~f:(fun (pc, _) accu -> f pc accu) in
      accu

let rec traverse f pc visited blocks =
  if not (Addr.Set.mem pc visited)
  then
    let visited = Addr.Set.add pc visited in
    let blocks = rewrite_block f pc blocks in
    let visited, blocks =
      fold_children
        blocks
        pc
        (fun pc (visited, blocks) ->
          let visited, blocks = traverse f pc visited blocks in
          visited, blocks )
        (visited, blocks)
    in
    visited, blocks
  else visited, blocks

let f ((pc, blocks, free_pc) as p) =
  let t = Timer.make () in
  let blocks =
    fold_closures
      p
      (fun f params (pc, args) blocks ->
        match f with
        | Some f when List.length params = List.length args ->
            let _, blocks = traverse (f, params, pc, args) pc Addr.Set.empty blocks in
            blocks
        | _ -> blocks )
      blocks
  in
  if times () then Format.eprintf "  tail calls: %a@." Timer.print t;
  pc, blocks, free_pc
