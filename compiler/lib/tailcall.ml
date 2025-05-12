1
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

let rewrite_block update_count (f, f_params, f_pc, used) pc blocks =
  let block = Addr.Map.find pc blocks in
  match block.branch with
  | Return x -> (
      match tail_call x f block.body with
      | Some f_args ->
          if List.compare_lengths f_params f_args = 0
          then (
            incr update_count;
            List.iter2 f_params f_args ~f:(fun p a -> Code.Var.propagate_name p a);
            used := true;
            Some
              (Addr.Map.add
                 pc
                 { params = block.params
                 ; body = remove_last block.body
                 ; branch = Branch (f_pc, f_args)
                 }
                 blocks))
          else None
      | None -> None)
  | _ -> None

let rec traverse update_count f pc visited blocks =
  if not (Addr.Set.mem pc visited)
  then
    let visited = Addr.Set.add pc visited in
    match rewrite_block update_count f pc blocks with
    | Some blocks ->
        (* The block was rewritten with a branch to the top of the function.
           No need to visit children. *)
        visited, blocks
    | None ->
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
  let previous_p = p in
  Code.invariant p;
  let free_pc = ref p.free_pc in
  let blocks = ref p.blocks in
  let update_count = ref 0 in
  let t = Timer.make () in
  Addr.Map.iter
    (fun pc _ ->
      let block = Addr.Map.find pc !blocks in
      let rewrite_body = ref false in
      let body =
        List.map block.body ~f:(function
          | Let (f, Closure (params, (pc_head, args), cloc)) as i ->
              if List.equal ~eq:Code.Var.equal params args
              then (
                blocks :=
                  snd
                    (traverse
                       update_count
                       (f, params, pc_head, ref false)
                       pc_head
                       Addr.Set.empty
                       !blocks);
                i)
              else
                let intermediate_pc = !free_pc in
                let need_to_create_intermediate_block = ref false in
                blocks :=
                  snd
                    (traverse
                       update_count
                       (f, params, intermediate_pc, need_to_create_intermediate_block)
                       pc_head
                       Addr.Set.empty
                       !blocks);
                if !need_to_create_intermediate_block
                then (
                  incr free_pc;
                  let new_params = List.map params ~f:Code.Var.fork in
                  let body =
                    (* duplicate the debug event before the loop header. *)
                    match (Addr.Map.find pc_head !blocks).body with
                    | (Event _ as e) :: _ -> [ e ]
                    | _ -> []
                  in
                  blocks :=
                    Addr.Map.add
                      intermediate_pc
                      { params; body; branch = Branch (pc_head, args) }
                      !blocks;
                  rewrite_body := true;
                  Let (f, Closure (new_params, (intermediate_pc, new_params), cloc)))
                else i
          | i -> i)
      in
      if !rewrite_body then blocks := Addr.Map.add pc { block with body } !blocks)
    p.blocks;
  let p = { p with blocks = !blocks; free_pc = !free_pc } in
  if times () then Format.eprintf "  tail calls: %a@." Timer.print t;
  if stats () then Format.eprintf "Stats - tail calls: %d optimizations@." !update_count;
  if debug_stats ()
  then Code.check_updates ~name:"tailcall" previous_p p ~updates:!update_count;
  Code.invariant p;
  p
