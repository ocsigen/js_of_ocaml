(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2026 Hugo Heuzard
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

(* Use the value of a variable known from a test on it:
   - in the false branch of [Cond (x, _, _)], [x] is [0] (the OCaml
     values tested by a conditional are only falsy when equal to 0);
   - in branch [i] of [Switch (x, _)], [x] is [i].

   We replace [x] by a constant in the arguments of the branch, and in
   the target block if this is its only predecessor. For instance,
   [if (!b) return b] becomes [if (!b) return 0], and [b] may then be
   inlined into the condition. *)

open! Stdlib
open Code

let debug = Debug.find "cond-refine"

let f p =
  let t = Timer.make () in
  let preds = Addr.Hashtbl.create 16 in
  let add_pred pc' =
    Addr.Hashtbl.replace
      preds
      pc'
      (1 + Option.value ~default:0 (Addr.Hashtbl.find_opt preds pc'))
  in
  (* Entry points of the program and of closures count as predecessors *)
  add_pred p.start;
  let assigned = ref Var.Set.empty in
  Addr.Map.iter
    (fun pc block ->
      List.iter block.body ~f:(function
        | Assign (x, _) -> assigned := Var.Set.add x !assigned
        | Let (_, Closure (_, (pc', _), _)) -> add_pred pc'
        | _ -> ());
      Code.fold_children p.blocks pc (fun pc' () -> add_pred pc') ())
    p.blocks;
  let single_pred pc = Option.value ~default:0 (Addr.Hashtbl.find_opt preds pc) = 1 in
  let count = ref 0 in
  (* Blocks to rewrite: target pc -> (tested variable, its value) *)
  let targets = ref [] in
  let blocks =
    Addr.Map.mapi
      (fun block_pc block ->
        let refine x (pc, args) n =
          if Var.Set.mem x !assigned
          then [], (pc, args)
          else (
            if single_pred pc && pc <> block_pc then targets := (pc, (x, n)) :: !targets;
            if List.mem ~eq:Var.equal x args
            then (
              incr count;
              let z = Var.fresh () in
              let args = List.map args ~f:(fun y -> if Var.equal x y then z else y) in
              [ Let (z, Constant (Int (Targetint.of_int_exn n))) ], (pc, args))
            else [], (pc, args))
        in
        match block.branch with
        | Cond (x, ((pc1, _) as cont1), ((pc2, _) as cont2)) when pc1 <> pc2 ->
            let instrs, cont2 = refine x cont2 0 in
            { block with body = block.body @ instrs; branch = Cond (x, cont1, cont2) }
        | Switch (x, a) ->
            (* A block reached from several cases cannot be refined *)
            let a' =
              Array.mapi a ~f:(fun i ((pc, _) as cont) ->
                  if
                    Array.fold_left a ~init:0 ~f:(fun n (pc', _) ->
                        if pc' = pc then n + 1 else n)
                    = 1
                  then refine x cont i
                  else [], cont)
            in
            let instrs = Array.fold_right a' ~init:[] ~f:(fun (i, _) acc -> i @ acc) in
            { block with
              body = block.body @ instrs
            ; branch = Switch (x, Array.map a' ~f:snd)
            }
        | Cond _ | Return _ | Raise _ | Stop | Branch _ | Pushtrap _ | Poptrap _ -> block)
      p.blocks
  in
  let blocks =
    List.fold_left !targets ~init:blocks ~f:(fun blocks (pc, (x, n)) ->
        let block = Addr.Map.find pc blocks in
        let uses = ref false in
        Freevars.iter_block_free_vars (fun y -> if Var.equal x y then uses := true) block;
        if not !uses
        then blocks
        else (
          incr count;
          let z = Var.fresh () in
          let block =
            Subst.Excluding_Binders.block (fun y -> if Var.equal x y then z else y) block
          in
          Addr.Map.add
            pc
            { block with
              body = Let (z, Constant (Int (Targetint.of_int_exn n))) :: block.body
            }
            blocks))
  in
  if debug () then Format.eprintf "  cond refine: %d, %a@." !count Timer.print t;
  let p = { p with blocks } in
  Code.invariant p;
  p
