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

(*
FIX: is there a way to merge this with dead code elimination?
*)

open Code

(****)

(* Postorder traversal of the whole program. *)

let traverse blocks pc f accu =
  let rec traverse_rec visited pc accu =
    if AddrSet.mem pc visited then (visited, accu) else begin
      let visited = AddrSet.add pc visited in
      let block = AddrMap.find pc blocks in
      let (visited, accu) =
        List.fold_left
          (fun ((visited, accu) as p) i ->
             match i with
               Let (_, Closure (_, (pc, _))) ->
                 traverse_rec visited pc accu
             | _ ->
                 p)
          (visited, accu) block.body
      in
      let (visited, accu) =
        match block.branch with
          Return _ | Raise _ | Stop ->
            (visited, accu)
        | Branch (pc, _) | Poptrap (pc, _) ->
            traverse_rec visited pc accu
        | Cond (_, _, (pc1, _), (pc2, _)) ->
            let (visited, accu) = traverse_rec visited pc1 accu in
            traverse_rec visited pc2 accu
        | Switch (_, a1, a2) ->
            let (visited, accu) =
              Array.fold_left
                (fun (visited, accu) (pc, _) -> traverse_rec visited pc accu)
                (visited, accu) a1 in
            let (visited, accu) =
              Array.fold_left
                (fun (visited, accu) (pc, _) -> traverse_rec visited pc accu)
                (visited, accu) a2 in
            (visited, accu)
        | Pushtrap ((pc1, _), _, (pc2, _), _) ->
            let (visited, accu) = traverse_rec visited pc1 accu in
            traverse_rec visited pc2 accu
      in
      (visited, f pc accu)
    end
  in
  snd (traverse_rec AddrSet.empty pc accu)

(****)

let is_trivial instr last =
  instr = []
    &&
  begin match last with
    Return _ | Raise _ | Stop | Branch _       -> true
  | Cond _ | Poptrap _ | Switch _ | Pushtrap _ -> false
  end

let resolve_branch blocks (pc, args) =
  match AddrMap.find pc blocks with
    {params = []; body = []; branch = Branch (pc', args')} ->
      Some (pc', args')
  | _ ->
      None

let concat_blocks pc instr params handler args params' instr' last' =
  (* This is only valid because we know that the params only occur in
     the block *)
  let m = Subst.build_mapping params' args in
  let s = Subst.from_map m in
    { params = params;
      handler = handler;
      body = instr @ Subst.instrs s instr';
      branch = Subst.last s last' }

let rec block_simpl pc (preds, entries, blocks) =
Format.eprintf "VV %d@." pc;
(*
Format.eprintf "RRRRRRRRRRRRRRR %d@." (AddrSet.cardinal (AddrMap.find 12644 preds));
*)
  let block = AddrMap.find pc blocks in
    match block.branch with
        Return _ | Raise _ | Stop | Poptrap _ ->
          (preds, entries, blocks)
      | Branch (pc', args) ->
          let block' = AddrMap.find pc' blocks in
          if
false
(*XXX FIX!
            not (AddrSet.mem pc' entries)
              &&
            AddrSet.cardinal (AddrMap.find pc' preds) = 1
              &&
            block'.params = [] && block'.handler = block.handler
*)
          then begin
Format.eprintf "UU %d ==> %d@." pc pc';
            (preds,
             entries,
             AddrMap.add pc
               (concat_blocks pc block.body block.params block.handler args
                  block'.params block'.body block'.branch)
               (AddrMap.remove pc' blocks))
          end else if false(*XXX args = [] && is_trivial block'.body block'.branch *)then begin
            (AddrMap.add pc' (AddrSet.remove pc (AddrMap.find pc' preds))
               preds,
             entries,
             AddrMap.add
               pc (concat_blocks
                     pc block.body block.params block.handler args
                     block'.params block'.body block'.branch)
               blocks)
          end else
            (preds, entries, blocks)
      | Cond (c, x, cont1, cont2) ->
          if cont1 = cont2 then begin
            let blocks =
              AddrMap.add pc {block with branch = Branch cont1 } blocks in
            block_simpl pc (preds, entries, blocks)
          end else begin
            match resolve_branch blocks cont1 with
              Some cont1' ->
                let pc1 = fst cont1 in let pc1' = fst cont1' in
                let preds =
                  AddrMap.add pc1'
                    (AddrSet.add pc
                       (AddrSet.remove pc1 (AddrMap.find pc1' preds)))
                    preds
                in
                let blocks =
                  AddrMap.add pc
                    { block with branch = Cond (c, x, cont1', cont2) } blocks
                in
                block_simpl pc (preds, entries, blocks)
            | None ->
                match resolve_branch blocks cont2 with
                  Some cont2' ->
                    let pc2 = fst cont2 in let pc2' = fst cont2' in
                    let preds =
                      AddrMap.add pc2'
                        (AddrSet.add pc
                           (AddrSet.remove pc2 (AddrMap.find pc2' preds)))
                        preds
                    in
                    let blocks =
                      AddrMap.add pc
                        { block with branch = Cond (c, x, cont1, cont2') }
                        blocks
                    in
                    block_simpl pc (preds, entries, blocks)
                | None ->
                    (preds, entries, blocks)
          end
      | Switch (x, a1, a2) ->
          let a1 =
            Array.map
              (fun pc ->
                 match resolve_branch blocks pc with Some pc -> pc | None -> pc)
              a1 in
          let a2 =
            Array.map
              (fun pc ->
                 match resolve_branch blocks pc with Some pc -> pc | None -> pc)
              a2 in
            (preds, entries,
             AddrMap.add pc { block with branch = Switch (x, a1, a2) } blocks)
      | Pushtrap _ ->
          (preds, entries, blocks)

let simpl (pc, blocks, free_pc) =
  let preds = AddrMap.map (fun _ -> AddrSet.empty) blocks in
  let entries = AddrSet.empty in
  let add_pred pc (pc', _) preds =
Format.eprintf "%d ==> %d@." pc pc';
    AddrMap.add pc' (AddrSet.add pc (AddrMap.find pc' preds)) preds in
  let (preds, entries) =
    AddrMap.fold
      (fun pc block (preds, entries) ->
         let entries =
           List.fold_left
             (fun entries i ->
                match i with
                  Let (_, Closure (_, (pc, _))) ->
                    AddrSet.add pc entries
                | _ ->
                    entries)
             entries block.body
         in
         let preds =
           match block.branch with
             Return _ | Raise _ | Stop ->
               preds
           | Branch cont | Poptrap cont ->
               add_pred pc cont preds
           | Cond (_, _, cont1, cont2)
           | Pushtrap (cont1, _, cont2, _) ->
               add_pred pc cont1 (add_pred pc cont2 preds)
           | Switch (_, a1, a2) ->
               let preds =
                 Array.fold_left
                   (fun preds cont -> add_pred pc cont preds) preds a1 in
               let preds =
                 Array.fold_left
                   (fun preds cont -> add_pred pc cont preds) preds a2 in
               preds
         in
         (preds, entries))
      blocks (preds, entries)
  in
(*
Format.eprintf "RRRRRRRRRRRRRRR %d@." (AddrSet.cardinal (AddrMap.find 12644 preds));*)
  let (_, _, blocks) =
    traverse blocks pc block_simpl (preds, entries, blocks) in
  (pc, blocks, free_pc)
