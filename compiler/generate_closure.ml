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

let f ((pc, blocks, free_pc) as p) : Code.program =
  let mutated_vars = Freevars.f p in
  let rewrite_list = ref [] in
  let blocks,free_pc =
    AddrMap.fold
      (fun pc block (blocks,free_pc) ->
         let updated = ref false in
         let body_rev, blocks, free_pc =
           List.fold_left
             (fun (body_rev, blocks, free_pc) i ->
                match i with
                  Let (x, Closure (params, (pc, pc_args ))) ->
                  let all_vars = AddrMap.find pc mutated_vars in
                  let vars = VarSet.elements (VarSet.remove x all_vars) in
                  if vars = []
                  then i::body_rev, blocks, free_pc
                  else begin
                    updated := true;
                    let new_pc = free_pc in
                    let free_pc = free_pc + 1 in
                    let closure = Code.Var.fresh () in
                    let args = List.map Code.Var.fork vars in
                    let mapping =
                      Jsoo_subst.from_map
                        (Jsoo_subst.build_mapping vars args)
                    in
                    rewrite_list := (mapping, pc) :: !rewrite_list;
                    let body_rev =
                      Let (x, Apply (closure, vars, true))
                      :: Let (closure, Closure (args, (new_pc, [])))
                      :: body_rev
                    in
                    let new_block =
                      let x = Code.Var.fresh () in
                      { params = [];
                        handler = None;
                        body = [Let (x, Closure (params, (pc, List.map mapping pc_args)))];
                        branch = Return x }
                    in
                    let blocks = AddrMap.add new_pc new_block blocks in
                    body_rev, blocks, free_pc
                  end
                | _ -> i::body_rev, blocks, free_pc
             )
             ([], blocks, free_pc) block.body
         in
         if !updated
         then AddrMap.add pc { block with body = List.rev body_rev } blocks, free_pc
         else blocks, free_pc
      )
      blocks (blocks,free_pc)
  in
  List.fold_left (fun program (mapping,pc) ->
    Jsoo_subst.cont mapping pc program
  ) (pc, blocks, free_pc) !rewrite_list



