(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
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

let bound_variables p ~f ~params ~cont:(pc, _) =
  let bound_vars = ref Var.Map.empty in
  let add_var x = bound_vars := Var.Map.add x (Var.fork x) !bound_vars in
  List.iter ~f:add_var (f :: params);
  let rec traverse pc =
    Code.traverse
      { fold = fold_children }
      (fun pc _ ->
        let block = Code.block pc p in
        Freevars.iter_block_bound_vars add_var block;
        List.iter
          ~f:(fun i ->
            match i with
            | Let (_, Closure (params, (pc', _), _)) ->
                List.iter ~f:add_var params;
                traverse pc'
            | _ -> ())
          block.body)
      pc
      p
      ()
  in
  traverse pc;
  !bound_vars

let rec blocks_to_rename p pc lst =
  Code.traverse
    { fold = Code.fold_children }
    (fun pc lst ->
      let block = Code.block pc p in
      List.fold_left
        ~f:(fun lst i ->
          match i with
          | Let (_, Closure (_, (pc', _), _)) -> blocks_to_rename p pc' lst
          | _ -> lst)
        ~init:(pc :: lst)
        block.body)
    pc
    p
    lst

let closure p ~f ~params ~cont =
  let s = Subst.from_map (bound_variables p ~f ~params ~cont) in
  let pc, args = cont in
  let blocks = blocks_to_rename p pc [] in
  let _free_pc, m =
    List.fold_left
      ~f:(fun (pc', m) pc -> pc' + 1, Addr.Map.add pc pc' m)
      ~init:(Code.free_pc p, Addr.Map.empty)
      blocks
  in
  let p =
    List.fold_left
      ~f:(fun p pc ->
        let b = Subst.Including_Binders.And_Continuations.block m s (Code.block pc p) in
        Code.add_block (Addr.Map.find pc m) b p)
      ~init:p
      blocks
  in
  p, s f, List.map ~f:s params, (Addr.Map.find pc m, List.map ~f:s args)
