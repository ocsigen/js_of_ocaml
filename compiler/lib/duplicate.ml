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

let closure p ~bound_vars ~f ~params ~cont:(pc, args) =
  let s =
    Subst.from_map
      (Var.Set.fold (fun x s -> Var.Map.add x (Var.fork x) s) bound_vars Var.Map.empty)
  in
  let free_pc, m =
    Code.traverse
      { fold = Code.fold_children }
      (fun pc (pc', m) -> pc' + 1, Addr.Map.add pc pc' m)
      pc
      p.blocks
      (p.free_pc, Addr.Map.empty)
  in
  let blocks =
    Code.traverse
      { fold = Code.fold_children }
      (fun pc blocks ->
        let b = Addr.Map.find pc blocks in
        let b = Subst.Including_Binders.And_Continuations.block m s b in
        Addr.Map.add (Addr.Map.find pc m) b blocks)
      pc
      p.blocks
      p.blocks
  in
  let p = { p with blocks; free_pc } in
  p, s f, List.map ~f:s params, (Addr.Map.find pc m, List.map ~f:s args)
