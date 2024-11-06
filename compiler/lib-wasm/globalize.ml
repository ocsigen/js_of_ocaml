(* Wasm_of_ocaml compiler
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

(*

Store some toplevel values into globals. Any variable which is used a
number of instructions after being defined is stored into a global
instead of a local. The goals are the following:
- Turn a large number of closures into constant closures, which has a
  significant impact on performance
- Reduce the compilation time of the toplevel function in case the
  Wasm engine decide to optimize it: reduce the register pressure by
  avoiding long-lived registers in the toplevel function, and make
  load elimination less expensive by reducing the number of constant
  structures defined in this function.
*)

open Stdlib

type st =
  { pos : int
  ; visited_variables : int Code.Var.Map.t
  ; globals : Code.Var.Set.t
  ; closures : Closure_conversion.closure Code.Var.Map.t
  }

let threshold = 1000

let rec globalize st x =
  if Code.Var.Set.mem x st.globals
  then st
  else
    let st = { st with globals = Code.Var.Set.add x st.globals } in
    globalize_closure st x

and globalize_closure st x =
  (* If a function is stored in a global variable, its free variables
     are also stored in a global variable, since they are retained
     anyway. *)
  match Code.Var.Map.find x st.closures with
  | { free_variables; _ } ->
      List.fold_left
        ~f:(fun st x ->
          if Code.Var.Map.mem x st.visited_variables then globalize st x else st)
        ~init:st
        free_variables
  | exception Not_found -> st

let use x st =
  match Code.Var.Map.find x st.visited_variables with
  | pos -> if st.pos > pos + threshold then globalize st x else st
  | exception Not_found -> st

let declare x st =
  { st with visited_variables = Code.Var.Map.add x st.pos st.visited_variables }

let traverse_expression x e st =
  match e with
  | Code.Apply { f; args; _ } ->
      st |> use f |> fun st -> List.fold_left ~f:(fun st x -> use x st) ~init:st args
  | Block (_, a, _, _) -> Array.fold_right ~f:use a ~init:st
  | Field (x, _, _) -> st |> use x
  | Closure _ ->
      List.fold_left
        ~f:(fun st x -> use x st)
        ~init:st
        (Code.Var.Map.find x st.closures).Closure_conversion.free_variables
  | Constant _ | Special _ -> st
  | Prim (_, args) ->
      List.fold_left
        ~f:(fun st a ->
          match a with
          | Code.Pv x -> st |> use x
          | Pc _ -> st)
        ~init:st
        args

let traverse_instruction st i =
  let st = { st with pos = st.pos + 1 } in
  match i with
  | Code.Let (x, e) -> st |> declare x |> traverse_expression x e
  | Assign (_, x) | Offset_ref (x, _) -> st |> use x
  | Set_field (x, _, _, y) -> st |> use x |> use y
  | Array_set (x, y, z) -> st |> use x |> use y |> use z
  | Event _ -> st

let traverse_block p st pc =
  let b = Code.Addr.Map.find pc p.Code.blocks in
  let st = List.fold_left ~f:(fun st x -> declare x st) ~init:st b.Code.params in
  List.fold_left ~f:(fun st i -> traverse_instruction st i) ~init:st b.Code.body

let f p g closures =
  let l = Structure.blocks_in_reverse_post_order g in
  let in_loop = Freevars.find_loops_in_closure p p.Code.start in
  let st =
    List.fold_left
      ~f:(fun st pc ->
        if Code.Addr.Map.mem pc in_loop then st else traverse_block p st pc)
      ~init:
        { pos = 0
        ; visited_variables = Code.Var.Map.empty
        ; globals = Code.Var.Set.empty
        ; closures
        }
      l
  in
  st.globals
