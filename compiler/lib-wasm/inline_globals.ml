(* Wasm_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2026
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

(* Each variable bound to a constant at toplevel becomes a global, so a
   nested constant block is built from several globals, most of them
   used only once, by the initializer of the enclosing block. Inlining
   their initializers there saves their definitions and the
   [global.get] instructions.

   Only the uses in global initializers are inlined. A global used in a
   function is kept: inlining it there would allocate a new value at
   each execution. *)

open! Stdlib
module W = Wasm_ast

let f fields =
  let init_uses = Code.Var.Hashtbl.create 1024 in
  let other_uses = Code.Var.Hashtbl.create 1024 in
  let count tbl x = Option.value ~default:0 (Code.Var.Hashtbl.find_opt tbl x) in
  let incr tbl x = Code.Var.Hashtbl.replace tbl x (count tbl x + 1) in
  let rec count_expression tbl e =
    (match e with
    | W.GlobalGet x -> incr tbl x
    | _ -> ());
    Wasm_traverse.iter_expression
      ~expression:(count_expression tbl)
      ~instructions:(count_instructions tbl)
      e
  and count_instructions tbl l = List.iter ~f:(count_instruction tbl) l
  and count_instruction tbl i =
    (match i with
    | W.GlobalSet (x, _) -> incr tbl x
    | _ -> ());
    Wasm_traverse.iter_instruction
      ~expression:(count_expression tbl)
      ~instructions:(count_instructions tbl)
      i
  in
  List.iter
    ~f:(fun field ->
      match field with
      | W.Function { body; _ } -> count_instructions other_uses body
      | Global { init; _ } -> count_expression init_uses init
      | Data _ | Tag _ | Import _ | Type _ -> ())
    fields;
  (* The initializers of the globals to inline, until their use is met *)
  let inlined = Code.Var.Hashtbl.create 1024 in
  let rec substitute e =
    match e with
    | W.GlobalGet x -> (
        match Code.Var.Hashtbl.find_opt inlined x with
        | Some e' ->
            Code.Var.Hashtbl.remove inlined x;
            e'
        | None -> e)
    | _ -> Wasm_traverse.map_expression ~expression:substitute ~instructions:Fun.id e
  in
  let fields =
    List.filter_map
      ~f:(fun field ->
        match field with
        | W.Global ({ name; exported_name; typ; init } as g) ->
            let init = substitute init in
            if
              (not typ.mut)
              && Option.is_none exported_name
              && count init_uses name = 1
              && count other_uses name = 0
            then (
              Code.Var.Hashtbl.add inlined name init;
              None)
            else Some (W.Global { g with init })
        | Function _ | Data _ | Tag _ | Import _ | Type _ -> Some field)
      fields
  in
  assert (Code.Var.Hashtbl.length inlined = 0);
  fields
