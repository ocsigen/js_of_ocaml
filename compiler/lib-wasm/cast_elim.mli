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

(** Removal of the casts which always succeed: [ref.cast (ref T) e] when
    the static type of [e] is already a subtype of [(ref T)]. *)

val f :
     context:Code_generation.context
  -> params:(Wasm_ast.var * Wasm_ast.value_type) list
  -> locals:(Wasm_ast.var * Wasm_ast.value_type) list
  -> Wasm_ast.instruction list
  -> Wasm_ast.instruction list
