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

(** Reorder a function's [locals] list so frequently-used locals get
    lower Wasm indices (one-byte LEB128 encoding for indices < 128).

    Parameters are untouched; only the non-parameter [locals] list is
    reordered. The instruction body is unchanged — the Wasm local
    index of a variable is determined by its position in
    [param_names @ locals], so rearranging the list is enough.

    Reference-type locals and numeric-type locals are kept in separate
    blocks so that the run-length encoding performed by
    [Wasm_output.coalesce_locals] doesn't get fragmented. *)

val f :
     locals:(Wasm_ast.var * Wasm_ast.value_type) list
  -> Wasm_ast.instruction list
  -> (Wasm_ast.var * Wasm_ast.value_type) list
