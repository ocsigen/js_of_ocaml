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

val f :
     name:Code.Var.t
  -> locals:(Wasm_ast.var * Wasm_ast.value_type) list
  -> Wasm_ast.instruction list
  -> (Wasm_ast.var * Wasm_ast.value_type) list
     * Wasm_ast.instruction list
     * Wasm_ast.module_field list
(** Split a large function into smaller ones.

    [f ~name ~locals body] outlines large runs of instructions of the
    body [body] of a parameterless function into separate functions.
    It returns the locals and body of the residual function, and the
    new functions. Neither the residual body nor the new functions have
    been post-processed (see [Initialize_locals]). *)
