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

(** Textual specialization of the [(@if ...)] annotations of a WAT file.

    Wax only accepts [(@if ...)] around module fields and whole
    instructions, and requires condition variables to be valid Wax
    identifiers. Runtime files written for the former wasm_of_ocaml
    preprocessor (still used by the OxCaml fork, and so by Jane Street's
    libraries) use them anywhere, for instance around an operand, and test
    variables such as [$portable-int]. This pass replaces each [(@if ...)]
    form whose condition can be evaluated by the body of the selected
    branch, wherever it occurs, before the file is handed to Wax.

    A variable that is not defined is looked up again with its hyphens
    replaced by underscores. A form whose condition cannot be evaluated
    (unknown variable, ill-typed or malformed condition) is left unchanged,
    for Wax to specialize or report. The text that is removed is replaced
    by blanks with the same line breaks, so that positions in the result
    match the original file. *)

type value =
  | Bool of bool
  | String of string
  | Version of int * int * int

val specialize : variables:(string * value) list -> string -> string
