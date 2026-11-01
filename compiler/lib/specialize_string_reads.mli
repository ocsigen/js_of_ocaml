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

val enabled : unit -> bool
(** Whether [caml_bytes_of_string] copies the string: true when compiling to
    Wasm with JavaScript strings. *)

val string_primitive : string -> string option
(** The string counterpart of a primitive which only reads the byte array
    passed as its first argument (and does not let it escape). *)

val iter_instr_read_only_uses :
  read:(Code.Var.t -> unit) -> other:(Code.Var.t -> unit) -> Code.instr -> unit
(** Same as {!iter_read_only_uses}, for an instruction. *)

val iter_read_only_uses :
  read:(Code.Var.t -> unit) -> other:(Code.Var.t -> unit) -> Code.block -> unit
(** Iterate over the variables used in a block: [read] is applied to the byte
    arrays only read by a primitive with a string counterpart, [other] to
    the other uses. *)

val f : Code.program -> Code.program
(** When [caml_bytes_of_string] copies the string, perform the reads of a byte
    array [x = caml_bytes_of_string s] on [s] instead when [x] is only read
    through primitives with a string counterpart, and remove the copy. *)
