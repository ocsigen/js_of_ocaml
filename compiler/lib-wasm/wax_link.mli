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

type value =
  | Bool of bool
  | String of string
  | Version of int * int * int

val value_equal : value -> value -> bool

type source =
  | Binary  (** Binary file (assembled already, passed through) *)
  | File  (** Not read yet *)
  | Contents of string  (** File contents to assemble *)

type input =
  { module_name : string
        (** Name under which the module should be imported (used to fill the [Binary.link_input] record) *)
  ; file : string  (** File originally containing the module *)
  ; source : source
        (** Information about the file, including possibly the already read file contents *)
  }

val with_preprocessed_files :
     variables:(string * value) list
  -> inputs:input list
  -> (Binaryen.link_input list -> 'a)
  -> 'a
(** Preprocess and assemble runtime modules using the Wax toolchain.

    Each text input (WAT or Wax, by extension) has its conditional
    annotations specialized against [variables], its exported functions named
    (when [name-wasm-functions] is set), and is assembled to a WebAssembly
    binary temporary file. Binary inputs are passed through unchanged. The
    resulting files are handed to [action] as {!Binaryen.link_input}s, suitable
    for [wasm-merge] (which accepts both binary and text inputs). *)
