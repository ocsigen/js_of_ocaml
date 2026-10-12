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

val with_binary_files :
     options:string list
  -> Binaryen.link_input list
  -> (Binaryen.link_input list -> 'a)
  -> 'a
(** Convert the input files in text format to binary format with
    [wasm-opt], passing it [options], and call the continuation with the
    resulting files. The files are deleted afterwards. *)

val link : inputs:Binaryen.link_input list -> output_file:string -> unit
(** Link the input modules (in binary format), keeping all their code *)

val build :
     allowed_imports:string list option
  -> convert_options:string list
  -> opt_options:string list
  -> variables:(string * Wat_preprocess.value) list
  -> inputs:Wat_preprocess.input list
  -> output_file:string
  -> unit
