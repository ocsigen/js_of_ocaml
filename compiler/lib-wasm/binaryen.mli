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

type link_input =
  { module_name : string  (** Name under which the module is imported in other modules *)
  ; file : string  (** File containing the Wasm module *)
  ; source_map_file : string option
  }

val link :
     ?options:string list
  -> inputs:link_input list
  -> opt_output_sourcemap:string option
  -> output_file:string
  -> unit
  -> unit

val dead_code_elimination :
     dependencies:string
  -> opt_input_sourcemap:string option
  -> input_file:string
  -> opt_output_sourcemap:string option
  -> output_file:string
  -> Stdlib.StringSet.t

val optimize :
     profile:Profile.t
  -> ?options:string list
  -> opt_input_sourcemap:string option
  -> input_file:string
  -> opt_output_sourcemap:string option
  -> output_file:string
  -> unit
  -> unit
