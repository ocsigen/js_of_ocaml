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

open Stdlib

module Wasm_binary : sig
  type import =
    { module_ : string
    ; name : string
    }

  val check : contents:string -> bool
  (** Checks whether [contents] is a Wasm Module *)

  val check_file : file:string -> bool
  (** Checks whether [file] contains a Wasm Module *)

  val read_imports : file:string -> import list

  val append_source_map_section : file:string -> url:string -> unit
end

type unit_data =
  { unit_name : string
  ; unit_info : Unit_info.t
  ; strings : string list
  ; fragments : (string * Javascript.expression) list
  }

val add_info :
     Zip.output
  -> ?predefined_exceptions:StringSet.t
  -> build_info:Build_info.t
  -> unit_data:unit_data list
  -> unit
  -> unit

val build_runtime_arguments :
     link_spec:(string * int list option) list
  -> separate_compilation:bool
  -> missing_primitives:string list
  -> wasm_dir:string
  -> generated_js:
       (string option * (string list * (string * Javascript.expression) list)) list
  -> unit
  -> Javascript.expression

val output_js : Javascript.program -> string

val link :
     output_file:string
  -> linkall:bool
  -> mklib:bool
  -> enable_source_maps:bool
  -> files:string list
  -> unit

val source_name : int option -> int option -> string -> string

val gen_dir : string -> (string -> 'a) -> 'a
