(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2017 Hugo Heuzard
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

  val read_imports : file:string -> import list
end

type unit_data =
  { unit_info : Unit_info.t
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
     ?link_spec:(string * int list option) list
  -> ?separate_compilation:bool
  -> missing_primitives:string list
  -> wasm_file:string
  -> generated_js:
       (string option * (string list * (string * Javascript.expression) list)) list
  -> unit
  -> Javascript.expression

val simplify_unit_info : unit_data list -> unit_data list

val output_js : Javascript.program -> string

val link :
     output_file:string
  -> linkall:bool
  -> enable_source_maps:bool
  -> files:string list
  -> unit
