(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2010 Jérôme Vouillon
 * Laboratoire PPS - CNRS Université Paris Diderot
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

type profile

type 'a target =
  | JavaScript : Pretty_print.t -> Source_map.t option target
  | Wasm
      : (Deadcode.variable_uses * Effects.in_cps * Code.program * Parse_bytecode.Debug.t)
        target

val f :
     target:'result target
  -> ?standalone:bool
  -> ?wrap_with_fun:[ `Iife | `Anonymous | `Named of string ]
  -> ?profile:profile
  -> link:[ `All | `All_from of string list | `Needed | `No ]
  -> ?source_map:Source_map.t
  -> Parse_bytecode.Debug.t
  -> Code.program
  -> 'result

val f' :
     ?standalone:bool
  -> ?wrap_with_fun:[ `Iife | `Anonymous | `Named of string ]
  -> ?profile:profile
  -> link:[ `All | `All_from of string list | `Needed | `No ]
  -> Pretty_print.t
  -> Parse_bytecode.Debug.t
  -> Code.program
  -> unit

val from_string :
     prims:string array
  -> debug:Instruct.debug_event list array
  -> string
  -> Pretty_print.t
  -> unit

val link_and_pack :
     ?standalone:bool
  -> ?wrap_with_fun:[ `Iife | `Anonymous | `Named of string ]
  -> ?link:[ `All | `All_from of string list | `Needed | `No ]
  -> Javascript.statement_list
  -> Javascript.statement_list

val configure : Pretty_print.t -> unit

val profiles : (int * profile) list

val profile : int -> profile option
