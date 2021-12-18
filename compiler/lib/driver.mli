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

val f :
     ?standalone:bool
  -> ?global:[ `globalThis | `Function | `Bind_to of string | `Custom of string ]
  -> ?profile:profile
  -> ?dynlink:bool
  -> ?linkall:bool
  -> ?source_map:string option * Source_map.t
  -> ?custom_header:string
  -> Pretty_print.t
  -> Parse_bytecode.Debug.t
  -> Code.program
  -> unit

val from_string : string array -> string -> Pretty_print.t -> unit

val profiles : (int * profile) list

val profile : int -> profile option
