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

module Debug : sig
  type data
  val create : unit -> data
  val find_loc : data -> ?after:bool -> int -> Parse_info.t option
  val is_empty : data -> bool
end

val from_channel :
  ?includes: string list ->
  ?toplevel:bool -> ?dynlink:bool -> ?debug:[`Full | `Names | `No] -> in_channel ->
  Code.program * Util.StringSet.t * Debug.data * bool

val from_string : string array -> string -> Code.program * Debug.data
