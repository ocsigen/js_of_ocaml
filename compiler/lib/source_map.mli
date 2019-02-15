(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2013 Hugo Heuzard
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

type map =
  { gen_line : int
  ; gen_col : int
  ; ori_source : int
  ; ori_line : int
  ; ori_col : int
  ; ori_name : int option }

type mapping = map list

type t =
  { version : int
  ; file : string
  ; sourceroot : string option
  ; mutable sources : string list
  ; mutable sources_content : string option list option
  ; mutable names : string list
  ; mutable mappings : mapping }

val merge : (int * string * t) list -> t option

val mapping_of_string : string -> mapping

val string_of_mapping : mapping -> string
