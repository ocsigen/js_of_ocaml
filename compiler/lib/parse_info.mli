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

type t = { src  : string option
	 ; name : string option
	 ; col  : int
	 ; line : int
	 ; idx  : int
	 ; fol  : bool option
	 }

val zero : t

type lineinfo

val make_lineinfo_from_file : string -> lineinfo
val make_lineinfo_from_string : ?offset:t -> string -> lineinfo
val make_lineinfo_from_channel : in_channel -> lineinfo * string

val relative_path : lineinfo -> string -> string option

val t_of_lexbuf : lineinfo -> Lexing.lexbuf -> t
