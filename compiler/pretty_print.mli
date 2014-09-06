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

type t

type pos = {
  mutable p_line : int;
  mutable p_col : int
}

val string : t -> string -> unit

val genbreak : t -> string -> int -> unit
val break : t -> unit
val break1 : t -> unit
val non_breaking_space : t -> unit
val space : ?indent:int -> t -> unit

val start_group : t -> int -> unit
val end_group : t -> unit

val newline : t -> unit

val to_out_channel : out_channel -> t
val to_buffer : Buffer.t -> t
val pos : t -> pos
val total : t -> int

val set_compact : t -> bool -> unit
val set_needed_space_function : t -> (char -> char -> bool) -> unit
