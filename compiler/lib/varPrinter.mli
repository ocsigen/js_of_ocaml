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

type t

val add_reserved : string list -> unit
val get_reserved : unit -> Util.StringSet.t
val create : ?pretty:bool -> ?stable:bool -> unit -> t
val reset : t -> unit
val to_string : t -> ?origin:int -> int -> string
val name : t -> int -> string -> unit
val get_name : t -> int -> string option
val propagate_name : t -> int -> int -> unit
val set_pretty : t -> bool -> unit
val set_stable : t -> bool -> unit
