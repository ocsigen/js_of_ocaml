(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2022 Hugo Heuzard
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

open! Stdlib

type t =
  { provides : StringSet.t
  ; requires : StringSet.t
  ; primitives : string list
  ; aliases : (string * string) list
  ; force_link : bool
  ; effects_without_cps : bool
  }

val of_cmo : Cmo_format.compilation_unit -> t

val of_primitives : aliases:(string * string) list -> string list -> t

val union : t -> t -> t

val empty : t

val prefix : string

val to_string : t -> string

val parse : t -> string -> t option
