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

open Stdlib

type fragment =
  { provides :
      (Parse_info.t option * string * Primitive.kind * Primitive.kind_arg list option)
      option
  ; requires : string list
  ; version_constraint : ((int -> int -> bool) * string) list list
  ; weakdef : bool
  ; code : Javascript.program }

val parse_file : string -> fragment list

val load_files : string list -> unit

type state

type always_required =
  { filename : string
  ; program : Javascript.program }

type output =
  { runtime_code : Javascript.program
  ; always_required_codes : always_required list }

val init : unit -> state

val resolve_deps : ?linkall:bool -> state -> StringSet.t -> state * StringSet.t

val link : Javascript.program -> state -> output

val get_provided : unit -> StringSet.t

val all : state -> string list
