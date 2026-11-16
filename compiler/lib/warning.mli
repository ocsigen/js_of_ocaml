(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2025 Hugo Heuzard
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

type t =
  [ (* Parsing bytecode *)
    `Integer_overflow
  | `Missing_debug_event
  | `Missing_cmi
  | `Effect_handlers_without_effect_backend
  | (* runtime *)
    `Missing_primitive
  | `Missing_define
  | `Missing_deps
  | `Free_variables_in_primitive
  | `Deprecated_joo_global_object
  | `Overriding_primitive
  | `Overriding_primitive_purity
  | `Deprecated_primitive
  | `Unused_js_variable
  ]

val all : t list

val name : t -> string

val parse : string -> t option

val enable : t -> unit

val disable : t -> unit

val enabled : t -> bool

val quiet : bool ref

val werror : bool ref

val warn : t -> ('a, Format.formatter, unit, unit) format4 -> 'a

val process_warnings : unit -> unit
