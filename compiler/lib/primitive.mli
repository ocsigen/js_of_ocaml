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

val is_pure : string -> bool

val exists : string -> bool

type kind =
  [ `Pure
  | `Mutable
  | `Mutator
  ]

type kind_arg =
  [ `Shallow_const
  | `Object_literal
  | `Const
  | `Mutable
  ]

type condition =
  [ `If of string
  | `Ifnot of string
  ]

type t =
  [ `Requires of string list
  | `Provides of string * kind * kind_arg list option
  | `Version of ((int -> int -> bool) * string) list
  | `Weakdef
  | `Always
  | `Alias of string
  | `Deprecated of string
  | condition
  ]

val kind : string -> kind

val kind_args : string -> kind_arg list option

val register : string -> kind -> kind_arg list option -> int option -> unit

val arity : string -> int

val has_arity : string -> int -> bool

val alias : string -> string -> unit

val aliases : unit -> (string * string) list

val resolve : string -> string

val add_external : string -> unit

val get_external : unit -> StringSet.t

val need_named_value : string -> bool

val register_named_value : string -> unit

val reset : unit -> unit
