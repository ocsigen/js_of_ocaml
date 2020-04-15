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

module Fragment : sig
  type provides =
    { parse_info : Parse_info.t option
    ; name : string
    ; kind : Primitive.kind
    ; kind_args : Primitive.kind_arg list option
    ; arity : int option
    ; named_values : StringSet.t
    }

  type t =
    { provides : provides option
    ; locations : Parse_info.t * Parse_info.t
    ; requires : string list
    ; version_constraint : ((int -> int -> bool) * string) list list
    ; weakdef : bool
    ; code : Javascript.program
    ; ignore : [ `No | `Because of Primitive.condition ]
    }

  val parse_file : string -> t list

  val parse_string : string -> t list

  val parse_builtin : Builtins.File.t -> t list
end

val load_files : filenames:string list -> unit

val load_fragment : filename:string -> Fragment.t -> unit

val check_deps : unit -> unit

type state

type always_required =
  { filename : string
  ; program : Javascript.program
  }

type output =
  { runtime_code : Javascript.program
  ; always_required_codes : always_required list
  }

val init : unit -> state

val resolve_deps : ?linkall:bool -> state -> StringSet.t -> state * StringSet.t

val link : Javascript.program -> state -> output

val get_provided : unit -> StringSet.t

val all : state -> string list
