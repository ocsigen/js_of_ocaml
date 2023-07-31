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
  type t

  val provides : t -> string list

  val parse_file : string -> t list

  val parse_string : string -> t list

  val parse_builtin : Builtins.File.t -> t list

  val pack : t -> t
end

val reset : unit -> unit

val load_files :
  ?ignore_always_annotation:bool -> target_env:Target_env.t -> string list -> unit

val load_fragments :
     ?ignore_always_annotation:bool
  -> target_env:Target_env.t
  -> filename:string
  -> Fragment.t list
  -> unit

val check_deps : unit -> unit

type state

type always_required =
  { filename : string
  ; program : Javascript.program
  ; requires : string list
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

val origin : name:string -> string option
