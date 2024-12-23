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

val string_of_effects_backend : Config.effects_backend -> string

type t

type kind =
  [ `Runtime
  | `Exe
  | `Cmo
  | `Cma
  | `Unknown
  ]

val create : kind -> t

val to_string : t -> string

val parse : string -> t option

val to_map : t -> string StringMap.t

val of_map : string StringMap.t -> t

val with_kind : t -> kind -> t

exception
  Incompatible_build_info of
    { key : string
    ; first : (string * string option)
    ; second : (string * string option)
    }

val merge : string -> t -> string -> t -> t

val kind : t -> kind

val configure : t -> unit
