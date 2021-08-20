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

module Debug : sig
  type t

  val create : toplevel:bool -> bool -> t

  val find_loc : t -> ?after:bool -> int -> Parse_info.t option

  val is_empty : t -> bool

  val paths : t -> units:StringSet.t -> StringSet.t
end

type one =
  { code : Code.program
  ; cmis : StringSet.t
  ; debug : Debug.t
  }

module Toc : sig
  type t

  val read : in_channel -> t
end

val read_primitives : Toc.t -> in_channel -> string list

val from_exe :
     ?includes:string list
  -> ?toplevel:bool
  -> ?exported_unit:string list
  -> ?dynlink:bool
  -> ?debug:bool
  -> in_channel
  -> one

val from_cmo :
     ?includes:string list
  -> ?toplevel:bool
  -> ?debug:bool
  -> Cmo_format.compilation_unit
  -> in_channel
  -> one

val from_cma :
     ?includes:string list
  -> ?toplevel:bool
  -> ?debug:bool
  -> Cmo_format.library
  -> in_channel
  -> one

val from_channel :
     in_channel
  -> [ `Cmo of Cmo_format.compilation_unit | `Cma of Cmo_format.library | `Exe ]

val from_string : string array -> string -> Code.program * Debug.t

val predefined_exceptions : unit -> Code.program
