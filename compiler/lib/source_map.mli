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

module Source_content : sig
  type t

  val create : string -> t
end

type map =
  | Gen of
      { gen_line : int
      ; gen_col : int
      }
  | Gen_Ori of
      { gen_line : int
      ; gen_col : int
      ; ori_source : int
      ; ori_line : int
      ; ori_col : int
      }
  | Gen_Ori_Name of
      { gen_line : int
      ; gen_col : int
      ; ori_source : int
      ; ori_line : int
      ; ori_col : int
      ; ori_name : int
      }

module Line_edits : sig
  type action =
    | Keep
    | Drop
    | Add of { count : int }

  val pp_action : Format.formatter -> action -> unit

  type t = action list

  val pp : Format.formatter -> t -> unit
end

module Mappings : sig
  type t

  val empty : t
  (** Represents the empty mapping. *)

  val of_string : string -> t
  (** By default, mappings are left uninterpreted, since many operations can be
      performed efficiently directly on the encoded form. Therefore this
      function is mostly a no-op and very cheap. It does not perform any
      validation of its argument, unlike {!val:edit} or {!val:decode}. It is
      guaranteed that {!val:of_string} and {!val:to_string} are inverse
      functions. *)

  val decode : t -> map list
  (** Parse the mappings. Prefer using the more efficient {!val:edit} on the
      uninterpreted form when applicable. *)

  val encode : map list -> t

  val to_string : t -> string
  (** Returns the mappings as a string in the Source map v3 format. This
      function is mostly a no-op and is very cheap. *)

  val edit : strict:bool -> t -> Line_edits.t -> t
  (** [edit ~strict mappings edits] applies line edits [edits] to [mappings] in
      order and returns the result. If [strict], then the number of {!const:Keep} and
      {!const:Drop} elements in [edits] should be the same or lesser than the
      number of generated lines covered by [mappings]. If the number of
      {!const:Line_edits.Keep} and {!const:Line_edits.Drop} actions is lesser
      than the number of lines in the domain of [mappings], only the lines
      affected by an edit are included in the result. If [strict] is false,
      there may be more edit actions than generated lines. In which case, a
      [Keep] that does not correspond to a line just adds a line without
      mappings and a [Drop] without a corresponding line does nothing. *)
end

type t =
  { version : int
  ; file : string
  ; sourceroot : string option
  ; sources : string list
  ; sources_content : Source_content.t option list option
  ; names : string list
  ; mappings : Mappings.t
        (** Left uninterpreted, since most useful operations can be performed efficiently
          directly on the encoded form, and a full decoding can be costly for big
          sourcemaps. *)
  }

val filter_map : t -> f:(int -> int option) -> t

val merge : t list -> t option

val empty : filename:string -> t

val concat : file:string -> sourceroot:string option -> t -> t -> t
(** If [s1] encodes a mapping for a generated file [f1], and [s2] for a
    generated file [f2], then [concat ~file ~sourceroot s1 s2] encodes the
    union of these mappings for the concatenation of [f1] and [f2], with name
    [file] and source root [sourceroot). *)

val to_string : t -> string

val to_file : t -> string -> unit

val of_string : string -> t
