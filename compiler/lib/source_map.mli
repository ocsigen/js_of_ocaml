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

module Mappings : sig
  type t

  val empty : t
  (** Represents the empty mapping. *)

  val of_string : string -> t
  (** By default, mappings are left uninterpreted, since many operations can be
      performed efficiently directly on the encoded form. Therefore this
      function is mostly a no-op and very cheap. It does not perform any
      validation of its argument, unlike {!val:decode}. It is guaranteed that
      {!val:of_string} and {!val:to_string} are inverse functions. *)

  val decode : t -> map list
  (** Parse the mappings. *)

  val encode : map list -> t

  val to_string : t -> string
  (** Returns the mappings as a string in the Source map v3 format. This
      function is mostly a no-op and is very cheap. *)
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

val to_string : t -> string

val to_file : t -> string -> unit

val of_string : string -> t

val of_file : string -> t
