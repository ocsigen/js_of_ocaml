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

module Offset : sig
  type t =
    { gen_line : int
    ; gen_column : int
    }
end

module Mappings : sig
  type decoded = map list

  type t
  (** Represent the a list of mapping in its encoded form. *)

  val empty : t
  (** The empty mapping. *)

  val of_string_unsafe : string -> t
  (** [of_string_unsafe] does not perform any validation of its argument, unlike
      {!val:decode}. It is guaranteed that {!val:of_string_unsafe} and {!val:to_string}
      are inverse functions. Time complexity O(1) *)

  val decode_exn : t -> decoded
  (** Parse the mappings. *)

  val encode : decoded -> t
  (** Encode the mappings. *)

  val encode_with_offset : decoded -> Offset.t * t
  (** Encode the mappings shifted by the returned offset so that the encoded mapping is
      more compact. This is useful to combining multiple mappings into an [Index.t] *)

  val number_of_lines : t -> int

  val first_line : t -> int

  val to_string : t -> string
  (** Returns the mappings as a string in the Source map v3 format. Time complexity O(1)
  *)
end

module Standard : sig
  type t =
    { version : int
    ; file : string option
    ; sourceroot : string option
    ; sources : string list
    ; sources_content : Source_content.t option list option
    ; names : string list
    ; mappings : Mappings.t
          (** Left uninterpreted, since most useful operations can be performed
              efficiently directly on the encoded form, and a full decoding can be costly
              for big sourcemaps. *)
    ; ignore_list : string list
    }

  val filter_map : t -> f:(int -> int option) -> t
  (** If [f l] returns [Some l'], map line [l] to [l'] (in the generated file) in the
      returned debug mappings. If [f l] returns [None], remove debug mappings which
      concern line [l] of the generated file. *)

  val merge : t list -> t option
  (** Merge two lists of debug mappings. The time cost of the merge is more than linear in
      function of the size of the input mappings. *)

  val empty : inline_source_content:bool -> t
end

module Index : sig
  type section =
    { offset : Offset.t
    ; map : Standard.t
    }

  type t =
    { version : int
    ; file : string option
    ; sections : section list
    }
end

type t =
  | Standard of Standard.t
  | Index of Index.t

val to_string : t -> string

val to_file : t -> string -> unit

val of_string : string -> t

val of_file : string -> t

val invariant : t -> unit

type info =
  { mappings : Mappings.decoded
  ; sources : string list
  ; names : string list
  }
