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

type mapping = map list

type t =
  { version : int
  ; file : string
  ; sourceroot : string option
  ; sources : string list
  ; sources_content : string option list option
  ; names : string list
  ; mappings : mapping
  }

val filter_map : t -> f:(int -> int option) -> t

val merge : t list -> t option

val mapping_of_string : string -> mapping

val string_of_mapping : mapping -> string

val empty : filename:string -> t
