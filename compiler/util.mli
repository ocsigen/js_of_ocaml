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

module IntSet : Set.S with type elt = int
module IntMap : Map.S with type key = int

module StringSet : Set.S with type elt = string
module StringMap : Map.S with type key = string

val opt_filter : ('a -> bool) -> 'a option -> 'a option
val opt_map : ('a -> 'b) -> 'a option -> 'b option
val opt_iter : ('a -> unit) -> 'a option -> unit
val filter_map : ('a -> 'b option) -> 'a list -> 'b list


val path_require_findlib : string -> string option
val find_pkg_dir : string -> string
val find_in_paths : ?pkg:string -> string list -> string -> string
val read_file : string -> string

val take : int -> 'a list -> 'a list * 'a list

val is_ascii : string -> bool
val has_backslash : string -> bool

module Timer : sig
  type t
  val init : (unit -> float) -> unit
  val make : unit -> t
  val get : t -> float
  val print : Format.formatter -> t -> unit
end
