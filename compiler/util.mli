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

module BitSet : sig
  type t
  val create : unit -> t
  val mem : t -> int -> bool
  val set : t -> int -> unit
  val unset : t -> int -> unit
  val next_free : t -> int -> int
  val next_mem : t -> int -> int
  val iter : (int -> unit) -> t -> unit
  val copy : t -> t
  val size : t -> int
end

val quiet : bool ref
val warn : ('a,unit,string,unit) format4 -> 'a

val opt_filter : ('a -> bool) -> 'a option -> 'a option
val opt_map : ('a -> 'b) -> 'a option -> 'b option
val opt_iter : ('a -> unit) -> 'a option -> unit
val filter_map : ('a -> 'b option) -> 'a list -> 'b list
val sort_uniq : ('a -> 'a -> int) -> 'a list -> 'a list
val array_fold_right_i : (int -> 'a -> 'b -> 'b) -> 'a array -> 'b -> 'b

val path_require_findlib : string -> string option
val set_find_pkg_dir : (string -> string) -> unit
val find_pkg_dir : string -> string
val find_in_findlib_paths : ?pkg:string -> string list -> string -> string
val find_in_path : string list -> string -> string
val absolute_path : string -> string
val read_file : string -> string

val take : int -> 'a list -> 'a list * 'a list
val last : 'a list -> 'a option

val is_ascii : string -> bool
val has_backslash : string -> bool

module Timer : sig
  type t
  val init : (unit -> float) -> unit
  val make : unit -> t
  val get : t -> float
  val print : Format.formatter -> t -> unit
end

val fail : bool ref
val failwith_ : ('a,unit,string,unit) format4 -> 'a
val raise_ : exn -> unit

val split_char : char -> string -> string list
val split : string -> string -> string list
val find : string -> Bytes.t -> int

(* [normalize_argv argv] returns a new array of arguments where '-long-option' are replaced by '--long-option' *)
val normalize_argv : ?warn_:bool -> string array -> string array

module Version : sig
  type t = int list
  val current : t
  val compare : t -> t -> int
  val split : string -> t
  val v : [ `V3      (* OCaml 3.12 to 4.01 *)
          | `V4_02   (* OCaml 4.02 *)
          | `V4_03   (* OCaml 4.03 *)
          | `V4_04   (* OCaml 4.04 *)
          ]
end

module MagicNumber : sig
  type t = private string * int
  exception Bad_magic_number of string
  exception Bad_magic_version of t

  val size : int
  val compare : t -> t -> int
  val of_string : string -> t
  val to_string : t -> string
  val kind : t -> [ `Cmo | `Cma | `Exe | `Other of string]
  val current_exe : t
  val current_cmo : t
  val current_cma : t
  val current : [ `Cmo | `Cma | `Exe ] -> t
end

val obj_of_const : Lambda.structured_constant -> Obj.t

val uncapitalize_ascii : string -> string
val capitalize_ascii   : string -> string


val find_loc_in_summary : string -> Ident.t -> Env.summary -> Location.t option
