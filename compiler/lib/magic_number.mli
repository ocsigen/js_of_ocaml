(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
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

type t = private string * int

exception Bad_magic_number of string

exception Bad_magic_version of t

val size : int

val compare : t -> t -> int

val equal : t -> t -> bool

val of_string : string -> t

val to_string : t -> string

val kind : t -> [ `Cmo | `Cma | `Exe | `Other of string ]

val current_exe : t

val current_cmo : t

val current_cma : t

val current : [ `Cmo | `Cma | `Exe ] -> t
