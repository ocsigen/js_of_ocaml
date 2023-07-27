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

type t = int list

val current : t

val compare : t -> t -> int

val split : string -> t

val v :
  [ `V4_08 (* OCaml 4.08 *)
  | `V4_09 (* OCaml 4.09 *)
  | `V4_10 (* OCaml 4.10 *)
  | `V4_11 (* OCaml 4.11 *)
  | `V4_12 (* OCaml 4.12 *)
  | `V4_13 (* OCaml 4.13 *)
  | `V4_14 (* OCaml 4.14 *)
  | `V5_00
  | `V5_01
  | `V5_02
  ]
