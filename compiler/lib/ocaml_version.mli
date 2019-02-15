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
  [ `V4_02 (* OCaml 4.02 *)
  | `V4_03 (* OCaml 4.03 *)
  | `V4_04 (* OCaml 4.04 / OCaml 4.05 *)
  | `V4_06 (* OCaml 4.06 *)
  | `V4_07 (* OCaml 4.07 *) ]
