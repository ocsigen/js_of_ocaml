(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2025 Hugo Heuzard
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

open! Stdlib

type t =
  | O1
  | O2
  | O3

let equal (a : t) b =
  match a, b with
  | O1, O1 | O2, O2 | O3, O3 -> true
  | O1, _ | O2, _ | O3, _ -> false

let all = [ O1; O2; O3 ]

let to_int = function
  | O1 -> 1
  | O2 -> 2
  | O3 -> 3

let of_int i = List.find_map ~f:(fun p -> if i = to_int p then Some p else None) all
