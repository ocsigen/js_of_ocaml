(* Js_of_ocaml tests
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2026 Ocsigen team
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

(* A number constant can reach, on a dead path, a use expecting a number of
   another kind: here, once the functions are inlined, the integer addition
   is applied to a float constant and to an int32 constant. *)

type _ t =
  | I : int t
  | F : float t
  | N : int32 t

let g (type u) (b : u t) (x : u) : u =
  match b with
  | I -> x + 1
  | F -> x +. 1.
  | N -> Int32.add x 1l

let h (type u) (b : u t) (x : u) : u =
  match b with
  | I -> x + 1
  | F -> x +. 1.
  | N -> Int32.add x 1l

let g' b = g b 5.

let h' b = h b 7l

let () =
  ignore (Sys.opaque_identity I);
  assert (g' F = 6.);
  assert (g' F = 6.);
  assert (h' N = 8l);
  assert (h' N = 8l)
