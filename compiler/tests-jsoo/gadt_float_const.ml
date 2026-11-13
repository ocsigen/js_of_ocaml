(* Js_of_ocaml tests
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2026 Jérôme Vouillon
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02111-1307, USA.
 *)

(* With GADTs, an integer operation can be applied to a float constant in
   dead code, once a function is inlined. Js_of_ocaml used to fail when
   simplifying such an operation: the constant is not an integer literal. *)

type _ t =
  | Int : int t
  | Float : float t

let f (type a) (w : a t) (x : a) : int =
  match w with
  | Int -> x + 1 + (x - 1 + 2) + (x - 1 - 2) + (1 lsr x) + [| 1; 2 |].(x)
  | Float -> int_of_float (x +. 1.)

let g w = f w 5.5

let%expect_test _ =
  ignore (Array.make 1 Int);
  Printf.printf "%d %d\n" (g (Array.make 1 Float).(0)) (g (Array.make 1 Float).(0));
  [%expect {| 6 6 |}]
