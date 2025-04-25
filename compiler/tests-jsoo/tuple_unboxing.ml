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
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

(* The type of [x] is only known to be a pair once the bound check
   has succeeded, so its fields must not be read when calling [f]. *)

type (_, _) eq = Refl : ('a, 'a) eq

let f (type a) (w : (a, int * int) eq array) (x : a) : int =
  match w.(0) with
  | Refl -> fst x + snd x

let%expect_test "type refined after an instruction which may raise" =
  Printf.printf "%d\n" (f [| Refl |] (1, 2));
  Printf.printf "%d\n" (try f [||] 3 with Invalid_argument _ -> -1);
  [%expect {|
    3
    -1
    |}]
