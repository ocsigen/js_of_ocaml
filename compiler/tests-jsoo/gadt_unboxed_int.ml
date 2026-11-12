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

(* With GADTs, a variable known to be an integer can be used as a block or
   a closure in dead code. Wasm_of_ocaml keeps such a variable unboxed and
   must box it before using it as a reference. *)

type _ t =
  | Int : int t
  | Ref : int ref t
  | Arr : int array t
  | Fun : (int -> unit) t
  | Fun2 : (int -> int -> unit) t

let f (type a) (w : a t) (x : a) =
  match w with
  | Int -> print_endline "int"
  | Ref -> incr (x : int ref)
  | Arr -> Array.unsafe_set (x : int array) 0 1
  | Fun -> (x : int -> unit) 1
  | Fun2 -> (x : int -> int -> unit) 1 2

let%expect_test _ =
  f (Array.make 1 Int).(0) 0;
  [%expect {| int |}]
