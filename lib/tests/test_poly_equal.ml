(* Js_of_ocaml tests
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2019 Hugo Heuzard
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
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
open Js_of_ocaml

let%expect_test "poly equal" =
  let obj1 = Js.Unsafe.obj [||] in
  let obj2 = Js.Unsafe.obj [||] in
  assert (obj1 = obj2);
  assert (not (obj1 = obj2));
  ()
[@@expect.uncaught_exn {| "Assert_failure lib/tests/test_poly_equal.ml:24:2" |}]

let%expect_test "poly equal neg" =
  let obj1 = Js.Unsafe.obj [||] in
  let obj2 = Js.Unsafe.obj [||] in
  assert (obj1 <> obj2);
  assert (not (obj1 <> obj1));
  ()

type pack = Pack : 'a -> pack

let%expect_test "number comparison" =
  assert (Pack 2 = Pack 2);
  assert (Pack 2 <> Pack 2.1);
  assert (Pack (Js.float 2.1) = Pack (Js.float 2.1));
  assert (Pack (Js.Unsafe.js_expr "Number(2.1)") <> Pack 2.);
  assert (Pack (Js.Unsafe.js_expr "new Number(2.1)") <> Pack 2.);
  assert (Pack (Js.Unsafe.js_expr "Number(2.1)") = Pack (Js.Unsafe.js_expr "Number(2.1)"))

let%expect_test "string comparison" =
  assert (Pack (Js.Unsafe.js_expr "String(2)") = Pack (Js.string "2"));
  assert (Pack (Js.Unsafe.js_expr "String('abc')") = Pack (Js.string "abc"));
  assert (Pack (Js.Unsafe.js_expr "new String('abcሴ')") = Pack (Js.string "abcሴ"));
  assert (Pack (Js.Unsafe.js_expr "String(1)") <> Pack (Js.string "2"));
  assert (Pack (Js.Unsafe.js_expr "String('abcd')") <> Pack (Js.string "abc"));
  assert (Pack (Js.Unsafe.js_expr "new String('abcd')") <> Pack (Js.string "abc"));
  assert (
    Pack (Js.Unsafe.js_expr "String('abcd')") = Pack (Js.Unsafe.js_expr "String('abcd')"))

let%expect_test "symbol comparison" =
  let s1 = Pack (Js.Unsafe.js_expr "Symbol('2')") in
  let s2 = Pack (Js.Unsafe.js_expr "Symbol('2')") in
  assert (s1 <> s2);
  assert (s1 = s1)

let%expect_test "object comparison" =
  let s1 = Pack (Js.Unsafe.js_expr "{}") in
  let s2 = Pack (Js.Unsafe.js_expr "{}") in
  assert (s1 <> s2);
  assert (s1 = s1)
