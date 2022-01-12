(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2017 Hugo Heuzard
 * Copyright (C) 2019 Ty Overby
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

(* https://github.com/ocsigen/js_of_ocaml/issues/739 *)

let print_bool b = print_endline (string_of_bool b)

let%expect_test "is_int" =
  let r = ref false in
  let f x =
    match Obj.is_int x with
    | true ->
        r := true;
        true
    | false ->
        r := false;
        false
  in
  print_string "[not (is_int 1)]: ";
  print_bool (not (f (Obj.repr 1)));
  print_string "[is_int (1,2,3)]: ";
  print_bool (f (Obj.repr (1, 2, 3)));
  [%expect {|
    [not (is_int 1)]: false
    [is_int (1,2,3)]: false
  |}]

(* https://github.com/ocsigen/js_of_ocaml/issues/666 *)
(* https://github.com/ocsigen/js_of_ocaml/pull/725 *)

let%expect_test "dup" =
  let magic = "abcd" in
  let js_string_enabled =
    match Sys.backend_type with
    | Other "js_of_ocaml" -> Array.unsafe_get (Obj.magic magic) 0 == "b"
    | _ -> false
  in
  let s = "Hello" in
  let s' : string = Obj.obj (Obj.dup (Obj.repr s)) in
  print_bool (s = s');
  print_bool (js_string_enabled = (s == s'));
  [%expect {|
    true
    true |}];
  let s = Bytes.of_string "Hello" in
  let s' : bytes = Obj.obj (Obj.dup (Obj.repr s)) in
  print_bool (s = s');
  print_bool (s != s');
  Bytes.set s' 1 'a';
  print_bool (s <> s');
  [%expect {|
    true
    true
    true
  |}]
