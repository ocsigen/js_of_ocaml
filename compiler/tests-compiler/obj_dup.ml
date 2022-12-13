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

(* https://github.com/ocsigen/js_of_ocaml/issues/666 *)
(* https://github.com/ocsigen/js_of_ocaml/pull/725 *)

let%expect_test _ =
  Util.compile_and_run
    ~use_js_string:true
    {|
    let print_bool b = print_endline (string_of_bool b)
    let () =
      let s = "Hello" in
      let s' : string = Obj.obj (Obj.dup (Obj.repr s)) in
      print_bool (s = s');
      print_bool (s != s')
  |};
  [%expect {|
    true
    false
  |}]

let%expect_test _ =
  Util.compile_and_run
    ~use_js_string:true
    {|
    let print_bool b = print_endline (string_of_bool b)
    let () =
      let s = Bytes.of_string "Hello" in
      let s' : bytes = Obj.obj (Obj.dup (Obj.repr s)) in
      print_bool (s = s');
      print_bool (s != s');
      Bytes.set s' 1 'a';
      print_bool (s <> s')
  |};
  [%expect {|
    true
    true
    true
  |}]

let%expect_test _ =
  Util.compile_and_run
    ~use_js_string:false
    {|
    let print_bool b = print_endline (string_of_bool b)
    let () =
      let s = "Hello" in
      let s' : string = Obj.obj (Obj.dup (Obj.repr s)) in
      print_bool (s = s');
      print_bool (s != s')
  |};
  [%expect {|
    true
    true
  |}]

let%expect_test _ =
  Util.compile_and_run
    ~use_js_string:false
    {|
    let print_bool b = print_endline (string_of_bool b)
    let () =
      let s = Bytes.of_string "Hello" in
      let s' : bytes = Obj.obj (Obj.dup (Obj.repr s)) in
      print_bool (s = s');
      print_bool (s != s');
      Bytes.set s' 1 'a';
      print_bool (s <> s')
  |};
  [%expect {|
    true
    true
    true
  |}]
