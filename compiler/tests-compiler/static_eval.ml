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

open Util

let%expect_test "static eval of string get" =
  let program =
    compile_and_parse
      ~flags:[ "--enable"; "use-js-string" ]
      {|
    let lr = ref []
    let black_box v = lr := (Obj.repr v) :: !lr

    let constant = "abcdefghijklmnopqrstuvwxyz"

    let call_with_char c = black_box c

    let ex = call_with_char constant.[-10] ;;
    black_box ex
    let ax = call_with_char constant.[6]  ;;
    black_box ax
    let bx = call_with_char constant.[30] ;;
    black_box bx ;;
  |}
  in
  print_var_decl program "ex";
  print_var_decl program "ax";
  print_var_decl program "bx";
  [%expect
    {|
    var ex = call_with_char(caml_string_get(cst_abcdefghijklmnopqrstuvwxyz,- 10));
    var ax = call_with_char(103);
    var bx = call_with_char(caml_string_get(cst_abcdefghijklmnopqrstuvwxyz,30)); |}]

let%expect_test "static eval of string get" =
  let program =
    compile_and_parse
      ~flags:[ "--disable"; "use-js-string" ]
      {|
    let lr = ref []
    let black_box v = lr := (Obj.repr v) :: !lr

    let constant = "abcdefghijklmnopqrstuvwxyz"

    let call_with_char c = black_box c

    let ex = call_with_char constant.[-10] ;;
    black_box ex
    let ax = call_with_char constant.[6]  ;;
    black_box ax
    let bx = call_with_char constant.[30] ;;
    black_box bx ;;
  |}
  in
  print_var_decl program "ex";
  print_var_decl program "ax";
  print_var_decl program "bx";
  [%expect
    {|
    var ex = call_with_char(caml_string_get(constant,- 10));
    var ax = call_with_char(103);
    var bx = call_with_char(caml_string_get(constant,30)); |}]
