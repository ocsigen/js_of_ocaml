(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
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

module Jsoo = Js_of_ocaml_compiler

let print_macro_transformed source =
  Util.with_temp_dir ~f:(fun () ->
      let buffer = Buffer.create (String.length source) in
      let pp = Jsoo.Pretty_print.to_buffer buffer in
      Jsoo.Pretty_print.set_compact pp false;
      let source =
        source
        |> Util.Filetype.js_text_of_string
        |> Util.Filetype.write_js ~name:"test.js"
      in
      let parsed = Util.parse_js source in
      let transformed, _ = Jsoo.Macro.f ~flags:false parsed in
      let (_ : Jsoo.Source_map.info) = Jsoo.Js_output.program pp transformed in
      print_endline (Buffer.contents buffer))

let print_macro_transformed source =
  Jsoo.Targetint.set_num_bits 32;
  try print_macro_transformed source with Failure s -> Format.printf "failure: %s%!" s

let%expect_test "BLOCK()" =
  print_macro_transformed "BLOCK()";
  [%expect {| failure: macro BLOCK called with inappropriate arguments |}]

let%expect_test "TAG()" =
  print_macro_transformed "TAG()";
  [%expect {| failure: macro TAG called with inappropriate arguments |}]

let%expect_test "LENGTH()" =
  print_macro_transformed "LENGTH()";
  [%expect {| failure: macro LENGTH called with inappropriate arguments |}]

let%expect_test "FIELD()" =
  print_macro_transformed "FIELD()";
  [%expect {| failure: macro FIELD called with inappropriate arguments |}]

let%expect_test "ISBLOCK()" =
  print_macro_transformed "ISBLOCK()";
  [%expect {| failure: macro ISBLOCK called with inappropriate arguments |}]

let%expect_test "BLOCK(1)" =
  print_macro_transformed "BLOCK(1)";
  [%expect {| failure: macro BLOCK called with inappropriate arguments |}]

let%expect_test "BLOCK(tag)" =
  print_macro_transformed "BLOCK(tag)";
  [%expect {| failure: macro BLOCK called with inappropriate arguments |}]

let%expect_test "BLOCK(1, a)" =
  print_macro_transformed "BLOCK(1, a)";
  [%expect {| [1, a]; |}]

let%expect_test "BLOCK(1, a, b, c)" =
  print_macro_transformed "BLOCK(1, a, b, c)";
  [%expect {| [1, a, b, c]; |}]

let%expect_test "BLOCK(077, a)" =
  print_macro_transformed "BLOCK(077, a)";
  [%expect {| [63, a]; |}]

let%expect_test "BLOCK(0779, a)" =
  print_macro_transformed "BLOCK(0779, a)";
  [%expect {| [779, a]; |}]

let%expect_test "TAG(a)" =
  print_macro_transformed "TAG(a)";
  [%expect {| a[0]; |}]

let%expect_test "LENGTH(a)" =
  print_macro_transformed "LENGTH(a)";
  [%expect {| a.length - 1; |}]

let%expect_test "FIELD(a)" =
  print_macro_transformed "FIELD(a)";
  [%expect {| failure: macro FIELD called with inappropriate arguments |}]

let%expect_test "FIELD(a, b)" =
  print_macro_transformed "FIELD(a, b)";
  [%expect {| failure: macro FIELD called with inappropriate arguments |}]

let%expect_test "FIELD(a, b << 5)" =
  print_macro_transformed "FIELD(a, b << 5)";
  [%expect {| failure: macro FIELD called with inappropriate arguments |}]

let%expect_test "FIELD(a, 0)" =
  print_macro_transformed "FIELD(a, 0)";
  [%expect {| a[1]; |}]

let%expect_test "FIELD(a, -1)" =
  print_macro_transformed "FIELD(a, -1)";
  [%expect {| failure: Negative field indexes are not allowed |}]

let%expect_test "ISBLOCK(a)" =
  print_macro_transformed "ISBLOCK(a)";
  [%expect {| typeof a !== "number"; |}]
