(* Js_of_ocaml tests
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2019 Ty Overby
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

open Js_of_ocaml_compiler

let print ~compact source =
  let buffer = Buffer.create (String.length source) in
  let pp = Pretty_print.to_buffer buffer in
  Pretty_print.set_compact pp compact;
  let lexed = Parse_js.lexer_from_string source in
  let parsed = Parse_js.parse lexed in
  Js_output.program pp parsed;
  print_endline (Buffer.contents buffer)

let%expect_test "no postfix addition coalesce" =
  print ~compact:true "a + +b";
  [%expect {|
    a+
    +b; |}]

let%expect_test "no postfix subtraction coalesce" =
  print ~compact:true "a - -b";
  [%expect {|
    a-
    -b; |}]

let%expect_test "reserved words as fields" =
  print ~compact:false
    {|
    x.debugger;
    x.catch;
    x.for;
    x.continue;
    var y = { debugger : 2 }
    var y = { catch : 2 }
    var y = { for : 2 }
    var y = { continue : 2 }
  |};
  [%expect {|
    x.debugger;
    x.catch;var
    y={debugger:2};var
    y={catch:2}; |}]

let%expect_test "preserve number literals" =
  print ~compact:false
    {|
     var x = 0xffff;
     var x = 0Xffff;
     var y = 071923;
     var y = 07123;
     var z = 0.0;
     var z = 0.;
     var t = 1.0e-3;
     var t = 1.0E+3;
     var t = 1e-3;
     var t = 1E+3; |};
  [%expect {||}]
