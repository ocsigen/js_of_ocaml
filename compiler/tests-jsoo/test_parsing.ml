(* Js_of_ocaml tests
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2022 Hugo Heuzard
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

let parse s =
  try
    let lexbuf = Lexing.from_string s in
    while true do
      let result = Calc_parser.main Calc_lexer.token lexbuf in
      print_int result;
      print_newline ();
      flush stdout
    done
  with Calc_lexer.Eof -> print_endline "EOF"

let%expect_test "parsing" =
  let (old : bool) = Parsing.set_trace false in
  parse "1+2*3";
  [%expect {|
    EOF |}];
  parse "(1+2)*3";
  [%expect {|
    EOF |}];
  parse "-10-1";
  [%expect {|
    EOF |}];
  parse "63/2*-3";
  [%expect {|
    EOF |}];
  let (_ : bool) = Parsing.set_trace old in
  parse "1+2*3";
  [%expect {| EOF |}];
  parse "(1+2)*3";
  [%expect {| EOF |}];
  parse "-10-1";
  [%expect {| EOF |}];
  parse "63/2*-3";
  [%expect {| EOF |}]
