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
      print_newline ()
    done
  with Calc_lexer.Eof -> print_endline "EOF"

let%expect_test "parsing" =
  let (old : bool) = Parsing.set_trace true in
  parse "1+2*3";
  [%expect
    {|
       State 0: shift to state 1
       State 1: read token INT(1)
       State 1: shift to state 3
       State 3: reduce by rule 2
       State 7: read token PLUS
       State 7: shift to state 10
       State 10: read token INT(2)
       State 10: shift to state 3
       State 3: reduce by rule 2
       State 16: read token TIMES
       State 16: shift to state 12
       State 12: read token INT(3)
       State 12: shift to state 3
       State 3: reduce by rule 2
       State 18: reduce by rule 6
       EOF |}];
  parse "(1+2)*3";
  [%expect
    {|
       State 0: shift to state 1
       State 1: read token LPAREN
       State 1: shift to state 5
       State 5: read token INT(1)
       State 5: shift to state 3
       State 3: reduce by rule 2
       State 9: read token PLUS
       State 9: shift to state 10
       State 10: read token INT(2)
       State 10: shift to state 3
       State 3: reduce by rule 2
       State 16: read token RPAREN
       State 16: reduce by rule 4
       State 9: shift to state 15
       State 15: reduce by rule 3
       State 7: read token TIMES
       State 7: shift to state 12
       State 12: read token INT(3)
       State 12: shift to state 3
       State 3: reduce by rule 2
       State 18: reduce by rule 6
       EOF |}];
  parse "-10-1";
  [%expect
    {|
       State 0: shift to state 1
       State 1: read token MINUS
       State 1: shift to state 4
       State 4: read token INT(10)
       State 4: shift to state 3
       State 3: reduce by rule 2
       State 8: reduce by rule 8
       State 7: read token MINUS
       State 7: shift to state 11
       State 11: read token INT(1)
       State 11: shift to state 3
       State 3: reduce by rule 2
       EOF |}];
  parse "63/2*-3";
  [%expect
    {|
       State 0: shift to state 1
       State 1: read token INT(63)
       State 1: shift to state 3
       State 3: reduce by rule 2
       State 7: read token DIV
       State 7: shift to state 13
       State 13: read token INT(2)
       State 13: shift to state 3
       State 3: reduce by rule 2
       State 19: reduce by rule 7
       State 7: read token TIMES
       State 7: shift to state 12
       State 12: read token MINUS
       State 12: shift to state 4
       State 4: read token INT(3)
       State 4: shift to state 3
       State 3: reduce by rule 2
       State 8: reduce by rule 8
       State 18: reduce by rule 6
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
