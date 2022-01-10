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

open Js_of_ocaml_compiler.Stdlib
open Js_of_ocaml_compiler

let print ~compact source =
  let buffer = Buffer.create (String.length source) in
  let pp = Pretty_print.to_buffer buffer in
  Pretty_print.set_compact pp compact;
  let lexbuf = Lexing.from_string source in
  let lexed = Parse_js.Lexer.of_lexbuf lexbuf in
  let parsed = Parse_js.parse lexed in
  Js_output.program pp parsed;
  print_endline (Buffer.contents buffer)

let%expect_test "spread operator survives round-trip" =
  print ~compact:true "f(...[1, 2, 3])";
  [%expect {| f(...[1,2,3]); |}]

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
  print
    ~compact:false
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
  [%expect
    {|
    x.debugger;
    x.catch;
    x.for;
    x.continue;
    var y={debugger:2};
    var y={catch:2};
    var y={for:2};
    var y={continue:2}; |}]

let%expect_test "preserve number literals" =
  print
    ~compact:false
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
  [%expect
    {|
    var x=0xffff;
    var x=0Xffff;
    var y=071923;
    var y=07123;
    var z=0.0;
    var z=0.;
    var t=1.0e-3;
    var t=1.0E+3;
    var t=1e-3;
    var t=1E+3; |}]

let%expect_test "preserve number literals in property_name" =
  print ~compact:false {|
    var number_as_key = { 100000000000000000000 : 2 }; |};
  [%expect {|
    var number_as_key={100000000000000000000:2}; |}]

let%expect_test "error reporting" =
  (try print ~compact:false {|
    var x = 2;
    {
    var = 5;
    }
    |}
   with Parse_js.Parsing_error pi ->
     Printf.printf
       "cannot parse js (from l:%d, c:%d)@."
       pi.Parse_info.line
       pi.Parse_info.col);
  [%expect {|
    cannot parse js (from l:4, c:8)@. |}]

(* check that the locations are correct and that the lexer is captures all the token *)
let check_vs_string s toks =
  let rec space a b =
    if a >= b
    then ()
    else
      match s.[a] with
      | ' ' | '\n' | '\t' -> space (succ a) b
      | c -> Printf.printf "pos:%d, expecting space until %d, found %C\n" a b c
  in
  let text pos str =
    let strlen = String.length str in
    if strlen + pos > String.length s
    then
      Printf.printf
        "pos: %d, expecting %S, found %S\n"
        pos
        str
        (String.sub s ~pos ~len:(String.length s - pos))
    else
      let sub = String.sub s ~pos ~len:strlen in
      if String.equal str sub
      then ()
      else Printf.printf "pos: %d, expecting %S, found %S\n" pos str sub
  in
  let rec loop pos = function
    | [] -> space pos (String.length s)
    | Js_token.T_VIRTUAL_SEMICOLON _ :: rest -> loop pos rest
    | (Js_token.T_STRING (_, _, len) as x) :: rest ->
        let { Parse_info.idx; _ } = Js_token.info x in
        let _str = Js_token.to_string x in
        space pos idx;
        let quote_start = s.[idx] in
        let quote_end = s.[idx + len] in
        (match quote_start, quote_end with
        | '"', '"' | '\'', '\'' -> ()
        | a, b ->
            Printf.printf "pos:%d+%d, expecting quotes, found %C+%C\n" idx (idx + len) a b);
        loop (idx + len + 1) rest
    | x :: rest ->
        let { Parse_info.idx; _ } = Js_token.info x in
        let str = Js_token.to_string x in
        space pos idx;
        text idx str;
        loop (idx + String.length str) rest
  in
  loop 0 toks

let parse_print_token ?(extra = false) s =
  let lex = Parse_js.Lexer.of_lexbuf (Lexing.from_string s) in
  let _p, tokens =
    try Parse_js.parse' lex
    with Parse_js.Parsing_error pi as e ->
      Printf.eprintf "cannot parse l:%d:%d@." pi.Parse_info.line pi.Parse_info.col;
      raise e
  in
  check_vs_string s tokens;
  let prev = ref 0 in
  let rec loop tokens =
    match tokens with
    | [ Js_token.EOF _ ] | [] -> ()
    | tok :: xs ->
        let s = if extra then Js_token.to_string_extra tok else Js_token.to_string tok in
        let pos = Js_token.info tok in
        (match !prev <> pos.Parse_info.line && pos.Parse_info.line <> 0 with
        | true -> Printf.printf "\n%2d: " pos.Parse_info.line
        | false -> ());
        if pos.Parse_info.line <> 0 then prev := pos.Parse_info.line;
        Printf.printf "%d:%s, " pos.Parse_info.col s;
        loop xs
  in
  loop tokens

let%expect_test "tokens" =
  parse_print_token {|
    var a = 42;
|};
  [%expect {| 2: 4:var, 8:a, 10:=, 12:42, 14:;, |}]

let%expect_test "multiline string" =
  parse_print_token {|
    42;
    "
    ";
    42
|};
  [%expect
    {|
    LEXER: WEIRD newline in quoted string

     2: 4:42, 6:;,
     3: 4:"\n    ",
     4: 5:;,
     5: 4:42, 0:;, |}];
  parse_print_token {|
    42;
    "\
    ";
    42
|};
  [%expect {|
    2: 4:42, 6:;,
    3: 4:"    ",
    4: 5:;,
    5: 4:42, 0:;, |}];
  parse_print_token {|
    42;
    "

    ";
    42
|};
  [%expect
    {|
    LEXER: WEIRD newline in quoted string
    LEXER: WEIRD newline in quoted string

     2: 4:42, 6:;,
     3: 4:"\n\n    ",
     5: 5:;,
     6: 4:42, 0:;, |}];
  [%expect {| |}]

let%expect_test "multiline comments" =
  parse_print_token {|
//com1
//com2
//com3
|};
  [%expect {|
    2: 0://com1,
    3: 0://com2,
    4: 0://com3, |}];
  parse_print_token {|
/* test */ 42 /* test */
|};
  [%expect {|
    2: 0:/* test */, 11:42, 14:/* test */, 0:;, |}];
  parse_print_token {|
    42
    /*
    "

    */
    42
|};
  [%expect {|
    2: 4:42,
    3: 4:/*
       "

       */, 0:;,
    7: 4:42, 0:;, |}]

let%expect_test "++--" =
  parse_print_token ~extra:true {|
    ++a
    --a
    a++
    a++
|};
  [%expect
    {|
    2: 4:++ (INCR), 6:a (identifier), 0:; (virtual),
    3: 4:-- (DECR), 6:a (identifier), 0:; (virtual),
    4: 4:a (identifier), 5:++ (INCR_NB), 0:; (virtual),
    5: 4:a (identifier), 5:++ (INCR_NB), 0:; (virtual), |}]

let%expect_test "div_or_regexp" =
  parse_print_token
    {|
    1 / 2
    1 + /regexp/
    if(a) { e } /regexp/
    +{ } / denominator
    +{ } / denominator[a]
    if(b) /regexp/
    (b) / denominator
|};
  [%expect
    {|
    2: 4:1, 6:/, 8:2, 0:;,
    3: 4:1, 6:+, 8:/regexp/, 0:;,
    4: 4:if, 6:(, 7:a, 8:), 10:{, 12:e, 0:;, 14:}, 16:/regexp/,
    5: 4:+, 5:{, 7:}, 9:/, 11:denominator,
    6: 4:+, 5:{, 7:}, 9:/, 11:denominator, 22:[, 23:a, 24:], 0:;,
    7: 4:if, 6:(, 7:b, 8:), 10:/regexp/,
    8: 4:(, 5:b, 6:), 8:/, 10:denominator, 0:;, |}]

let%expect_test "virtual semicolon" =
  parse_print_token
    ~extra:true
    {|
    return;
    return 2
    return
    2

    continue;
    continue a
    continue
    a

    break;
    break a
    break
    a

    throw 2;
    throw 2

    { 1
    2 } 3

    a = b
    ++c

    a = b + c
    (d + e).print()

|};
  [%expect
    {|
     2: 4:return, 10:;,
     3: 4:return, 11:2, 0:; (virtual),
     4: 4:return, 0:; (virtual),
     5: 4:2, 0:; (virtual),
     7: 4:continue, 12:;,
     8: 4:continue, 13:a (identifier), 0:; (virtual),
     9: 4:continue, 0:; (virtual),
    10: 4:a (identifier), 0:; (virtual),
    12: 4:break, 9:;,
    13: 4:break, 10:a (identifier), 0:; (virtual),
    14: 4:break, 0:; (virtual),
    15: 4:a (identifier), 0:; (virtual),
    17: 4:throw, 10:2, 11:;,
    18: 4:throw, 10:2, 0:; (virtual),
    20: 4:{, 6:1, 0:; (virtual),
    21: 4:2, 0:; (virtual), 6:}, 8:3, 0:; (virtual),
    23: 4:a (identifier), 6:=, 8:b (identifier), 0:; (virtual),
    24: 4:++ (INCR), 6:c (identifier), 0:; (virtual),
    26: 4:a (identifier), 6:=, 8:b (identifier), 10:+, 12:c (identifier),
    27: 4:(, 5:d (identifier), 7:+, 9:e (identifier), 10:), 11:., 12:print (identifier), 17:(, 18:), 0:; (virtual), |}]

let%expect_test _ =
  parse_print_token
    ~extra:true
    {|
function UnexpectedVirtualElement(data) {
    var err = new Error();

    err.type = 'virtual-hyperscript.unexpected.virtual-element';
    err.message =
        'The parent vnode is:\n' +
        errorString(data.parentVnode)
        '\n' +
        'Suggested fix: change your `h(..., [ ... ])` callsite.';
    err.foreignObject = data.foreignObject;
    err.parentVnode = data.parentVnode;

    return err;
}
|};
  [%expect
    {|
     2: 0:function, 9:UnexpectedVirtualElement (identifier), 33:(, 34:data (identifier), 38:), 40:{,
     3: 4:var, 8:err (identifier), 12:=, 14:new, 18:Error (identifier), 23:(, 24:), 25:;,
     5: 4:err (identifier), 7:., 8:type (identifier), 13:=, 15:"virtual-hyperscript.unexpected.virtual-element", 63:;,
     6: 4:err (identifier), 7:., 8:message (identifier), 16:=,
     7: 8:"The parent vnode is:\\n", 33:+,
     8: 8:errorString (identifier), 19:(, 20:data (identifier), 24:., 25:parentVnode (identifier), 36:), 0:; (virtual),
     9: 8:"\\n", 13:+,
    10: 8:"Suggested fix: change your `h(..., [ ... ])` callsite.", 64:;,
    11: 4:err (identifier), 7:., 8:foreignObject (identifier), 22:=, 24:data (identifier), 28:., 29:foreignObject (identifier), 42:;,
    12: 4:err (identifier), 7:., 8:parentVnode (identifier), 20:=, 22:data (identifier), 26:., 27:parentVnode (identifier), 38:;,
    14: 4:return, 11:err (identifier), 14:;,
    15: 0:}, |}]

let%expect_test "annot" =
  parse_print_token
    ~extra:true
    {|
a
//Provides: test
//Just a comment
//Requires: something
//Another comment
a
if (a) {
//Provides: test
b
}
//Provides: test
c
|};
  [%expect
    {|
     2: 0:a (identifier), 0:; (virtual),
     3: 0://Provides: test(annot),
     4: 0://Just a comment,
     5: 0://Requires: something(annot),
     6: 0://Another comment,
     7: 0:a (identifier), 0:; (virtual),
     8: 0:if, 3:(, 4:a (identifier), 5:), 7:{,
     9: 0://Provides: test,
    10: 0:b (identifier), 0:; (virtual),
    11: 0:},
    12: 0://Provides: test(annot),
    13: 0:c (identifier), 0:; (virtual), |}]
