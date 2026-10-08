(* Js_of_ocaml compiler
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
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

(* Simplifications of JavaScript code by Js_traverse.simpl and clean *)

open Js_of_ocaml_compiler

let process js_prog =
  Config.Flag.set "shortvar" false;
  let lex = Parse_js.Lexer.of_string js_prog in
  let p = Parse_js.parse `Script lex in
  let p = (new Js_traverse.rename_variable ~esm:false)#program p in
  let p = (new Js_traverse.simpl)#program p in
  let p = (new Js_traverse.clean)#program p in
  let p = Js_assign.program p in
  let buffer = Buffer.create 256 in
  let pp = Pretty_print.to_buffer buffer in
  Pretty_print.set_compact pp false;
  let (_ : Source_map.info) = Js_output.program pp p in
  print_string (Buffer.contents buffer)

let%expect_test "no ES2021 logical assignments" =
  process
    {|
function f(a, b, c) {
  a = a || c();
  b = b && c();
  c = c ?? a;
  return [a, b, c];
}
|};
  [%expect
    {| function f(a, b, c){a = a || c(); b = b && c(); c = c ?? a; return [a, b, c];} |}]

let%expect_test "blocks around block-scoped declarations" =
  process
    {|
"use strict";
function f(c, x) {
  if (c) {} else { let z = x(); }
  if (c) { const z = x(); }
  if (c) { function g() {} }
  while (c) { class C {} }
  { let y = 1; }
  let y = 2;
  return y;
}
|};
  [%expect
    {|
           "use strict";
           function f(c, x){
            if(c) ; else{let z = x();}
            if(c){const z = x();}
            if(c){function g(){}}
            while(c){class C{}}
            {let y = 1;}
            let y = 2;
            return y;
           }
           |}]
