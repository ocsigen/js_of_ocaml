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

(* Simplifications of the generated JavaScript code *)

open Util
open Js_of_ocaml_compiler

(* Labelled blocks exited by a single conditional [break] *)

let%expect_test "labelled block, single break" =
  let p =
    {|
let f a x =
  let v = if Array.length a > 0 && a.(0) = x then a.(0) + 1 else invalid_arg "f" in
  v * 2
|}
  in
  let p = compile_and_parse p in
  print_fun_decl p (Some "f");
  [%expect
    {|
           function f(a, x){
            var
             v =
               0 < a.length - 1 && caml_check_bound(a, 0)[1] === x
                ? caml_check_bound(a, 0)[1] + 1 | 0
                : caml_call1(Stdlib[1], cst_f);
            return v * 2 | 0;
           }
           //end
           |}]

let%expect_test "labelled block, several breaks" =
  let p =
    {|
let f a x y =
  let v =
    if Array.length a > 0 && a.(0) = x then a.(0) + 1
    else if Array.length a > 1 && a.(1) = y then a.(1) + 2
    else invalid_arg "f"
  in
  v * 2
|}
  in
  let p = compile_and_parse p in
  print_fun_decl p (Some "f");
  [%expect
    {|
           function f(a, x, y){
            a:
            {
             if(0 < a.length - 1 && caml_check_bound(a, 0)[1] === x){var v = caml_check_bound(a, 0)[1] + 1 | 0; break a;}
             if(1 < a.length - 1 && caml_check_bound(a, 1)[2] === y){v = caml_check_bound(a, 1)[2] + 2 | 0; break a;}
             v = caml_call1(Stdlib[1], cst_f);
            }
            return v * 2 | 0;
           }
           //end
           |}]

(* [not] whose result is only used as a condition *)

let%expect_test "not" =
  let p =
    {|
let f mem l x =
  if not (mem x l) then print_endline "absent";
  print_endline "done"
let g (a : int) b =
  if not (a = b) then print_endline "different";
  print_endline "done"
let h x = not x
|}
  in
  let p = compile_and_parse p in
  print_fun_decl p (Some "f");
  print_fun_decl p (Some "g");
  print_fun_decl p (Some "h");
  [%expect
    {|
           function f(mem, l, x){
            if(! caml_call2(mem, x, l)) caml_call1(Stdlib[46], cst_absent);
            return caml_call1(Stdlib[46], cst_done);
           }
           //end
           function g(a, b){
            if(a !== b) caml_call1(Stdlib[46], cst_different);
            return caml_call1(Stdlib[46], cst_done$0);
           }
           //end
           function h(x){return 1 - x;}
           //end
           |}]

(* Peephole simplifications on JavaScript code *)

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

let%expect_test "empty branches" =
  process
    {|
function f(a, b) {
  if (a());
  if (b);
  if (a()); else b();
  if (a() === b); else b();
  if (a() < b); else b();
  if (a() && b); else b();
  if (a() ? 1 : 0); else b();
}
|};
  [%expect
    {|
    function f(a, b){
     a();
     if(! a()) b();
     if(a() !== b) b();
     if(! (a() < b)) b();
     if(! (a() && b)) b();
     if(! a()) b();
    }
    |}]

let%expect_test "conditional expressions" =
  process
    {|
function f(a, b, c) {
  var x = a ? b() : a;
  var y = a ? a : b();
  var z = !(a === b), t = !(a != c), u = !(a < b);
  return [x, y, z, t, u];
}
|};
  [%expect
    {|
    function f(a, b, c){
     var x = a && b(), y = a || b(), z = a !== b, t = a == c, u = ! (a < b);
     return [x, y, z, t, u];
    }
    |}]
