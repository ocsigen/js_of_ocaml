(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
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

open Util

let%expect_test "bool in conditional (should omit ? 1 : 0)" =
  let program =
    compile_and_parse
      {|
      let f (a : int) (b : int) =
        if a < b then 1 else 2
      |}
  in
  print_fun_decl program (Some "f");
  [%expect {|
    function f(a, b){return a < b ? 1 : 2;}
    //end
    |}]

let%expect_test "bool used as integer (must keep ? 1 : 0)" =
  let program =
    compile_and_parse
      {|
      let f (a : int) (b : int) =
        (if a < b then 1 else 0) + 1
      |}
  in
  print_fun_decl program (Some "f");
  [%expect
    {|
    function f(a, b){var _a_ = a < b ? 1 : 0; return _a_ + 1 | 0;}
    //end
    |}]

let%expect_test "bool returned (must keep ? 1 : 0)" =
  let program = compile_and_parse {|
      let f (a : int) (b : int) = a < b
      |} in
  print_fun_decl program (Some "f");
  [%expect {|
    function f(a, b){return a < b ? 1 : 0;}
    //end
    |}]

let%expect_test "bool through Not in conditional (should omit ? 1 : 0)" =
  let program =
    compile_and_parse
      {|
      let f (a : int) (b : int) =
        if not (a < b) then 1 else 2
      |}
  in
  print_fun_decl program (Some "f");
  [%expect {|
    function f(a, b){return a < b ? 2 : 1;}
    //end
    |}]

let%expect_test "bool through phi in conditional (should omit ? 1 : 0)" =
  let program =
    compile_and_parse
      {|
      let f (a : int) (b : int) (c : bool) =
        let x = if c then a < b else a > b in
        if x then 1 else 2
      |}
  in
  print_fun_decl program (Some "f");
  [%expect
    {|
    function f(a, b, c){var x = c ? a < b : b < a; return x ? 1 : 2;}
    //end
    |}]
