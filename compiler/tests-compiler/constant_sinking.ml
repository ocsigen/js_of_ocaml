(* Js_of_ocaml tests
 * http://www.ocsigen.org/js_of_ocaml/
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

let print_program program =
  let buffer = Buffer.create 256 in
  let pp = Pretty_print.to_buffer buffer in
  Pretty_print.set_compact pp false;
  let _ = Js_output.program pp program in
  print_string (Buffer.contents buffer)

let parse_and_sink source =
  Config.Flag.set "shortvar" false;
  let lexed = Parse_js.Lexer.of_string ~filename:"test" source in
  let parsed = Parse_js.parse `Script lexed in
  let renamed = (new Js_traverse.rename_variable ~esm:false)#program parsed in
  let sunk = Js_constant_sinking.f renamed in
  try
    Debug.disable "js_assign";
    let assigned = Js_assign.program sunk in
    print_program assigned
  with Assert_failure _ -> (
    print_endline "FIXME: assert_failure";
    flush_all ();
    Debug.enable "js_assign";
    try ignore (Js_assign.program sunk) with _ -> ())

(* When the if/while body is a block, insertions go through apply_insertions
   (via self#block) which correctly calls self#expression on the init.
   These tests verify the basic sinking behavior works. *)
let%expect_test "sunk into block body (if)" =
  parse_and_sink
    {|
    function f(c) {
      var a = 42;
      var b = a;
      if (c) {
        console.log(b);
        console.log(b);
      }
    }
    |};
  [%expect {| function f(c){; ; if(c){var b = 42; console.log(b); console.log(b);}} |}]

let%expect_test "sunk into block body (while)" =
  parse_and_sink
    {|
    function f(arr) {
      var a = 42;
      var b = a;
      var i = 0;
      while (i < arr.length) {
        arr[i] = b;
        console.log(b);
        i++;
      }
    }
    |};
  [%expect
    {|
    function f(arr){
     ;
     ;
     var i = 0;
     while(i < arr.length){var b = 42; arr[i] = b; console.log(b); i++;}
    }
    |}]

(* When the body is a single statement (not a block), insertions go
   through handle_scoped_body which calls self#expression on the init. *)
let%expect_test "sunk into non-block if body" =
  parse_and_sink
    {|
    function f(c) {
      var a = 42;
      var b = a;
      if (c) console.log(b, b);
    }
    |};
  [%expect {| function f(c){; ; if(c){var b = 42; console.log(b, b);}} |}]

let%expect_test "sunk into non-block while body" =
  parse_and_sink
    {|
    function f() {
      var a = 42;
      var b = a;
      while (true) console.log(b, b);
    }
    |};
  [%expect {| function f(){; ; while(true){var b = 42; console.log(b, b);}} |}]

(* Chained inlining: a -> b -> c, where c is sunk into non-block body *)
let%expect_test "chained inlining sunk into non-block body" =
  parse_and_sink
    {|
    function f(c) {
      var a = 1;
      var b = a;
      var d = b;
      if (c) console.log(d, d);
    }
    |};
  [%expect {| function f(c){; ; ; if(c){var d = 1; console.log(d, d);}} |}]

(* Allocations (arrays, objects) can be sunk into block scopes like if-branches *)
let%expect_test "allocation sunk into if block" =
  parse_and_sink
    {|
    function f(c) {
      var a = [1, 2, 3];
      if (c) {
        console.log(a);
        console.log(a);
      }
    }
    |};
  [%expect
    {| function f(c){; if(c){var a = [1, 2, 3]; console.log(a); console.log(a);}} |}]

(* Same but with a non-block if body — goes through handle_scoped_body *)
let%expect_test "allocation sunk into non-block if body" =
  parse_and_sink
    {|
    function f(c) {
      var a = [1, 2, 3];
      if (c) console.log(a, a);
    }
    |};
  [%expect {| function f(c){; if(c){var a = [1, 2, 3]; console.log(a, a);}} |}]

(* Allocations must NOT be sunk into a loop body - would create
   multiple allocations where there was originally one *)
let%expect_test "allocation not sunk into loop" =
  parse_and_sink
    {|
    function f() {
      var a = [1, 2, 3];
      var i = 0;
      while (i < 10) {
        console.log(a);
        console.log(a);
        i++;
      }
    }
    |};
  [%expect
    {|
    function f(){
     ;
     var i = 0;
     var a = [1, 2, 3];
     while(i < 10){console.log(a); console.log(a); i++;}
    }
    |}]

(* Allocations must NOT be sunk into a function body *)
let%expect_test "allocation not sunk into function" =
  parse_and_sink
    {|
    function f() {
      var a = [1, 2, 3];
      function g() {
        console.log(a);
        console.log(a);
      }
      g();
    }
    |};
  [%expect
    {|
    function f(){
     ;
     var a = [1, 2, 3];
     function g(){console.log(a); console.log(a);}
     g();
    }
    |}]

(* Allocations must NOT be sunk into for await body *)
let%expect_test "allocation not sunk into for-await-of loop" =
  parse_and_sink
    {|
    async function f(iter) {
      var a = [1, 2, 3];
      for await (var x of iter) {
        console.log(a);
        console.log(a);
      }
    }
    |};
  [%expect
    {|
    async function f(iter){
     ;
     var a = [1, 2, 3];
     for await(var x of iter){console.log(a); console.log(a);}
    }
    |}]

(* Primitives CAN be sunk into loops *)
let%expect_test "primitive sunk into loop" =
  parse_and_sink
    {|
    function f() {
      var a = 42;
      var i = 0;
      while (i < 10) {
        console.log(a);
        console.log(a);
        i++;
      }
    }
    |};
  [%expect
    {|
    function f(){
     ;
     var i = 0;
     while(i < 10){var a = 42; console.log(a); console.log(a); i++;}
    }
    |}]

(* Primitives can sink into non-block if/loop bodies via handle_scoped_body *)
let%expect_test "primitive sunk into non-block if body" =
  parse_and_sink
    {|
    function f(c) {
      var a = 42;
      if (c) console.log(a, a);
    }
    |};
  [%expect {| function f(c){; if(c){var a = 42; console.log(a, a);}} |}]

let%expect_test "primitive sunk into non-block while body" =
  parse_and_sink
    {|
    function f() {
      var a = 42;
      var i = 0;
      while (i < 10) console.log(a, a, i++);
    }
    |};
  [%expect
    {| function f(){; var i = 0; while(i < 10){var a = 42; console.log(a, a, i++);}} |}]

(* Allocations must NOT be sunk into object methods or class methods *)
let%expect_test "allocation not sunk into object method" =
  parse_and_sink
    {|
    function f() {
      var a = [1, 2, 3];
      return {
        m: function() {
          console.log(a);
          console.log(a);
        }
      };
    }
    |};
  [%expect
    {|
    function f(){
     ;
     var a = [1, 2, 3];
     return {m: function(){console.log(a); console.log(a);}};
    }
    |}]

let%expect_test "allocation not sunk into class method" =
  parse_and_sink
    {|
    function f() {
      var a = [1, 2, 3];
      class C {
        m() {
          console.log(a);
          console.log(a);
        }
      }
      return new C();
    }
    |};
  [%expect
    {|
    function f(){
     ;
     var a = [1, 2, 3];
     class C{m(){console.log(a); console.log(a);}}
     return new C();
    }
    |}]

(* Class field initializers and static blocks are implicit function scopes.
   The sinking pass doesn't override class_element, so these are not
   treated as function boundaries. *)
let%expect_test "allocation not sunk into class field initializer" =
  parse_and_sink
    {|
    function f() {
      var a = [1, 2, 3];
      class C {
        x = (console.log(a), console.log(a), 0);
      }
      return new C();
    }
    |};
  [%expect
    {|
    function f(){
     ;
     var a = [1, 2, 3];
     class C{x = (console.log(a), console.log(a), 0);}
     return new C();
    }
    |}]

let%expect_test "allocation not sunk into static block" =
  parse_and_sink
    {|
    function f() {
      var a = [1, 2, 3];
      class C {
        static {
          console.log(a);
          console.log(a);
        }
      }
      return C;
    }
    |};
  [%expect
    {|
    function f(){
     ;
     var a = [1, 2, 3];
     class C{static {console.log(a); console.log(a);}}
     return C;
    }
    |}]

let%expect_test "allocation not sunk into getter" =
  parse_and_sink
    {|
    function f() {
      var a = [1, 2, 3];
      return {
        get x() {
          console.log(a);
          console.log(a);
          return 0;
        }
      };
    }
    |};
  [%expect
    {|
    function f(){
     ;
     var a = [1, 2, 3];
     return {get x(){console.log(a); console.log(a); return 0;}};
    }
    |}]
