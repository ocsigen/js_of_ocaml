(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2026 Hugo Heuzard
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
open Js_of_ocaml_compiler

let process ?(coalesce = true) js_prog =
  Config.Flag.set "shortvar" false;
  let lex = Parse_js.Lexer.of_string js_prog in
  let p = Parse_js.parse `Script lex in
  let p = (new Js_traverse.rename_variable ~esm:false)#program p in
  let p = if coalesce then Js_variable_coalescing.f p else p in
  let p = (new Js_traverse.simpl)#program p in
  let p = (new Js_traverse.clean)#program p in
  let p = Js_assign.program p in
  let buffer = Buffer.create 256 in
  let pp = Pretty_print.to_buffer buffer in
  Pretty_print.set_compact pp false;
  let (_ : Source_map.info) = Js_output.program pp p in
  Buffer.contents buffer

let test ?coalesce js_prog = process ?coalesce js_prog |> print_string

let test_run ?coalesce js_prog =
  with_temp_dir ~f:(fun () ->
      let s = process ?coalesce js_prog in
      let file = Filetype.js_text_of_string s |> Filetype.write_js ~name:"test.js" in
      run_javascript file |> print_string)

let%expect_test "disjoint lifespans — basic coalescing" =
  test
    {|
(function(x) {
  var a = x + 1;
  console.log(a);
  var b = x + 2;
  console.log(b);
})(0)
|};
  [%expect
    {| (function(b){var a = b + 1; console.log(a); b = b + 2; console.log(b);}(0)); |}]

let%expect_test "disjoint lifespans — runtime correctness" =
  test_run
    {|
(function(x) {
  var a = x + 1;
  console.log(a);
  var b = x + 2;
  console.log(b);
})(0)
|};
  [%expect {|
    1
    2
    |}]

let%expect_test "overlapping lifespans — no coalescing" =
  test {|
(function(x) {
  var a = x + 1;
  var b = x + 2;
  console.log(a + b);
})(0)
|};
  [%expect {| (function(b){var a = b + 1, b = b + 2; console.log(a + b);}(0)); |}]

let%expect_test "overlapping lifespans — runtime correctness" =
  test_run
    {|
(function(x) {
  var a = x + 1;
  var b = x + 2;
  console.log(a + b);
})(0)
|};
  [%expect {| 3 |}]

let%expect_test "copy hint elimination" =
  test {|
(function(x) {
  var a = x + 1;
  var b = a;
  console.log(b);
})(0)
|};
  [%expect {| (function(a){a = a + 1; console.log(a);}(0)); |}]

let%expect_test "copy hint elimination — runtime correctness" =
  test_run {|
(function(x) {
  var a = x + 1;
  var b = a;
  console.log(b);
})(0)
|};
  [%expect {| 1 |}]

let%expect_test "try-catch — variables live across exception boundary" =
  test
    {|
(function() {
  var x = 1;
  var r = 0;
  try {
    r = x;
    x = 2;
    if (x > 0) throw 0;
    x = 3;
  } catch(e) {
    r = x;
  }
  console.log(r);
})()
|};
  [%expect
    {|
    (function(){
       var x = 1, r = 0;
       try{r = x; x = 2; if(x > 0) throw 0; x = 3;}catch(e){r = x;}
       console.log(r);
      }
      ());
    |}]

let%expect_test "try-catch — runtime correctness" =
  test_run
    {|
(function() {
  var x = 1;
  var r = 0;
  try {
    r = x;
    x = 2;
    if (x > 0) throw 0;
    x = 3;
  } catch(e) {
    r = x;
  }
  console.log(r);
})()
|};
  [%expect {| 2 |}]

let%expect_test "loop with variable reuse across iterations" =
  test
    {|
(function(n) {
  var sum = 0;
  for (var i = 0; i < n; i++) {
    var x = i * 10;
    sum = sum + x;
    var y = i * 100;
    sum = sum + y;
  }
  console.log(sum);
})(4)
|};
  [%expect
    {|
    (function(n){
       var sum = 0;
       for(var i = 0; i < n; i++){
        var a = i * 10;
        sum = sum + a;
        a = i * 100;
        sum = sum + a;
       }
       console.log(sum);
      }
      (4));
    |}]

let%expect_test "loop with variable reuse — runtime correctness" =
  test_run
    {|
(function(n) {
  var sum = 0;
  for (var i = 0; i < n; i++) {
    var x = i * 10;
    sum = sum + x;
    var y = i * 100;
    sum = sum + y;
  }
  console.log(sum);
})(4)
|};
  [%expect {| 660 |}]

let%expect_test "labeled break" =
  test
    {|
(function(x) {
  var a, b, r;
  outer:
  {
    a = x + 1;
    if (a > 5) { r = a; break outer; }
    b = x + 2;
    r = b;
  }
  console.log(r);
})(10)
|};
  [%expect
    {|
    (function(x){
       var a;
       a:
       {a = x + 1; if(a > 5) break a; a = x + 2;}
       console.log(a);
      }
      (10));
    |}]

let%expect_test "labeled break — runtime correctness" =
  test_run
    {|
(function(x) {
  var a, b, r;
  outer:
  {
    a = x + 1;
    if (a > 5) { r = a; break outer; }
    b = x + 2;
    r = b;
  }
  console.log(r);
})(10)
|};
  [%expect {| 11 |}]

let%expect_test "switch cases" =
  test
    {|
(function(x) {
  var r;
  switch(x) {
    case 0: var a = 10; r = a + 1; break;
    case 1: var b = 20; r = b + 2; break;
    case 2: var c = 30; r = c + 3; break;
    default: var d = 40; r = d + 4; break;
  }
  console.log(r);
})(1)
|};
  [%expect
    {|
    (function(a){
       switch(a){
         case 0:
          a = 10; a = a + 1; break;
         case 1:
          a = 20; a = a + 2; break;
         case 2:
          a = 30; a = a + 3; break;
         default: a = 40; a = a + 4; break;
       }
       console.log(a);
      }
      (1));
    |}]

let%expect_test "switch cases — runtime correctness" =
  test_run
    {|
(function(x) {
  var r;
  switch(x) {
    case 0: var a = 10; r = a + 1; break;
    case 1: var b = 20; r = b + 2; break;
    case 2: var c = 30; r = c + 3; break;
    default: var d = 40; r = d + 4; break;
  }
  console.log(r);
})(1)
|};
  [%expect {| 22 |}]

let%expect_test "captured variable exclusion" =
  test
    {|
(function(x) {
  var a = x + 1;
  var f = function() { return a; };
  var b = x + 2;
  console.log(f() + b);
})(10)
|};
  [%expect
    {|
    (function(b){
       var a = b + 1;
       function f(){return a;}
       var b = b + 2;
       console.log(f() + b);
      }
      (10));
    |}]

let%expect_test "captured variable exclusion — runtime correctness" =
  test_run
    {|
(function(x) {
  var a = x + 1;
  var f = function() { return a; };
  var b = x + 2;
  console.log(f() + b);
})(10)
|};
  [%expect {| 23 |}]

let%expect_test "coalescing enabled vs disabled" =
  let prog =
    {|
(function(x) {
  var a = x + 1;
  console.log(a);
  var b = x + 2;
  console.log(b);
})(0)
|}
  in
  Printf.printf "--- without coalescing ---\n";
  test ~coalesce:false prog;
  Printf.printf "--- with coalescing ---\n";
  test ~coalesce:true prog;
  [%expect
    {|
    --- without coalescing ---
    (function(x){var a = x + 1; console.log(a); var b = x + 2; console.log(b);}(0));
    --- with coalescing ---
    (function(b){var a = b + 1; console.log(a); b = b + 2; console.log(b);}(0));
    |}]
