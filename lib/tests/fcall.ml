(* Js_of_ocaml tests
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2020 Hugo Heuzard
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
open Js_of_ocaml

let s x =
  let to_string =
    Js.Unsafe.eval_string
      {|
(function(x){
    if(x === null)
      return "null"
    if(x === undefined)
      return "undefined"
    if(typeof x === "function")
      return "function#" + x.length + "#" + x.l
    if(x.toString() == "[object Arguments]")
      return "(Arguments: " + Array.prototype.slice.call(x).toString() + ")";
    return x.toString()
})
|}
  in
  Js.to_string (Js.Unsafe.fun_call to_string [| Js.Unsafe.inject x |])

let call_and_log f ?(cont = (Obj.magic Fun.id : _ -> _)) str =
  let call = Js.Unsafe.eval_string str in
  let r = Js.Unsafe.fun_call call [| Js.Unsafe.inject f |] in
  Printf.printf "Result: %s" (s (cont r))

let cb1 a = Printf.printf "got %s, done\n" (s a)

let cb2 a b = Printf.printf "got %s, %s, done\n" (s a) (s b)

let cb3 a b c = Printf.printf "got %s, %s, %s, done\n" (s a) (s b) (s c)

let cb4 a b c d = Printf.printf "got %s, %s, %s, %s, done\n" (s a) (s b) (s c) (s d)

let cb5 a b c d e =
  Printf.printf "got %s, %s, %s, %s, %s, done\n" (s a) (s b) (s c) (s d) (s e)

(* Wrap callback *)

let%expect_test "over application, extra arguments are dropped" =
  call_and_log (Js.wrap_callback cb3) {| (function(f){ return f(1,2,3,4) }) |};
  [%expect {|
    got 1, 2, 3, done
    Result: 0 |}]

let%expect_test "over application, extra arguments are dropped" =
  call_and_log (Js.wrap_callback cb3) {| (function(f){ return f(1,2)(3,4) }) |};
  [%expect {|
    got 1, 2, 3, done
    Result: 0 |}]

let%expect_test "partial application 1 + 2" =
  call_and_log (Js.wrap_callback cb3) {| (function(f){ return f(1)(2,3) }) |};
  [%expect {|
    got 1, 2, 3, done
    Result: 0 |}]

let%expect_test "partial application 2 + 1" =
  call_and_log (Js.wrap_callback cb3) {| (function(f){ return f(1,2)(3) }) |};
  [%expect {|
    got 1, 2, 3, done
    Result: 0 |}]

let%expect_test "partial application, callback is called when all arguments are available"
    =
  call_and_log (Js.wrap_callback cb5) {| (function(f){ return f(1)(2)(3)(4)(5) }) |};
  [%expect {|
    got 1, 2, 3, 4, 5, done
    Result: 0 |}]

let%expect_test
    "partial application, 0 argument call is treated like 1 argument (undefined)" =
  call_and_log (Js.wrap_callback cb5) {| (function(f){ return f(1)()(3)()(5) }) |};
  [%expect {|
    got 1, undefined, 3, undefined, 5, done
    Result: 0 |}]

let%expect_test _ =
  let plus = Js.wrap_callback (fun a b -> a + b) in
  call_and_log plus {| (function(f){ return f(1) }) |};
  [%expect {| Result: function#0#undefined |}];
  call_and_log plus {| (function(f){ return f(1)(2) }) |};
  [%expect {| Result: 3 |}];
  call_and_log plus {| (function(f){ return f(1,2) }) |};
  [%expect {| Result: 3 |}];
  call_and_log plus {| (function(f){ return f(1,2,3) }) |};
  [%expect {| Result: 3 |}]

(* Wrap callback with argument *)

let%expect_test "wrap_callback_arguments" =
  call_and_log
    (Js.Unsafe.callback_with_arguments (Obj.magic cb1))
    {| (function(f){ return f(1,2,3,4,5) }) |};
  [%expect {|
    got 1,2,3,4,5, done
    Result: 0 |}]

let%expect_test "wrap_callback_arguments" =
  call_and_log
    (Js.Unsafe.callback_with_arguments (Obj.magic cb1))
    {| (function(f){ return f() }) |};
  [%expect {|
    got , done
    Result: 0 |}]

(* Wrap with arity *)

let%expect_test "wrap_callback_strict" =
  call_and_log
    (Js.Unsafe.callback_with_arity 3 cb3)
    {| (function(f){ return f(1,2,3) }) |};
  [%expect {|
    got 1, 2, 3, done
    Result: 0 |}];
  call_and_log
    (Js.Unsafe.callback_with_arity 3 cb3)
    {| (function(f){ return f(1,2,3,4) }) |};
  [%expect {|
    got 1, 2, 3, done
    Result: 0 |}];
  call_and_log (Js.Unsafe.callback_with_arity 3 cb3) {| (function(f){ return f(1,2) }) |};
  [%expect {|
    got 1, 2, undefined, done
    Result: 0 |}]

let%expect_test "wrap_callback_strict" =
  call_and_log
    (Js.Unsafe.callback_with_arity 2 cb3)
    {| (function(f){ return f(1,2,3) }) |};
  [%expect {|
    Result: function#1#1 |}];
  call_and_log
    (Js.Unsafe.callback_with_arity 2 cb3)
    ~cont:(fun g -> g 4)
    {| (function(f){ return f(1,2,3) }) |};
  [%expect {|
    got 1, 2, 4, done
    Result: 0 |}];
  call_and_log
    (Js.Unsafe.callback_with_arity 2 cb3)
    ~cont:(fun g -> g 3)
    {| (function(f){ return f(1,2) }) |};
  [%expect {|
    got 1, 2, 3, done
    Result: 0 |}];
  call_and_log (Js.Unsafe.callback_with_arity 2 cb3) {| (function(f){ return f(1,2) }) |};
  [%expect {|
    Result: function#1#1 |}]

let%expect_test "wrap_callback_strict" =
  call_and_log
    (Js.Unsafe.callback_with_arity 4 cb3)
    {| (function(f){ return f(1,2,3) }) |};
  [%expect {|
    got 1, 2, 3, done
    Result: 0 |}];
  call_and_log
    (Js.Unsafe.callback_with_arity 4 cb3)
    {| (function(f){ return f(1,2,3,4) }) |};
  [%expect {|
    got 1, 2, 3, done
    Result: 0 |}];
  call_and_log (Js.Unsafe.callback_with_arity 4 cb3) {| (function(f){ return f(1,2) }) |};
  [%expect {|
    got 1, 2, undefined, done
    Result: 0 |}]

(* Wrap meth callback *)

let%expect_test "over application, extra arguments are dropped" =
  call_and_log
    (Js.wrap_meth_callback cb4)
    {| (function(f){ return f.apply("this",[1,2,3,4]) }) |};
  [%expect {|
    got this, 1, 2, 3, done
    Result: 0 |}]

let%expect_test "over application, extra arguments are dropped" =
  call_and_log
    (Js.wrap_meth_callback cb4)
    {| (function(f){ return f.apply("this",[1,2])(3,4) }) |};
  [%expect {|
    got this, 1, 2, 3, done
    Result: 0 |}]

let%expect_test "partial application 1 + 2" =
  call_and_log
    (Js.wrap_meth_callback cb4)
    {| (function(f){ return f.apply("this", [1])(2,3) }) |};
  [%expect {|
    got this, 1, 2, 3, done
    Result: 0 |}]

let%expect_test "partial application 2 + 1" =
  call_and_log
    (Js.wrap_meth_callback cb4)
    {| (function(f){ return f.apply("this",[1,2])(3) }) |};
  [%expect {|
    got this, 1, 2, 3, done
    Result: 0 |}]

let%expect_test "partial application, callback is called when all arguments are available"
    =
  call_and_log
    (Js.wrap_meth_callback cb5)
    {| (function(f){ return f.apply("this",[])(1)(2)(3)(4) }) |};
  [%expect {|
    got this, 1, 2, 3, 4, done
    Result: 0 |}]

let%expect_test "partial application, 0 argument call is treated 1 argument (undefined)" =
  call_and_log
    (Js.wrap_meth_callback cb5)
    {| (function(f){ return f.apply("this",[])(1)()(3)() }) |};
  [%expect {|
    got this, 1, undefined, 3, undefined, done
    Result: 0 |}]

let%expect_test _ =
  let plus = Js.wrap_meth_callback (fun _ a b -> a + b) in
  call_and_log plus {| (function(f){ return f(1) }) |};
  [%expect {| Result: function#0#undefined |}];
  call_and_log plus {| (function(f){ return f(1)(2) }) |};
  [%expect {| Result: 3 |}];
  call_and_log plus {| (function(f){ return f(1,2) }) |};
  [%expect {| Result: 3 |}];
  call_and_log plus {| (function(f){ return f(1,2,3) }) |};
  [%expect {| Result: 3 |}]

(* Wrap callback with argument *)

let%expect_test "wrap_meth_callback_arguments" =
  call_and_log
    (Js.Unsafe.meth_callback_with_arguments (Obj.magic cb2))
    {| (function(f){ return f.apply("this",[1,2,3,4,5]) }) |};
  [%expect {|
    got this, 1,2,3,4,5, done
    Result: 0 |}]

let%expect_test "wrap_meth_callback_arguments" =
  call_and_log
    (Js.Unsafe.meth_callback_with_arguments (Obj.magic cb2))
    {| (function(f){ return f.apply("this", []) }) |};
  [%expect {|
    got this, , done
    Result: 0 |}]

(* Wrap with arity *)

let%expect_test "wrap_meth_callback_strict" =
  call_and_log
    (Js.Unsafe.meth_callback_with_arity 3 cb4)
    {| (function(f){ return f.apply("this",[1,2,3]) }) |};
  [%expect {|
    got this, 1, 2, 3, done
    Result: 0 |}];
  call_and_log
    (Js.Unsafe.meth_callback_with_arity 3 cb4)
    {| (function(f){ return f.apply("this",[1,2,3,4]) }) |};
  [%expect {|
    got this, 1, 2, 3, done
    Result: 0 |}];
  call_and_log
    (Js.Unsafe.meth_callback_with_arity 3 cb4)
    {| (function(f){ return f.apply("this",[1,2]) }) |};
  [%expect {|
    got this, 1, 2, undefined, done
    Result: 0 |}]

let%expect_test "wrap_meth_callback_strict" =
  call_and_log
    (Js.Unsafe.meth_callback_with_arity 2 cb4)
    {| (function(f){ return f.apply("this",[1,2,3]) }) |};
  [%expect {|
    Result: function#1#1 |}];
  call_and_log
    (Js.Unsafe.meth_callback_with_arity 2 cb4)
    ~cont:(fun g -> g 4)
    {| (function(f){ return f.apply("this",[1,2,3]) }) |};
  [%expect {|
    got this, 1, 2, 4, done
    Result: 0 |}];
  call_and_log
    (Js.Unsafe.meth_callback_with_arity 2 cb4)
    ~cont:(fun g -> g 3)
    {| (function(f){ return f.apply("this",[1,2]) }) |};
  [%expect {|
    got this, 1, 2, 3, done
    Result: 0 |}];
  call_and_log
    (Js.Unsafe.meth_callback_with_arity 2 cb4)
    {| (function(f){ return f.apply("this",[1,2]) }) |};
  [%expect {| Result: function#1#1 |}]

let%expect_test "wrap_meth_callback_strict" =
  call_and_log
    (Js.Unsafe.meth_callback_with_arity 4 cb4)
    {| (function(f){ return f.apply("this",[1,2,3]) }) |};
  [%expect {|
    got this, 1, 2, 3, done
    Result: 0 |}];
  call_and_log
    (Js.Unsafe.meth_callback_with_arity 4 cb4)
    {| (function(f){ return f.apply("this",[1,2,3,4]) }) |};
  (* Should not return a function *)
  [%expect {|
    got this, 1, 2, 3, done
    Result: 0 |}];
  call_and_log
    (Js.Unsafe.meth_callback_with_arity 4 cb4)
    {| (function(f){ return f.apply("this",[1,2]) }) |};
  [%expect {|
    got this, 1, 2, undefined, done
    Result: 0 |}]

(* Wrap meth callback unsafe *)
let%expect_test "over application, extra arguments are dropped" =
  call_and_log
    (Js.Unsafe.meth_callback cb4)
    {| (function(f){ return f.apply("this",[1,2,3,4]) }) |};
  [%expect {|
    got this, 1, 2, 3, done
    Result: 0 |}]

let%expect_test "partial application, extra arguments set to undefined" =
  call_and_log
    (Js.Unsafe.meth_callback cb4)
    {| (function(f){ return f.apply("this",[1,2]) }) |};
  [%expect {|
    got this, 1, 2, undefined, done
    Result: 0 |}]

(* caml_call_gen *)

let%expect_test _ =
  call_and_log cb3 ~cont:(fun g -> g 1) {| (function(f){ return f }) |};
  [%expect {|
    Result: function#2#2 |}]

let%expect_test _ =
  call_and_log cb3 ~cont:(fun g -> g 1 2 3 4) {| (function(f){ return f }) |};
  [%expect {|
    got 1, 2, 3, done
    Result: 0 |}]

let%expect_test _ =
  let f cb =
    try call_and_log (cb 1) ~cont:(fun g -> g 1 2 3) {| (function(f){ return f }) |} with
    | Invalid_argument s | Failure s -> Printf.printf "Error: %s" s
    | _ -> Printf.printf "Error: unknown"
  in
  f cb5;
  [%expect {| Result: function#1#1 |}];
  f cb4;
  [%expect {|
    got 1, 1, 2, 3, done
    Result: 0 |}];
  f cb3;
  [%expect {|
    got 1, 1, 2, done
    Result: 0 |}]

let%expect_test _ =
  let f cb =
    try call_and_log (cb 1 2 3) {| (function(f){ return f }) |} with
    | Invalid_argument s | Failure s -> Printf.printf "Error: %s" s
    | _ -> Printf.printf "Error: unknown"
  in
  f (Obj.magic cb1);
  [%expect {|
    got 1, done
    Result: 0 |}];
  f (Obj.magic cb2);
  [%expect {|
    got 1, 2, done
    Result: 0 |}];
  f (Obj.magic cb3);
  [%expect {|
    got 1, 2, 3, done
    Result: 0 |}];
  f (Obj.magic cb4);
  [%expect {|
    Result: function#1#1 |}];
  f (Obj.magic cb5);
  [%expect {|
    Result: function#2#2 |}]

let%expect_test _ =
  let open Js_of_ocaml in
  let f = Js.wrap_callback (fun s -> print_endline s) in
  Js.export "f" f;
  let () =
    Js.Unsafe.fun_call
      (Js.Unsafe.pure_js_expr "jsoo_exports")##.f
      [| Js.Unsafe.coerce (Js.string "hello") |]
  in
  ();
  [%expect {| hello |}]
