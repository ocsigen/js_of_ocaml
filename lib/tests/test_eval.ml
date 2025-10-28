open Js_of_ocaml

let check d = if d == 1 then print_endline "PASSED" else print_endline "FAILED"

let%expect_test "eval_string" =
  check (Js.Unsafe.eval_string "{x:1}");
  (try
     let _ = Js.Unsafe.eval_string "function f() {return 1} ()" in
     print_endline "FAILED"
   with _ -> print_endline "PASSED");
  [%expect {|
    PASSED
    PASSED
    |}]

let%expect_test "js_expr" =
  check Js.Unsafe.(get (js_expr "{x:1}") "x");
  check (Js.Unsafe.js_expr "function f() {return 1} ()");
  [%expect {|
    PASSED
    PASSED
    |}]
