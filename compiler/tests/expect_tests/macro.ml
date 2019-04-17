module Jsoo = Js_of_ocaml_compiler

let print_macro_transformed source =
  let buffer = Buffer.create (String.length source) in
  let pp = Jsoo.Pretty_print.to_buffer buffer in
  Jsoo.Pretty_print.set_compact pp false;
  let parsed = Util.parse_js source in
  let transformed = Jsoo.Macro.f parsed in
  Jsoo.Js_output.program pp transformed;
  print_endline (Buffer.contents buffer)

let print_macro_transformed source =
  try print_macro_transformed source with Failure s -> Format.printf "failure: %s" s

let%expect_test "BLOCK()" =
  print_macro_transformed "BLOCK()";
  [%expect {| failure: macro BLOCK called with inappropriate arguments |}]

let%expect_test "TAG()" =
  print_macro_transformed "TAG()";
  [%expect {| failure: macro TAG called with inappropriate arguments |}]

let%expect_test "LENGTH()" =
  print_macro_transformed "LENGTH()";
  [%expect {| failure: macro LENGTH called with inappropriate arguments |}]

let%expect_test "FIELD()" =
  print_macro_transformed "FIELD()";
  [%expect {| failure: macro FIELD called with inappropriate arguments |}]

let%expect_test "ISBLOCK()" =
  print_macro_transformed "ISBLOCK()";
  [%expect {| failure: macro ISBLOCK called with inappropriate arguments |}]

let%expect_test "BLOCK(1)" =
  print_macro_transformed "BLOCK(1)";
  [%expect {| failure: macro BLOCK called with inappropriate arguments |}]

let%expect_test "BLOCK(tag)" =
  print_macro_transformed "BLOCK(tag)";
  [%expect {| failure: macro BLOCK called with inappropriate arguments |}]

let%expect_test "BLOCK(1, a)" =
  print_macro_transformed "BLOCK(1, a)";
  [%expect {| [1,a]; |}]

let%expect_test "BLOCK(1, a, b, c)" =
  print_macro_transformed "BLOCK(1, a, b, c)";
  [%expect {| [1,a,b,c]; |}]

let%expect_test "TAG(a)" =
  print_macro_transformed "TAG(a)";
  [%expect {| a[0]; |}]

let%expect_test "LENGTH(a)" =
  print_macro_transformed "LENGTH(a)";
  [%expect {| a.length - 1; |}]

let%expect_test "FIELD(a)" =
  print_macro_transformed "FIELD(a)";
  [%expect {| failure: macro FIELD called with inappropriate arguments |}]

let%expect_test "FIELD(a, b)" =
  print_macro_transformed "FIELD(a, b)";
  [%expect {| a[1 + b]; |}]

let%expect_test "FIELD(a, b << 5)" =
  print_macro_transformed "FIELD(a, b << 5)";
  [%expect {| a[1 + (b << 5)]; |}]

let%expect_test "FIELD(a, 0)" =
  print_macro_transformed "FIELD(a, 0)";
  [%expect {| a[1]; |}]

let%expect_test "FIELD(a, -1)" =
  print_macro_transformed "FIELD(a, -1)";
  [%expect {| failure: Negative field indexes are not allowed |}]

let%expect_test "ISBLOCK(a)" =
  print_macro_transformed "ISBLOCK(a)";
  [%expect {| typeof a !== "number"; |}]
