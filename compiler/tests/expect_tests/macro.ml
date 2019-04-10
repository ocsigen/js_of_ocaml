module Jsoo = Js_of_ocaml_compiler

let print_macro_transformed source =
  let buffer = Buffer.create (String.length source) in
  let pp = Jsoo.Pretty_print.to_buffer buffer in
  Jsoo.Pretty_print.set_compact pp false;
  let lexed = Jsoo.Parse_js.lexer_from_string source in
  let parsed = Jsoo.Parse_js.parse lexed in
  let transformed = Jsoo.Driver.For_testing.macro parsed in
  Jsoo.Js_output.program pp transformed;
  print_endline (Buffer.contents buffer)

let print_macro_transformed source =
  try print_macro_transformed source
  with Assert_failure (s, _, _) -> Format.printf "assertion failed in %s" s

let%expect_test "BLOCK()" =
  print_macro_transformed "BLOCK()";
  [%expect {| assertion failed in compiler/lib/driver.ml |}]

let%expect_test "TAG()" =
  print_macro_transformed "TAG()";
  [%expect {| assertion failed in compiler/lib/driver.ml |}]

let%expect_test "LENGTH()" =
  print_macro_transformed "LENGTH()";
  [%expect {| assertion failed in compiler/lib/driver.ml |}]

let%expect_test "FIELD()" =
  print_macro_transformed "FIELD()";
  [%expect {| assertion failed in compiler/lib/driver.ml |}]

let%expect_test "ISBLOCK()" =
  print_macro_transformed "ISBLOCK()";
  [%expect {| assertion failed in compiler/lib/driver.ml |}]

let%expect_test "BLOCK(1)" =
  print_macro_transformed "BLOCK(1)";
  [%expect {| ({tag:1,length:0}); |}]

let%expect_test "BLOCK(1, a)" =
  print_macro_transformed "BLOCK(1, a)";
  [%expect {| ({tag:1,length:1,f0:a}); |}]

let%expect_test "BLOCK(1, a, b, c)" =
  print_macro_transformed "BLOCK(1, a, b, c)";
  [%expect {| ({tag:1,length:3,f0:a,f1:b,f2:c}); |}]

let%expect_test "TAG(a)" =
  print_macro_transformed "TAG(a)";
  [%expect {| a.tag; |}]

let%expect_test "LENGTH(a)" =
  print_macro_transformed "LENGTH(a)";
  [%expect {| a.length; |}]

let%expect_test "FIELD(a, 0)" =
  print_macro_transformed "FIELD(a, 0)";
  [%expect {| a.f0; |}]

let%expect_test "ISBLOCK(a)" =
  print_macro_transformed "ISBLOCK(a)";
  [%expect {| typeof a.tag == "number"; |}]
