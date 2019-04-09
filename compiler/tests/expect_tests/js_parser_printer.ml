open Js_of_ocaml_compiler

let print_compacted source =
  let buffer = Buffer.create (String.length source) in
  let pp = Pretty_print.to_buffer buffer in
  Pretty_print.set_compact pp true;
  let lexed = Parse_js.lexer_from_string source in
  let parsed = Parse_js.parse lexed in
  Js_output.program pp parsed;
  print_endline (Buffer.contents buffer)

let%expect_test "no postfix addition coalesce" =
  print_compacted "a + +b";
  [%expect {|
    a+
    +b; |}]

let%expect_test "no postfix subtraction coalesce" =
  print_compacted "a - -b";
  [%expect {|
    a-
    -b; |}]

let%expect_test "reserved words as fields" =
  print_compacted
    {|
    x.debugger;
    x.catch;
    var y = { debugger : 2 }
    var y = { catch : 2 }
  |};
  [%expect {|
    x.debugger;x.catch;var
    y={debugger:2};var
    y={catch:2}; |}]
