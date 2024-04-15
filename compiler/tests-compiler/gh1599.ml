let%expect_test _ =
  let prog =
    {|
type t =
  | A
  | B
  | C

let f x =
  while
    match x with
    | A -> true
    | B -> true
    | C -> true
  do
    ()
  done
  |}
  in
  let js = Util.compile_and_parse prog in
  Util.print_program js;
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Failure "non-zero exit code")
  Raised at Stdlib__Buffer.add_channel in file "buffer.ml", line 222, characters 18-35
  Called from Jsoo_compiler_expect_tests_helper__Util.channel_to_string.loop in file "compiler/tests-compiler/util/util.ml", line 169, characters 4-52
  Called from Jsoo_compiler_expect_tests_helper__Util.channel_to_string in file "compiler/tests-compiler/util/util.ml", line 172, characters 7-14

  Trailing output
  ---------------
  /home/hugo/js_of_ocaml/_build/default/compiler/bin-js_of_ocaml/js_of_ocaml.exe: You found a bug. Please report it at https://github.com/ocsigen/js_of_ocaml/issues :
  Error: File "compiler/lib/parse_bytecode.ml", line 867, characters 31-37: Assertion failed

  process exited with error code 125
   /home/hugo/js_of_ocaml/_build/default/compiler/bin-js_of_ocaml/js_of_ocaml.exe --pretty --sourcemap --disable=effects --disable=use-js-string --disable header test.cmo -o test.js |}]
