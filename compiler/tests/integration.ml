open Util

let%expect_test _ =
  {| console.log("hello world") |}
  |> Util.Format.js_source_of_string
  |> Util.Format.write_js
  |> Util.run_javascript
  |> print_endline;
  [%expect {| hello world |}]

let compile_and_run s =
  s
  |> Format.ocaml_source_of_string
  |> Format.write_ocaml
  |> compile_ocaml_to_bc
  |> compile_bc_to_javascript
  |> run_javascript
  |> print_endline

let%expect_test _ =
  compile_and_run {| print_endline "hello world" |};
  [%expect {| hello world |}]
