open Util
let compile_and_run s =
  s
  |> Format.ocaml_source_of_string
  |> Format.write_ocaml
  |> compile_ocaml_to_bc
  |> compile_bc_to_javascript
  |> run_javascript
  |> print_endline
