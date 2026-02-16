let () = print_endline "hello"

let () = Wasm_of_ocaml_compiler_dynlink.loadfile "./plugin.wasmo"
