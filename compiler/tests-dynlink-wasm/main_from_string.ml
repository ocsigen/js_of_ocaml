let () = print_endline "hello"

let () = Wasm_of_ocaml_dynlink.loadfile "./plugin_compiled.wasmo"
