let () =
  Wasm_of_ocaml_compiler_dynlink.loadfile "plugin.cmo";
  print_endline "done"
