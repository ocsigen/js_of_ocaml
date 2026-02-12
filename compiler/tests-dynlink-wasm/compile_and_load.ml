open Js_of_ocaml_compiler
open Wasm_of_ocaml_compiler

let () =
  Config.set_target `Wasm;
  Config.set_effects_backend `Disabled;
  Generate.init ();
  let ic = open_in_bin "plugin.cmo" in
  let cmo =
    match Parse_bytecode.from_channel ic with
    | `Cmo cmo -> cmo
    | `Cma _ | `Exe -> failwith "Expected .cmo file"
  in
  let unit_name = Ocaml_compiler.Cmo_format.name cmo in
  let one = Parse_bytecode.from_cmo ~debug:false cmo ic in
  close_in ic;
  let wasm_binary, _fragments = Generate.compile ~unit_name:(Some unit_name) one.code in
  print_endline "compilation ok";
  ignore (Wasm_of_ocaml_dynlink.load_module_bytes (Bytes.of_string wasm_binary));
  print_endline "done"
