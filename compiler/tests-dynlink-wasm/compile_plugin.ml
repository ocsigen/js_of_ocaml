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
  let unit_info = Unit_info.of_cmo cmo in
  let one = Parse_bytecode.from_cmo ~debug:false cmo ic in
  close_in ic;
  let wasm_binary, fragments = Generate.compile ~unit_name:(Some unit_name) one.code in
  let z = Zip.open_out "plugin_compiled.wasmo" in
  Zip.add_entry z ~name:"code.wasm" ~contents:wasm_binary;
  Zip.add_entry z ~name:"link_order" ~contents:unit_name;
  Link.add_info
    z
    ~build_info:(Build_info.create `Cmo)
    ~unit_data:[ { Link.unit_name; unit_info; fragments } ]
    ();
  Zip.close_out z
