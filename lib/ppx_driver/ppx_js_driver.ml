let () =
  let js_mapper = Ppx_js.js_mapper [] in
  Ppx_driver.register_transformation "js_of_ocaml"
    ~impl:(js_mapper.Ast_mapper.structure js_mapper)
    ~intf:(js_mapper.Ast_mapper.signature js_mapper)
