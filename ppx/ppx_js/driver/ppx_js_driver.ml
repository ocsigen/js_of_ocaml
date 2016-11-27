let () =
  Ppx_driver.add_arg "-no-js-of-ocaml-wrapper"
    (Arg.Unit (fun () -> Ppx_js.wrapper := None))
    ~doc:" Undocumented";
  let js_mapper = Ppx_js.js_mapper [] in
  Ppx_driver.register_transformation "js_of_ocaml"
    ~impl:(js_mapper.Ast_mapper.structure js_mapper)
    ~intf:(js_mapper.Ast_mapper.signature js_mapper)
