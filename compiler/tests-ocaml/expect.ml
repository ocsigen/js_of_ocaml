
let () = Js_of_ocaml_toplevel.JsooTop.initialize ()

let () = Toplevel_expect_test.run (fun _ -> Ast_mapper.default_mapper)
