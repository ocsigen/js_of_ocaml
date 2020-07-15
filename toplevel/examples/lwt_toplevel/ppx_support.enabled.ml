let init () = Ast_mapper.register "js_of_ocaml" (fun _ -> Ppx_js.mapper)
