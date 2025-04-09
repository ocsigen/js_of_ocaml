
let () = Js_of_ocaml_toplevel.JsooTop.initialize ()

let () = Printexc.register_printer (fun x ->
             match Js_of_ocaml.Js_error.of_exn x with
             | None -> None
             | Some e -> Some (Js_of_ocaml.Js_error.message e))

let () = Toplevel_expect_test.run (fun _ -> Ast_mapper.default_mapper)

