[@@@ocaml.alert "-unsafe_multidomain"]

let () = Js_of_ocaml_toplevel_common.JsooTop.initialize ()

let () = Printexc.register_printer (fun x ->
             match Jsoo_runtime.Error.of_exn x with
             | None -> None
             | Some e -> Some ("<JavaScript exception>"))

let () = Toplevel_expect_test.run (fun _ -> Ast_mapper.default_mapper)

