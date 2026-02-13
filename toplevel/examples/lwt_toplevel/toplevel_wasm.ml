let () = Js_of_ocaml.Dom_html.onload (fun () -> Toplevel_main.run ~setup_preview:ignore ())
