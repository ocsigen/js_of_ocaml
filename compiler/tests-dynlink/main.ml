let () = print_endline "hello"

let () = Js_of_ocaml_toplevel.JsooTop.initialize ()

let () =
  Sys.interactive := false;
  Dynlink.loadfile "/static/plugin.cmo"
