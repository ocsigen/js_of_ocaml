let hide_js_preview () =
  try
    let elt = Js_of_ocaml.Dom_html.getElementById "js-details" in
    Js_of_ocaml.Js.Unsafe.set
      (Js_of_ocaml.Js.Unsafe.get elt (Js_of_ocaml.Js.string "style"))
      (Js_of_ocaml.Js.string "display")
      (Js_of_ocaml.Js.string "none")
  with Not_found -> ()

let () =
  Js_of_ocaml.Dom_html.onload (fun () ->
      Toplevel_main.run ~setup_preview:hide_js_preview ())
