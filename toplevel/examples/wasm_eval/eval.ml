open Js_of_ocaml

let execute (code_js : Js.js_string Js.t) : Js.js_string Js.t =
  let code = Js.to_string code_js in
  let buffer = Buffer.create 256 in
  let ppf = Format.formatter_of_buffer buffer in
  Js_of_ocaml_toplevel_common.JsooTop.execute true ppf code;
  Js.string (Buffer.contents buffer)

let () =
  Js_of_ocaml_toplevel_common.JsooTop.initialize ();
  Js.Unsafe.set Js.Unsafe.global (Js.string "wasmExecute") (Js.wrap_callback execute)
