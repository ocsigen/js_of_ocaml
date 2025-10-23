open Js_of_ocaml

let () =
  let p : Js.js_string Js.t = Jsoo_runtime.Sys.external_ "process" in
  let o : _ Js.t = Jsoo_runtime.Sys.external_ "obj" in
  let del = Jsoo_runtime.Sys.external_ "caml_js_delete" in
  ignore (Js.Unsafe.fun_call del [| o; Js.Unsafe.coerce (Js.string "process") |]);
  print_endline (Js.to_string p)
