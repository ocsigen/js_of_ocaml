let () = print_endline "hello"

let require s =
  let open Js_of_ocaml in
  Js.Unsafe.fun_call (Js.Unsafe.js_expr "require") [| Js.Unsafe.inject (Js.string s) |]

let () = require "./plugin.js"

let () = require "./plugin2.js"
