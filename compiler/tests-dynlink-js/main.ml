let () = print_endline "hello"

let require s =
  let open Js_of_ocaml in
  (Js.Unsafe.js_expr "require" : Js.js_string Js.t -> unit) (Js.string s)

let () = require "./plugin.js"
