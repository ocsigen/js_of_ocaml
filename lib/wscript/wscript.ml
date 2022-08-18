open Js_of_ocaml

let stdin = Js.Unsafe.global##._WScript##._StdIn

type obj

let obj = Js.Unsafe.global##._ActiveXObject

external set_prop : 'a -> string -> Js.Unsafe.any array -> 'd -> unit = "caml_js_set_prop"
