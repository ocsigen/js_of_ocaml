open Js_of_ocaml
open Js_of_ocaml_wscript

let create () = Js.Unsafe.coerce (new%js Wscript.obj (Js.string "Excel.Application"))
