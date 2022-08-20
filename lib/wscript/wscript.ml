open Js_of_ocaml

let stdin = Js.Unsafe.global##._WScript##._StdIn

let echo x = Js.Unsafe.global##._WScript##echo (Com.variant x)
