open Js_of_ocaml

val stdin : < readLine : Js.js_string Js.t Js.meth > Js.t

type obj

val obj : (Js.js_string Js.t -> obj Js.t) Js.constr

val set_prop : 'a -> 'b -> 'c -> 'd -> unit
