open Js_of_ocaml

val stdin : < readLine : Js.js_string Js.t Js.meth > Js.t

type obj

val obj : (Js.js_string Js.t -> obj Js.t) Js.constr

val set_prop : 'a -> string -> Js.Unsafe.any array -> 'd -> unit

type variant =
  | Null : variant
  | Array : _ array -> variant
  | Bool : bool -> variant
  | Number : float -> variant
  | String : string -> variant
  | Object : _ Js.t -> variant

val to_variant : _ Js.t Js.Opt.t -> variant

val variant : variant -> Js.Unsafe.any

val echo : variant -> unit
