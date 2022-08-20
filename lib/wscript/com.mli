open Js_of_ocaml

type obj

val obj : (Js.js_string Js.t -> obj Js.t) Js.constr

module Unsafe : sig
  external set_prop : 'a -> string -> Js.Unsafe.any array -> 'd -> unit = "caml_js_set_prop"
end

type variant =
  | Null : variant
  | Array : _ array -> variant
  | Bool : bool -> variant
  | Number : float -> variant
  | String : string -> variant
  | Object : _ Js.t -> variant

val to_variant : _ Js.t Js.Opt.t -> variant
val variant : variant -> Js.Unsafe.any
