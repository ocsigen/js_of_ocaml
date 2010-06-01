
open Js

class type js_string = object
  method toString : js_string t meth
  method valueOf : js_string t meth
  method charAt : int -> js_string t meth
  method charCodeAt : int -> float meth (* This may return NaN... *)
  method concat : js_string t -> js_string t meth
  method concat_2 : js_string t -> js_string t -> js_string t meth
  method concat_3 :
    js_string t -> js_string t -> js_string t -> js_string t meth
  method concat_4 :
    js_string t -> js_string t -> js_string t -> js_string t ->
    js_string t meth
  method indexOf : js_string t -> int meth
  method indexOf_from : js_string t -> int -> int meth
  method lastIndexOf : js_string t -> int meth
  method lastIndexOf_from : js_string t -> int -> int meth
  method localeCompare : js_string t -> float meth
(*
  method _match : JsRegexp.t -> ... opt meth
  method replace : JsRegexp.t -> js_string t -> js_string t
*)
(* FIX: replace using a function? *)
  method replace_string : js_string t -> js_string t -> js_string t
(*
  method search : JsRegexp.t -> ... opt meth
*)
  method slice : int -> int -> js_string t meth
  method slice_end : int -> js_string t meth
(*FIX: recursive dependency...
  method split : js_string t -> js_string t js_array t meth
  method split_limited : js_string t -> int -> js_string t js_array t meth
*)
  method substring : int -> int -> js_string t meth
  method substring_to_end : int -> js_string t meth
  method toLowerCase : js_string meth
  method toLocaleLowerCase : js_string meth
  method toUpperCase : js_string meth
  method toLocaleUpperCase : js_string meth
end

type t = string Js.t

external of_string : string -> t = "caml_string_to_js"
external to_string : t -> string = "caml_string_from_js"

let of_int i = of_string (string_of_int i) (*FIX: inefficient*)
