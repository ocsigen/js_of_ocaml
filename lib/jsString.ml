
type t = Js.string

external of_string : string -> t = "caml_string_to_js"
external to_string : t -> string = "caml_string_from_js"

let of_int i = of_string (string_of_int i) (*FIX: inefficient*)
