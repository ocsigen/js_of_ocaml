
type t

external of_string : string -> t = "caml_string_from_js"
external to_string : t -> string = "caml_string_to_js"
