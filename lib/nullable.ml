
type 'a t = 'a

external get_null : unit -> 'a t = "caml_null_value"

let null = get_null ()

let some x = x

let maybe x = if x == null then None else Some x
