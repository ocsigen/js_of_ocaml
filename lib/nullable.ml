
type 'a t = 'a

let null : 'a t = Js.extract Js.null

let some x = x

let maybe x = if x == null then None else Some x
