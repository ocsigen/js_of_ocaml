
type 'a t = 'a Js.array

let array_header = [| |]

let to_array (a : 'a t) : 'a array =
  Js.extract (Js.meth_call (Js.inject array_header) "concat" [|Js.inject a|])
