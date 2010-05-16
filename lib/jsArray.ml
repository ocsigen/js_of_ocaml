
type 'a t = 'a array Js.t

let array_header = Js.Unsafe.inject [| |]

let to_array (a : 'a t) : 'a array =
  Js.Unsafe.extract
    (Js.Unsafe.meth_call array_header "concat" [|Js.Unsafe.inject a|])
