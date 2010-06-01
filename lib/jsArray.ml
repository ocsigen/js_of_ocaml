
open Js

class type ['a] js_array = object
  method toString : string t meth
  method toLocaleString : string t meth
  method concat : 'a js_array t -> 'a js_array t meth
  method join : string t -> string t meth
  method pop : 'a optdef meth
  method push : 'a -> int meth
  method push_2 : 'a -> 'a -> int meth
  method push_3 : 'a -> 'a -> 'a -> int meth
  method push_4 : 'a -> 'a -> 'a -> 'a -> int meth
  method reverse : 'a js_array t meth
  method shift : 'a optdef meth
  method slice : int -> int -> 'a js_array t meth
  method slice_end : int -> 'a js_array t meth
(* method sort : ('a -> 'a -> float) callback -> 'a js_array t meth *)
(* method sort : ('a -> 'a -> int) callback -> 'a js_array t meth *)
  method sort_asStrings : 'a js_array t meth
  method splice : int -> int -> 'a js_array t meth
  method splice_1 : int -> int -> 'a -> 'a js_array t meth
  method splice_2 : int -> int -> 'a -> 'a -> 'a js_array t meth
  method splice_3 : int -> int -> 'a -> 'a -> 'a -> 'a js_array t meth
  method splice_4 : int -> int -> 'a -> 'a -> 'a -> 'a -> 'a js_array t meth
  method unshift : 'a -> int meth
  method unshift_2 : 'a -> 'a -> int meth
  method unshift_3 : 'a -> 'a -> 'a -> int meth
  method unshift_4 : 'a -> 'a -> 'a -> 'a -> int meth
  method length : int prop
end

type 'a t = 'a js_array Js.t

let array_header = Js.Unsafe.inject [| |]

(* FIX: use external primitives for portability *)
let to_array (a : 'a t) : 'a array =
  Js.Unsafe.extract
    (Js.Unsafe.meth_call array_header "concat" [|Js.Unsafe.inject a|])

let of_array a =
  Js.Unsafe.extract (Js.Unsafe.meth_call a "slice" [|Js.Unsafe.inject 1|])
