
# Module `Js_of_ocaml.Json`

Unsafe IO. (See [`Deriving_Json`](./Deriving_Json.md) for typesafe IO)

```ocaml
val output : 'a -> Js.js_string Js.t
```
Marshal any OCaml value into this JSON representation.

```ocaml
val unsafe_input : Js.js_string Js.t -> 'a
```
Unmarshal a string in JSON format as an OCaml value (unsafe but fast !).

Raises `Failure` under the wasm\_of\_ocaml backend, where the encoding of OCaml values is ambiguous (integers and floats both map to numbers).
