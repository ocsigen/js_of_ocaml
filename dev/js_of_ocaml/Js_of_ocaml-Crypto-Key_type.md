
# Module `Crypto.Key_type`

The `KeyType` of a [`cryptoKey`](./Js_of_ocaml-Crypto-class-type-cryptoKey.md).

```ocaml
type t = 
  | Public
  | Private
  | Secret
```
```ocaml
val to_js : t -> Js.js_string Js.t
```
```ocaml
val of_js : Js.js_string Js.t -> t
```
raises `Invalid_argument` on an unrecognised string.