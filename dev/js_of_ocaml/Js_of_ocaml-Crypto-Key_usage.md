
# Module `Crypto.Key_usage`

A `KeyUsage`: an operation a [`cryptoKey`](./Js_of_ocaml-Crypto-class-type-cryptoKey.md) may be used for.

```ocaml
type t = 
  | Encrypt
  | Decrypt
  | Sign
  | Verify
  | Derive_key
  | Derive_bits
  | Wrap_key
  | Unwrap_key
```
```ocaml
val to_js : t -> Js.js_string Js.t
```
```ocaml
val of_js : Js.js_string Js.t -> t
```
raises `Invalid_argument` on an unrecognised string.
```ocaml
val list_to_js : t list -> Js.js_string Js.t Js.js_array Js.t
```
```ocaml
val list_of_js : Js.js_string Js.t Js.js_array Js.t -> t list
```