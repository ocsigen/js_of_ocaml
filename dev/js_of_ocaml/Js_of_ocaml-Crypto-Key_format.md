
# Module `Crypto.Key_format`

A `KeyFormat` for importing and exporting keys.

```ocaml
type t = 
  | Raw
  | Spki
  | Pkcs8
  | Jwk
```
```ocaml
val to_js : t -> Js.js_string Js.t
```
```ocaml
val of_js : Js.js_string Js.t -> t
```
raises `Invalid_argument` on an unrecognised string.