
# Module `Code.Native_string`

```ocaml
type t = private 
  | Byte of string
  | Utf of Stdlib.Utf8_string.t
```
```ocaml
val of_string : string -> t
```
```ocaml
val of_bytestring : string -> t
```
```ocaml
val equal : t -> t -> bool
```