
# Module `Stdlib.Utf8_string`

```ocaml
type t = private 
  | Utf8 of string
```
```ocaml
val of_string_exn : string -> t
```
```ocaml
val compare : t -> t -> int
```
```ocaml
val equal : t -> t -> bool
```