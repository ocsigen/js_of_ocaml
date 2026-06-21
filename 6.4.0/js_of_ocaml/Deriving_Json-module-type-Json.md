
# Module type `Deriving_Json.Json`

The signature of the JSON class.

```ocaml
type a
```
```ocaml
val t : a t
```
```ocaml
val write : Stdlib.Buffer.t -> a -> unit
```
```ocaml
val read : Deriving_Json_lexer.lexbuf -> a
```
```ocaml
val to_string : a -> string
```
```ocaml
val from_string : string -> a
```