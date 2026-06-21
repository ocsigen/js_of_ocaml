
# Module `Deriving_Json.Json_option`


## Parameters

```ocaml
module A : Json
```

## Signature

```ocaml
type a = A.a option
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