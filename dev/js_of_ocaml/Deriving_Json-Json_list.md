
# Module `Deriving_Json.Json_list`


## Parameters

```ocaml
module A : Json
```

## Signature

```ocaml
type a = A.a list
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