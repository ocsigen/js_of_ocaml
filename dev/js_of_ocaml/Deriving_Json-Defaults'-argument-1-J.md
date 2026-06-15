
# Parameter `Defaults'.J`

```ocaml
type a
```
```ocaml
val write : Stdlib.Buffer.t -> a -> unit
```
```ocaml
val read : Deriving_Json_lexer.lexbuf -> a
```
```ocaml
val match_variant : [ `Cst of int | `NCst of int ] -> bool
```
```ocaml
val read_variant : 
  Deriving_Json_lexer.lexbuf ->
  [ `Cst of int | `NCst of int ] ->
  a
```