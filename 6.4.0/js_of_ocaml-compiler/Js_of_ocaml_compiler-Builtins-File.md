
# Module `Builtins.File`

```ocaml
type t
```
```ocaml
val name : t -> string
```
```ocaml
val content : t -> string
```
```ocaml
val fragments : t -> string option
```
```ocaml
val create : name:string -> content:string -> t
```