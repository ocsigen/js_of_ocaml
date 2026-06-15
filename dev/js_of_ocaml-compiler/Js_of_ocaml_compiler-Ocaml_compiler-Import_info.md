
# Module `Ocaml_compiler.Import_info`

```ocaml
type t
```
```ocaml
type table
```
```ocaml
val make : string -> Stdlib.Digest.t option -> t
```
```ocaml
val to_list : table -> t list
```
```ocaml
val of_list : t list -> table
```
```ocaml
val name : t -> string
```
```ocaml
val crc : t -> Stdlib.Digest.t option
```