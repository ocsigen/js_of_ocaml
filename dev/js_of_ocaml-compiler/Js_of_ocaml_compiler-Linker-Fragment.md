
# Module `Linker.Fragment`

```ocaml
type t
```
```ocaml
val provides : t -> string list
```
```ocaml
val parse_file : string -> t list
```
```ocaml
val parse_string : string -> t list
```
```ocaml
val parse_builtin : Builtins.File.t -> t list
```
```ocaml
val pack : t -> t
```