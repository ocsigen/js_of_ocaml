
# Module `Js_of_ocaml_compiler.Target_env`

```ocaml
type t = 
  | Browser
  | Nodejs
  | Isomorphic
```
```ocaml
val all : t list
```
```ocaml
val equal : t -> t -> bool
```
```ocaml
val to_string : t -> string
```
```ocaml
val of_string : string -> t option
```