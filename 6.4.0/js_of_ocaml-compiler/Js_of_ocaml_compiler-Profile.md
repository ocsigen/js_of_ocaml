
# Module `Js_of_ocaml_compiler.Profile`

```ocaml
type t = 
  | O1
  | O2
  | O3
```
```ocaml
val all : t list
```
```ocaml
val of_int : int -> t option
```
```ocaml
val to_int : t -> int
```
```ocaml
val equal : t -> t -> bool
```