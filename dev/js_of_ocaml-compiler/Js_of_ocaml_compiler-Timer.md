
# Module `Js_of_ocaml_compiler.Timer`

```ocaml
type t
```
```ocaml
val make : ?get_time:(unit -> float) -> unit -> t
```
```ocaml
val get : t -> float
```
```ocaml
val print : Stdlib.Format.formatter -> t -> unit
```