
# Module `Code.Addr`

```ocaml
type t = int
```
```ocaml
val to_string : t -> string
```
```ocaml
val zero : t
```
```ocaml
val succ : t -> t
```
```ocaml
val pred : t -> t
```
```ocaml
module Set : Stdlib.Set.S with type elt = t
```
```ocaml
module Map : Stdlib.Map.S with type key = t
```
```ocaml
module Hashtbl : Js_of_ocaml_compiler.Stdlib.Hashtbl.S with type key = t
```