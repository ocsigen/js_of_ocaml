
# Module `Js_of_ocaml_compiler.Pqueue`

```ocaml
module type OrderedType = sig ... end
```
```ocaml
module type S = sig ... end
```
```ocaml
module Make (Ord : OrderedType) : S with type elt = Ord.t
```