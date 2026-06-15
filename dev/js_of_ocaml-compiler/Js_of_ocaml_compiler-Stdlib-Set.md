
# Module `Stdlib.Set`

```ocaml
module type S = sig ... end
```
```ocaml
module Make (Ord : Stdlib.Set.OrderedType) : S with type elt = Ord.t
```