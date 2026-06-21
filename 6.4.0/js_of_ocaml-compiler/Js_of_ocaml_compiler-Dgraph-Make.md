
# Module `Dgraph.Make`


## Parameters

```ocaml
module N : sig ... end
```
```ocaml
module NSet : SET with type elt = N.t
```
```ocaml
module NMap : MAP with type key = N.t
```

## Signature

```ocaml
type t = {
  domain : NSet.t;
  fold_children : 'a. (N.t -> 'a -> 'a) -> N.t -> 'a -> 'a;
}
```
```ocaml
val invert : t -> t
```
```ocaml
module type DOMAIN = sig ... end
```
```ocaml
module Solver (D : DOMAIN) : sig ... end
```