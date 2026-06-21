
# Module `Dgraph.Make_Imperative`


## Parameters

```ocaml
module N : sig ... end
```
```ocaml
module NSet : ISet with type elt = N.t
```
```ocaml
module NTbl : Tbl with type key = N.t
```

## Signature

```ocaml
type t = {
  domain : NSet.t;
  iter_children : (N.t -> unit) -> N.t -> unit;
}
```
```ocaml
val invert : NTbl.size -> t -> t
```
```ocaml
module type DOMAIN = sig ... end
```
```ocaml
module Solver (D : DOMAIN) : sig ... end
```