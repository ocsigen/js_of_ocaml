
# Module `Dgraph.Solver`


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
```ocaml
module A : ACTION
```
```ocaml
module D : DOMAIN
```

## Signature

```ocaml
type t = {
  domain : NSet.t;
  iter_children : (N.t -> A.t -> unit) -> N.t -> unit;
}
```
```ocaml
val f : 
  state:D.t NTbl.t ->
  t ->
  (state:D.t NTbl.t -> dep:N.t -> target:N.t -> action:A.t -> D.t) ->
  unit
```