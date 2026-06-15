
# Module `Pqueue.Make`


## Parameters

```ocaml
module Ord : OrderedType
```

## Signature

```ocaml
type elt = Ord.t
```
```ocaml
type t
```
```ocaml
val empty : t
```
```ocaml
val is_empty : t -> bool
```
```ocaml
val add : elt -> t -> t
```
```ocaml
val union : t -> t -> t
```
```ocaml
val find_min : t -> elt
```
```ocaml
val remove_min : t -> t
```