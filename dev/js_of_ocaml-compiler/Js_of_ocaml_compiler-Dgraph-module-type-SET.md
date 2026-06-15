
# Module type `Dgraph.SET`

```ocaml
type elt
```
```ocaml
type t
```
```ocaml
val empty : t
```
```ocaml
val add : elt -> t -> t
```
```ocaml
val remove : elt -> t -> t
```
```ocaml
val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
```
```ocaml
val mem : elt -> t -> bool
```
```ocaml
val equal : t -> t -> bool
```