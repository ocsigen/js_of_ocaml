
# Module type `Dgraph.ISet`

```ocaml
type t
```
```ocaml
type elt
```
```ocaml
val iter : (elt -> unit) -> t -> unit
```
```ocaml
val mem : t -> elt -> bool
```
```ocaml
val add : t -> elt -> unit
```
```ocaml
val remove : t -> elt -> unit
```
```ocaml
val copy : t -> t
```