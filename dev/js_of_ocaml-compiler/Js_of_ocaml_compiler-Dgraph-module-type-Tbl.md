
# Module type `Dgraph.Tbl`

```ocaml
type 'a t
```
```ocaml
type key
```
```ocaml
type size
```
```ocaml
val get : 'a t -> key -> 'a
```
```ocaml
val set : 'a t -> key -> 'a -> unit
```
```ocaml
val make : size -> 'a -> 'a t
```