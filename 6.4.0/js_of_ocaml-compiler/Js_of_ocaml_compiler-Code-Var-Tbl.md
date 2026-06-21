
# Module `Var.Tbl`

```ocaml
type key = t
```
```ocaml
type 'a t
```
```ocaml
type size = unit
```
```ocaml
val get : 'a t -> key -> 'a
```
```ocaml
val set : 'a t -> key -> 'a -> unit
```
```ocaml
val length : 'a t -> int
```
```ocaml
val make : size -> 'a -> 'a t
```
```ocaml
val iter : (key -> 'a -> unit) -> 'a t -> unit
```