
# Module `Var.ISet`

```ocaml
type elt = t
```
```ocaml
type t
```
```ocaml
val empty : unit -> t
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