
# Parameter `Solver.NSet`

```ocaml
type t
```
```ocaml
type elt = N.t
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