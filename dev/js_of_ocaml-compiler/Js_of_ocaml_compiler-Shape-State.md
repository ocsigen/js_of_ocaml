
# Module `Shape.State`

```ocaml
val propagate : Code.Var.t -> int -> Code.Var.t -> unit
```
```ocaml
val assign : Code.Var.t -> t -> unit
```
```ocaml
val get : Code.Var.t -> t option
```
```ocaml
val mem : Code.Var.t -> bool
```
```ocaml
val is_pure_fun : Code.Var.t -> bool
```
```ocaml
val reset : unit -> unit
```