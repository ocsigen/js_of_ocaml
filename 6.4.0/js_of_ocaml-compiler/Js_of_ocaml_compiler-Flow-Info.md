
# Module `Flow.Info`

```ocaml
type t
```
```ocaml
val def : t -> Code.Var.t -> Code.expr option
```
```ocaml
val update_def : t -> Code.Var.t -> Code.expr -> unit
```
```ocaml
val possibly_mutable : t -> Code.Var.t -> bool
```