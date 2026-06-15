
# Module `Js_of_ocaml_compiler.Pure_fun`

```ocaml
type t
```
```ocaml
val pure_expr : t -> Code.expr -> bool
```
```ocaml
val pure_instr : t -> Code.instr -> bool
```
```ocaml
val pure : t -> Code.Var.t -> bool
```
```ocaml
val empty : t
```
```ocaml
val f : Code.program -> t
```