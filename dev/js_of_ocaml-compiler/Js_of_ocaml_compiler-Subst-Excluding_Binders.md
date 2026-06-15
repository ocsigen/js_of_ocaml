
# Module `Subst.Excluding_Binders`

The operations of this module substitute variable names that appear in expressions, except for binders, i.e., names on the right-hand side of a [`Code.instr.Let`](./Js_of_ocaml_compiler-Code.md#type-instr.Let).

```ocaml
val program : (Code.Var.t -> Code.Var.t) -> Code.program -> Code.program
```
```ocaml
val expr : (Code.Var.t -> Code.Var.t) -> Code.expr -> Code.expr
```
```ocaml
val instr : (Code.Var.t -> Code.Var.t) -> Code.instr -> Code.instr
```
```ocaml
val instrs : (Code.Var.t -> Code.Var.t) -> Code.instr list -> Code.instr list
```
```ocaml
val block : (Code.Var.t -> Code.Var.t) -> Code.block -> Code.block
```
```ocaml
val last : (Code.Var.t -> Code.Var.t) -> Code.last -> Code.last
```
```ocaml
val cont : (Code.Var.t -> Code.Var.t) -> int -> Code.program -> Code.program
```
```ocaml
val cont' : 
  (Code.Var.t -> Code.Var.t) ->
  int ->
  Code.block Js_of_ocaml_compiler.Code.Addr.Map.t ->
  Code.Addr.Set.t ->
  Code.block Js_of_ocaml_compiler.Code.Addr.Map.t * Code.Addr.Set.t
```