
# Module `Subst.Including_Binders`

The operations of this module also substitute the variables names that appear on the left-hand-side of a [`Code.instr.Let`](./Js_of_ocaml_compiler-Code.md#type-instr.Let), or as block parameters, or as closure parameters, or are bound by an exception handler.

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
module And_Continuations : sig ... end
```