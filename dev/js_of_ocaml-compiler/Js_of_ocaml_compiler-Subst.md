
# Module `Js_of_ocaml_compiler.Subst`

```ocaml
module Excluding_Binders : sig ... end
```
The operations of this module substitute variable names that appear in expressions, except for binders, i.e., names on the right-hand side of a [`Code.instr.Let`](./Js_of_ocaml_compiler-Code.md#type-instr.Let).

```ocaml
val from_array : Code.Var.t array -> Code.Var.t -> Code.Var.t
```
```ocaml
val build_mapping : 
  Code.Var.t list ->
  Code.Var.t list ->
  Code.Var.t Js_of_ocaml_compiler.Code.Var.Map.t
```
```ocaml
val from_map : 
  Code.Var.t Js_of_ocaml_compiler.Code.Var.Map.t ->
  Code.Var.t ->
  Code.Var.t
```
```ocaml
module Including_Binders : sig ... end
```
The operations of this module also substitute the variables names that appear on the left-hand-side of a [`Code.instr.Let`](./Js_of_ocaml_compiler-Code.md#type-instr.Let), or as block parameters, or as closure parameters, or are bound by an exception handler.
