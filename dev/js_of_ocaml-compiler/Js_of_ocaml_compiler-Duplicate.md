
# Module `Js_of_ocaml_compiler.Duplicate`

```ocaml
val closure : 
  Code.program ->
  f:Code.Var.t ->
  params:Code.Var.t list ->
  cont:(int * Code.Var.t list) ->
  int Stdlib.Array.t ->
  Code.program * Code.Var.t * Code.Var.t list * (int * Code.Var.t list)
```
Given a program and a closure `f` \-- defined by its name, parameters, and its continuation \--, return a program with a copy of `f`. Also returns the new name of `f`, and the similarly substituted parameter list and continuation.
