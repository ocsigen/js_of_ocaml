
# Module `Js_of_ocaml_compiler.Global_deadcode`

Global deadcode elimination pass.

This module provides a global liveness analysis more powerful than that found in `deadcode.ml`. In particular, this analysis annotates blocks with the specific fields that are live. It also uses `global_flow.ml` to determine the liveness of function return values. It first computes an initial liveness of each variable by traversing the program IR. Then it propagates this information to the dependencies of each variable using a flow analysis solver. Lastly it replaces dead variables with a sentinel zero variable (the JS value \`undefined\`).

Although this module does not perform any dead-code elimination itself, it is designed to be used to identify and substitute dead variables that are then removed by `deadcode.ml`. In particular it allows for the elimination of unused functions defined in functors, which the original deadcode elimination cannot.

```ocaml
val f : 
  Pure_fun.t ->
  Code.program ->
  deadcode_sentinel:Code.Var.t ->
  Global_flow.info ->
  Code.program
```