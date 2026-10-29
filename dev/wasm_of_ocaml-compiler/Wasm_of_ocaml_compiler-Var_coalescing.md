# Module `Wasm_of_ocaml_compiler.Var_coalescing`

Liveness analysis and variable coalescing for the Wasm backend.

Merges Wasm locals (including parameters) with disjoint live ranges so that a single local is reused. This shrinks the function's `locals` declaration and eliminates redundant `local.set`/`local.get` copies.

Parameters are valid coalescing targets: a local whose live range does not overlap a parameter's can be rewritten to use the parameter's index. Two parameters never merge because they are all live at function entry.

```ocaml
val f : 
  param_names:Js_of_ocaml_compiler.Code.Var.t list ->
  param_types:Wasm_ast.value_type list ->
  locals:(Js_of_ocaml_compiler.Code.Var.t * Wasm_ast.value_type) list ->
  Wasm_ast.instruction list ->
  (Js_of_ocaml_compiler.Code.Var.t * Wasm_ast.value_type) list
  * Wasm_ast.instruction list
```
```ocaml
val report_stats : unit -> unit
```
Emit aggregated time/stats logs accumulated across all `f` calls and reset the counters. Honours the `times` and `stats` debug flags.
