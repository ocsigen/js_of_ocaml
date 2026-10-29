# Module `Wasm_of_ocaml_compiler.Local_sink`

Local.set sinking for the Wasm backend.

For each `local.set x e` in a function, the pass tries to push the write forward to the first subsequent `local.get x`, turning the `set`/`get` pair into a single `local.tee`. The sink is applied only when it does not cross another write to `x` (no intervening `local.set x` / `local.tee x`), does not cross a control-flow boundary, and does not reorder effects unsafely (conservatively, we require `e` or the intervening code to be effect-free).

When the `local.get x` is the only read of `x`, it is replaced by `e` itself, and `x` is removed from `locals` if it is no longer accessed.

```ocaml
val f : 
  locals:(Wasm_ast.var * Wasm_ast.value_type) list ->
  Wasm_ast.instruction list ->
  (Wasm_ast.var * Wasm_ast.value_type) list * Wasm_ast.instruction list
```
```ocaml
val report_stats : unit -> unit
```
Emit aggregated time/stats logs accumulated across all `f` calls and reset the counters. Honours the `times` and `stats` debug flags.
