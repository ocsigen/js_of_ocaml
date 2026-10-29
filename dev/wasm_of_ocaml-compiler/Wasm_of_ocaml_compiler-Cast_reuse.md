# Module `Wasm_of_ocaml_compiler.Cast_reuse`

Reuse of casts for the Wasm backend.

When a local is cast to the same type several times without being written in between, the result of the first cast is kept in a fresh local and reused instead of casting again. This matters for code run by V8's baseline compiler, which does not remove redundant casts.

Returns the extended `locals` list and the rewritten body.

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
