
# Module `Wasm_of_ocaml_compiler.Initialize_locals`

```ocaml
val f : 
  param_names:Wasm_ast.var list ->
  locals:(Wasm_ast.var * Wasm_ast.value_type) list ->
  Wasm_ast.instruction list ->
  (Wasm_ast.var * Wasm_ast.value_type) list * Wasm_ast.instruction list
```