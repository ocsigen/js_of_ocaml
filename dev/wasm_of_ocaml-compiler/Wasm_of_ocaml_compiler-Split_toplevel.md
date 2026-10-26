# Module `Wasm_of_ocaml_compiler.Split_toplevel`

```ocaml
val f : 
  name:Js_of_ocaml_compiler.Code.Var.t ->
  locals:(Wasm_ast.var * Wasm_ast.value_type) list ->
  Wasm_ast.instruction list ->
  (Wasm_ast.var * Wasm_ast.value_type) list
  * Wasm_ast.instruction list
  * Wasm_ast.module_field list
```
Split a large function into smaller ones.

`f ~name ~locals body` outlines large runs of instructions of the body `body` of a parameterless function into separate functions. It returns the locals and body of the residual function, and the new functions. Neither the residual body nor the new functions have been post-processed (see `Initialize_locals`).
