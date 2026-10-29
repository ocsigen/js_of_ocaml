# Module `Wasm_of_ocaml_compiler.Inline_globals`

```ocaml
val f : Wasm_ast.module_field list -> Wasm_ast.module_field list
```
Inline the initializer of each immutable, non-exported global used only once, in the initializer of another global, at that use, and remove the global. The globals must be in definition order.
