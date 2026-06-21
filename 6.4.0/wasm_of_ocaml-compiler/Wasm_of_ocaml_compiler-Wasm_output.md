
# Module `Wasm_of_ocaml_compiler.Wasm_output`

```ocaml
val to_string : Wasm_ast.module_field list -> string
```
```ocaml
val f : 
  opt_source_map_file:string option ->
  Stdlib.out_channel ->
  Wasm_ast.module_field list ->
  unit
```