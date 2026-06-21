
# Module `Js_of_ocaml_compiler.Js_variable_coalescing`

Liveness analysis and variable coalescing for JavaScript generation.

```ocaml
val f : Javascript.program -> Javascript.program
```
`f program` performs liveness analysis on the given JavaScript program and renames variables to minimize the number of distinct variables used, effectively reusing registers where possible.
