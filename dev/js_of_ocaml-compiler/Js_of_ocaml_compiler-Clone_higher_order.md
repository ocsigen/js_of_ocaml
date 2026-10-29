# Module `Js_of_ocaml_compiler.Clone_higher_order`

```ocaml
val f : Code.program -> Code.program
```
Duplicate higher-order functions for each combination of known closures passed as the parameters they call, and redirect the corresponding call sites to the copies.
