
# Module `Wasm_of_ocaml_compiler.Closure_conversion`

```ocaml
type closure = {
  functions : (Js_of_ocaml_compiler.Code.Var.t * int) list;
  free_variables : Js_of_ocaml_compiler.Code.Var.t list;
  mutable id : int option;
}
```
```ocaml
val f : 
  Js_of_ocaml_compiler.Code.program ->
  Js_of_ocaml_compiler.Code.program
  * closure Js_of_ocaml_compiler.Code.Var.Map.t
```