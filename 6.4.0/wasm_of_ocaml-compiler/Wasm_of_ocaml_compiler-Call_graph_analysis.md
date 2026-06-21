
# Module `Wasm_of_ocaml_compiler.Call_graph_analysis`

```ocaml
type t
```
```ocaml
val direct_calls_only : t -> Js_of_ocaml_compiler.Code.Var.t -> bool
```
```ocaml
val f : 
  Js_of_ocaml_compiler.Code.program ->
  Js_of_ocaml_compiler.Global_flow.info ->
  t
```