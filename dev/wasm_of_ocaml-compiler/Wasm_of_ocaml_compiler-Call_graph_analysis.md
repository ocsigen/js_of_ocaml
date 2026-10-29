# Module `Wasm_of_ocaml_compiler.Call_graph_analysis`

```ocaml
type t
```
```ocaml
val direct_calls_only : t -> Js_of_ocaml_compiler.Code.Var.t -> bool
```
```ocaml
val known_callee : 
  Js_of_ocaml_compiler.Global_flow.info ->
  t ->
  wrappable:(Js_of_ocaml_compiler.Code.Var.t -> bool) ->
  bool ->
  Js_of_ocaml_compiler.Code.Var.t ->
  Js_of_ocaml_compiler.Code.Var.t option
```
`known_callee info t ~wrappable exact f` is the function called at a call site `Apply { f; exact; _ }` when it is the only possible callee and the call can be a direct call: the function is only called directly, or it is `wrappable`.

```ocaml
val raising_functions : 
  Js_of_ocaml_compiler.Code.program ->
  Js_of_ocaml_compiler.Global_flow.info ->
  t ->
  wrappable:(Js_of_ocaml_compiler.Code.Var.t -> bool) ->
  (Js_of_ocaml_compiler.Code.Var.t -> bool) ->
  unit Js_of_ocaml_compiler.Code.Var.Hashtbl.t
```
Functions which signal exceptions by returning null. The functions that are not only called directly (`direct_calls_only` is false) must be wrapped: the function called through closures converts the null value back into an exception. Only functions satisfying `wrappable` are considered for this.

```ocaml
val f : 
  Js_of_ocaml_compiler.Code.program ->
  Js_of_ocaml_compiler.Global_flow.info ->
  t
```
