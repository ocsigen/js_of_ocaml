
# Module `_.Bigarray`

```ocaml
val get : 
  bound_error_index:int ->
  unsafe:bool ->
  kind:Js_of_ocaml_compiler.Optimization_hint.Bigarray.kind ->
  layout:Js_of_ocaml_compiler.Optimization_hint.Bigarray.layout ->
  expression ->
  indices:expression list ->
  expression
```
```ocaml
val set : 
  bound_error_index:int ->
  unsafe:bool ->
  kind:Js_of_ocaml_compiler.Optimization_hint.Bigarray.kind ->
  layout:Js_of_ocaml_compiler.Optimization_hint.Bigarray.layout ->
  expression ->
  indices:expression list ->
  expression ->
  expression
```