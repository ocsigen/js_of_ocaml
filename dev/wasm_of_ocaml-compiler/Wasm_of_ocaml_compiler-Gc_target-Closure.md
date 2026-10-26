# Module `Gc_target.Closure`

```ocaml
val translate : 
  context:Code_generation.context ->
  closures:Closure_conversion.closure Js_of_ocaml_compiler.Code.Var.Map.t ->
  cps:bool ->
  ?cps_version:Js_of_ocaml_compiler.Code.Var.t ->
  no_code_pointer:bool ->
  Js_of_ocaml_compiler.Code.Var.t ->
  expression
```
`cps_version` is the CPS version of the function (double translation): the closure then holds the code pointers of both versions, unless `no_code_pointer`

```ocaml
val bind_environment : 
  context:Code_generation.context ->
  closures:Closure_conversion.closure Js_of_ocaml_compiler.Code.Var.Map.t ->
  cps:bool ->
  ?pair:bool ->
  no_code_pointer:bool ->
  Js_of_ocaml_compiler.Code.Var.t ->
  unit Code_generation.t
```
`pair` indicates that the function is the direct-style version of a function with a CPS version (double translation)

```ocaml
val curry_allocate : 
  ?cps_f:Js_of_ocaml_compiler.Code.Var.t ->
  cps:bool ->
  arity:int ->
  int ->
  f:Js_of_ocaml_compiler.Code.Var.t ->
  closure:Js_of_ocaml_compiler.Code.Var.t ->
  arg:Js_of_ocaml_compiler.Code.Var.t ->
  Wasm_ast.expression Code_generation.t
```
```ocaml
val curry_load : 
  cps:bool ->
  arity:int ->
  int ->
  Js_of_ocaml_compiler.Code.Var.t ->
  (expression * expression * Wasm_ast.value_type option) Code_generation.t
```
```ocaml
val dummy : cps:bool -> arity:int -> Wasm_ast.expression Code_generation.t
```
