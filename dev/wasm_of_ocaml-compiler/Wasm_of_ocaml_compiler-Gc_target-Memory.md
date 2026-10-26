# Module `Gc_target.Memory`

```ocaml
val allocate : 
  tag:int ->
  Wasm_ast.expression list Code_generation.t ->
  expression
```
```ocaml
val allocate_float_array : 
  Wasm_ast.expression list Code_generation.t ->
  expression
```
```ocaml
val load_function_pointer : 
  cps:bool ->
  arity:int ->
  ?skip_cast:bool ->
  expression ->
  (Wasm_ast.var * Wasm_ast.expression) Code_generation.t
```
```ocaml
val load_real_closure : 
  cps:bool ->
  arity:int ->
  expression ->
  (Wasm_ast.var * Wasm_ast.expression) Code_generation.t
```
```ocaml
val check_function_arity : 
  Js_of_ocaml_compiler.Code.Var.t ->
  cps:bool ->
  arity:int ->
  (typ:Wasm_ast.value_type option -> expression -> expression) ->
  unit Code_generation.t ->
  unit Code_generation.t
```
```ocaml
val call_continuation : expression -> expression -> expression
```
`call_continuation k res` applies the continuation `k` to `res`

```ocaml
val cps_call_or_direct : 
  arity:int ->
  Js_of_ocaml_compiler.Code.Var.t ->
  expression list ->
  expression
```
With double translation, CPS call to a function which is not known statically: if the function has no CPS version, its direct-style version is called and the result is passed to the continuation (the last argument). The calls are tail calls, so this can only be used when the result of the call is immediately returned. The call must be exact, unless there is a single argument besides the continuation.

```ocaml
val tag : expression -> expression
```
```ocaml
val field : expression -> int -> expression
```
```ocaml
val set_field : expression -> int -> expression -> unit Code_generation.t
```
```ocaml
val array_get : expression -> expression -> expression
```
```ocaml
val array_set : 
  expression ->
  expression ->
  expression ->
  unit Code_generation.t
```
```ocaml
val float_array_get : expression -> expression -> expression
```
```ocaml
val float_array_set : 
  expression ->
  expression ->
  expression ->
  unit Code_generation.t
```
```ocaml
val check_is_float_array : expression -> expression
```
```ocaml
val gen_array_get : expression -> expression -> expression
```
```ocaml
val gen_array_set : 
  expression ->
  expression ->
  expression ->
  unit Code_generation.t
```
```ocaml
val array_length : expression -> expression
```
```ocaml
val float_array_length : expression -> expression
```
```ocaml
val gen_array_length : expression -> expression
```
```ocaml
val bytes_length : expression -> expression
```
```ocaml
val bytes_get : expression -> expression -> expression
```
```ocaml
val bytes_set : 
  expression ->
  expression ->
  expression ->
  unit Code_generation.t
```
```ocaml
val box_float : expression -> expression
```
```ocaml
val unbox_float : expression -> expression
```
```ocaml
val box_float32 : expression -> expression
```
```ocaml
val unbox_float32 : expression -> expression
```
```ocaml
val box_int32 : expression -> expression
```
```ocaml
val unbox_int32 : expression -> expression
```
```ocaml
val box_int64 : expression -> expression
```
```ocaml
val unbox_int64 : expression -> expression
```
```ocaml
val box_nativeint : expression -> expression
```
```ocaml
val unbox_nativeint : expression -> expression
```
