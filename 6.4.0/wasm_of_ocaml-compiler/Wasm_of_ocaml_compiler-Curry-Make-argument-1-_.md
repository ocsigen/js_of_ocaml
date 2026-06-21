
# Parameter `Make._`

```ocaml
type expression = Code_generation.expression
```
```ocaml
module Memory : sig ... end
```
```ocaml
module Type : sig ... end
```
```ocaml
module Value : sig ... end
```
```ocaml
module Constant : sig ... end
```
```ocaml
module Closure : sig ... end
```
```ocaml
module Math : sig ... end
```
```ocaml
module Bigarray : sig ... end
```
```ocaml
val internal_primitives : 
  (string
   * Js_of_ocaml_compiler.Primitive.kind
   * ((Js_of_ocaml_compiler.Code.prim_arg -> expression) ->
   Js_of_ocaml_compiler.Code.prim_arg list ->
   expression))
    list
```
```ocaml
val handle_exceptions : 
  result_typ:Wasm_ast.value_type list ->
  fall_through:'a ->
  context:([> `Catch | `Skip ] as 'b) list ->
  (result_typ:Wasm_ast.value_type list ->
    fall_through:[> `Skip ] ->
    context:'b list ->
    unit Code_generation.t) ->
  Wasm_ast.var ->
  (result_typ:Wasm_ast.value_type list ->
    fall_through:'a ->
    context:'b list ->
    unit Code_generation.t) ->
  unit Code_generation.t
```
```ocaml
val post_process_function_body : 
  param_names:Wasm_ast.var list ->
  locals:(Wasm_ast.var * Wasm_ast.value_type) list ->
  Wasm_ast.instruction list ->
  (Wasm_ast.var * Wasm_ast.value_type) list * Wasm_ast.instruction list
```
```ocaml
val entry_point : 
  toplevel_fun:Wasm_ast.var ->
  Wasm_ast.func_type * Wasm_ast.var list * unit Code_generation.t
```