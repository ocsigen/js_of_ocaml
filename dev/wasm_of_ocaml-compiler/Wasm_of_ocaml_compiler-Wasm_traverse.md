# Module `Wasm_of_ocaml_compiler.Wasm_traverse`

Shallow traversals of the Wasm AST.

These functions apply `expression` to each direct sub-expression of a node and `instructions` to each instruction list it contains, in evaluation order: operands from left to right, the arguments of `call_ref` before the function, the condition of an `if` before its branches, the instructions of a `Seq` before its expression. A pass handles the constructs it cares about and delegates the others to these functions.

```ocaml
val iter_expression : 
  expression:(Wasm_ast.expression -> unit) ->
  instructions:(Wasm_ast.instruction list -> unit) ->
  Wasm_ast.expression ->
  unit
```
```ocaml
val iter_instruction : 
  expression:(Wasm_ast.expression -> unit) ->
  instructions:(Wasm_ast.instruction list -> unit) ->
  Wasm_ast.instruction ->
  unit
```
```ocaml
val map_expression : 
  expression:(Wasm_ast.expression -> Wasm_ast.expression) ->
  instructions:(Wasm_ast.instruction list -> Wasm_ast.instruction list) ->
  Wasm_ast.expression ->
  Wasm_ast.expression
```
```ocaml
val map_instruction : 
  expression:(Wasm_ast.expression -> Wasm_ast.expression) ->
  instructions:(Wasm_ast.instruction list -> Wasm_ast.instruction list) ->
  Wasm_ast.instruction ->
  Wasm_ast.instruction
```
```ocaml
val fold_map_expression : 
  expression:('a -> Wasm_ast.expression -> Wasm_ast.expression * 'a) ->
  instructions:
    ('a -> Wasm_ast.instruction list -> Wasm_ast.instruction list * 'a) ->
  'a ->
  Wasm_ast.expression ->
  Wasm_ast.expression * 'a
```
The accumulator is threaded through the children in evaluation order, including through both branches of an `if`: a pass for which branches must not see each other handles `if` itself.

```ocaml
val fold_map_instruction : 
  expression:('a -> Wasm_ast.expression -> Wasm_ast.expression * 'a) ->
  instructions:
    ('a -> Wasm_ast.instruction list -> Wasm_ast.instruction list * 'a) ->
  'a ->
  Wasm_ast.instruction ->
  Wasm_ast.instruction * 'a
```
```ocaml
val iter_locals_expression : 
  read:(Wasm_ast.var -> unit) ->
  write:(Wasm_ast.var -> unit) ->
  Wasm_ast.expression ->
  unit
```
Same as `iter_locals`, for an expression.

```ocaml
val iter_locals : 
  read:(Wasm_ast.var -> unit) ->
  write:(Wasm_ast.var -> unit) ->
  Wasm_ast.instruction list ->
  unit
```
Deep traversal: call `read` on the variable of each `local.get` and `write` on the variable of each `local.set` or `local.tee` in the whole instruction list, in evaluation order.
