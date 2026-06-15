
# Module `Js_of_ocaml_compiler.Flow`

```ocaml
module Info : sig ... end
```
```ocaml
val get_approx : 
  Info.t ->
  (Code.Var.Set.elt -> 'b) ->
  'b ->
  ('b -> 'b -> 'b) ->
  Code.Var.Tbl.key ->
  'b
```
```ocaml
val the_def_of : Info.t -> Code.prim_arg -> Code.expr option
```
```ocaml
val the_const_of : 
  eq:(Code.constant -> Code.constant -> bool) ->
  Info.t ->
  Code.prim_arg ->
  Code.constant option
```
```ocaml
val the_string_of : Info.t -> Code.prim_arg -> string option
```
```ocaml
val the_native_string_of : 
  Info.t ->
  Code.prim_arg ->
  Code.Native_string.t option
```
```ocaml
val the_block_contents_of : Info.t -> Code.prim_arg -> Code.Var.t array option
```
```ocaml
val the_int : Info.t -> Code.prim_arg -> Targetint.t option
```
```ocaml
val f : Code.program -> Code.program * Info.t
```
```ocaml
val the_shape_of : 
  return_values:Code.Var.Set.t Js_of_ocaml_compiler.Code.Var.Map.t ->
  pure:Pure_fun.t ->
  blocks:bool ->
  Info.t ->
  (Code.Var.t -> Shape.t) * (Code.Var.t -> Shape.t -> unit)
```
Returns `(get, set)` where `get x` computes the shape of variable `x` and `set x s` injects a shape for `x` into the internal cache (used to register shapes of fresh variables introduced by specialization). Results are memoized across calls to `get`.
