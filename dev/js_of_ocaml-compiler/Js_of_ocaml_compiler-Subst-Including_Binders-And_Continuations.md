
# Module `Including_Binders.And_Continuations`

```ocaml
val block : 
  Code.Addr.t Js_of_ocaml_compiler.Code.Addr.Map.t ->
  (Code.Var.t -> Code.Var.t) ->
  Code.block ->
  Code.block
```
Same as `Including_Binders.block`, but also substitutes continuation addresses.
