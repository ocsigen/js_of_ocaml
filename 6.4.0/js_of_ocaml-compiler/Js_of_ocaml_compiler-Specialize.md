
# Module `Js_of_ocaml_compiler.Specialize`

```ocaml
val f : 
  shape:(Code.Var.t -> Shape.t) ->
  set_shape:(Code.Var.t -> Shape.t -> unit) ->
  update_def:(Code.Var.t -> Code.expr -> unit) ->
  Code.program ->
  Code.program
```
```ocaml
val switches : Code.program -> Code.program
```