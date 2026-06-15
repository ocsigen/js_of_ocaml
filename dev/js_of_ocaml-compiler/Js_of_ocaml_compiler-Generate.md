
# Module `Js_of_ocaml_compiler.Generate`

```ocaml
val f : 
  Code.program ->
  exported_runtime:bool ->
  live_vars:Deadcode.variable_uses ->
  trampolined_calls:Effects.trampolined_calls ->
  in_cps:Effects.in_cps ->
  should_export:bool ->
  warn_on_unhandled_effect:bool ->
  deadcode_sentinel:Code.Var.t ->
  Javascript.program
```
```ocaml
val init : unit -> unit
```