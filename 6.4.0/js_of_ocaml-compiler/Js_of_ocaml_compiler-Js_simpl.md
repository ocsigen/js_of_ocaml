
# Module `Js_of_ocaml_compiler.Js_simpl`

```ocaml
val if_statement : 
  function_end:(unit -> Javascript.location) ->
  Javascript.expression ->
  Javascript.location ->
  (Javascript.statement * Javascript.location) ->
  bool ->
  (Javascript.statement * Javascript.location) ->
  bool ->
  (Javascript.statement * Javascript.location) list
```
```ocaml
val block : 
  (Javascript.statement * Javascript.location) list ->
  Javascript.statement * Javascript.location
```
```ocaml
val unblock : 
  (Javascript.statement * Javascript.location) ->
  (Javascript.statement * Javascript.location) list
```
```ocaml
val function_body : 
  (Javascript.statement * Javascript.location) list ->
  (Javascript.statement * Javascript.location) list
```