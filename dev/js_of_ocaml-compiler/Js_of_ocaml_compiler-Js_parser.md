
# Module `Js_of_ocaml_compiler.Js_parser`

```ocaml
type token = Js_token.token
```
```ocaml
exception Error
```
```ocaml
val standalone_expression : 
  (Stdlib.Lexing.lexbuf -> token) ->
  Stdlib.Lexing.lexbuf ->
  'tv_standalone_expression
```
```ocaml
val script : 
  (Stdlib.Lexing.lexbuf -> token) ->
  Stdlib.Lexing.lexbuf ->
  'tv_script
```
```ocaml
val module_ : 
  (Stdlib.Lexing.lexbuf -> token) ->
  Stdlib.Lexing.lexbuf ->
  'tv_module_
```
```ocaml
module MenhirInterpreter : sig ... end
```
```ocaml
module Incremental : sig ... end
```
```ocaml
module Tables : MenhirLib.TableFormat.TABLES with type token = token
```