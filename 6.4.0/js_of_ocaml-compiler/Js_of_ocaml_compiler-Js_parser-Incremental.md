
# Module `Js_parser.Incremental`

```ocaml
val standalone_expression : 
  Stdlib.Lexing.position ->
  'tv_standalone_expression MenhirInterpreter.checkpoint
```
```ocaml
val script : Stdlib.Lexing.position -> 'tv_script MenhirInterpreter.checkpoint
```
```ocaml
val module_ : 
  Stdlib.Lexing.position ->
  'tv_module_ MenhirInterpreter.checkpoint
```