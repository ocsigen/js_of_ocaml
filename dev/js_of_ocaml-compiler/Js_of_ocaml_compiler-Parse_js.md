
# Module `Js_of_ocaml_compiler.Parse_js`

```ocaml
module Lexer : sig ... end
```
```ocaml
exception Parsing_error of Parse_info.t
```
```ocaml
val parse : [ `Script | `Module ] -> Lexer.t -> Javascript.program
```
```ocaml
val parse' : 
  [ `Script | `Module ] ->
  Lexer.t ->
  ((Js_token.Annot.t * Parse_info.t) list * Javascript.program) list
  * (Js_token.t * Loc.t) list
```
```ocaml
val parse_expr : Lexer.t -> Javascript.expression
```