# Module `Js_of_ocaml_compiler.Flow_lexer`

```ocaml
module Lex_mode : sig ... end
```
```ocaml
module Parse_error : sig ... end
```
```ocaml
module Lex_env : sig ... end
```
```ocaml
val drop_line : Lex_env.t -> unit
```
```ocaml
val regexp : Lex_env.t -> Js_token.t * Loc.t
```
```ocaml
val token : Lex_env.t -> Js_token.t * Loc.t
```
```ocaml
val lex : Lex_env.t -> Js_token.t * Loc.t
```
```ocaml
val is_valid_identifier_name : string -> bool
```
