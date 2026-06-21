
# Module `Flow_lexer.Lex_result`

```ocaml
type t
```
```ocaml
val token : t -> Js_token.t
```
```ocaml
val loc : t -> Loc.t
```
```ocaml
val errors : t -> (Loc.t * Parse_error.t) list
```