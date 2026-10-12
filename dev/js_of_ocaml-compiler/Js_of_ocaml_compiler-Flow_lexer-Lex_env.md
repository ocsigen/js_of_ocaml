# Module `Flow_lexer.Lex_env`

```ocaml
type t
```
```ocaml
val create : Sedlexing.lexbuf -> t
```
```ocaml
val take_errors : t -> (Loc.t * Parse_error.t) list
```
The errors of the last token, and forget them
