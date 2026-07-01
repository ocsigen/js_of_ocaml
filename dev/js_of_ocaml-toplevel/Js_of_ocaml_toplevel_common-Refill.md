
# Module `Js_of_ocaml_toplevel_common.Refill`

```ocaml
val lexbuf : 
  string ->
  int Stdlib.ref ->
  Stdlib.Format.formatter option ->
  bytes ->
  int ->
  int
```
`lexbuf s p ppf buffer len` is a `Lexing.from_function`\-compatible refill function feeding `s` to the lexer one line at a time, tracking the current position in `p`. When `ppf` is `Some _`, each chunk is echoed to it (this is how the toplevel echoes the phrase being read).

`s` is normalized so that it ends with `;;`: the incremental `Toploop.parse_toplevel_phrase` otherwise drops a trailing unterminated phrase.
