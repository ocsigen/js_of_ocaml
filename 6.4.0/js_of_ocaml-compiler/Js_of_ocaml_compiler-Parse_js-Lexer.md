
# Module `Parse_js.Lexer`

```ocaml
type t
```
```ocaml
type error
```
```ocaml
val of_file : string -> t
```
```ocaml
val of_string : 
  ?report_error:(error -> unit) ->
  ?pos:Stdlib.Lexing.position ->
  ?filename:string ->
  string ->
  t
```
```ocaml
val print_error : error -> unit
```
```ocaml
val of_channel : Stdlib.in_channel -> t
```