
# Module `Js_of_ocaml_compiler.Parse_info`

```ocaml
type t = {
  src : string option;
  name : string option;
  col : int;
  line : int;
  idx : int;
}
```
```ocaml
val zero : t
```
```ocaml
val t_of_lexbuf : Stdlib.Lexing.lexbuf -> t
```
```ocaml
val t_of_pos : Stdlib.Lexing.position -> t
```
```ocaml
val start_position : t -> Stdlib.Lexing.position
```
```ocaml
val t_of_position : src:string option -> Stdlib.Lexing.position -> t
```
```ocaml
val to_string : t -> string
```