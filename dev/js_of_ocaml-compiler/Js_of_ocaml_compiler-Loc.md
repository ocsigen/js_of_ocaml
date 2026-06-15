
# Module `Js_of_ocaml_compiler.Loc`

```ocaml
type line = {
  pos_fname : string;
  pos_lnum : int;
  pos_bol : int;
}
```
```ocaml
type t = 
  | SameLine of {
    line : line;
    cnum_start : int;
    offset : int;
  }
  | MultiLine of {
    line_start : line;
    cnum_start : int;
    line_end : line;
    offset : int;
  }
```
```ocaml
val filename : t -> string
```
```ocaml
val line' : t -> line
```
```ocaml
val line_end' : t -> line
```
```ocaml
val cnum : t -> int
```
```ocaml
val line : t -> int
```
```ocaml
val line_end : t -> int
```
```ocaml
val column : t -> int
```
```ocaml
val dummy_line : line
```
```ocaml
val dummy : t
```
```ocaml
val create : 
  ?last_line:line ->
  Stdlib.Lexing.position ->
  Stdlib.Lexing.position ->
  t
```
```ocaml
val p1 : t -> Stdlib.Lexing.position
```
```ocaml
val p2 : t -> Stdlib.Lexing.position
```