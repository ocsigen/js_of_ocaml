
# Module `Deriving_Json_lexer`

```ocaml
type lexbuf
```
```ocaml
val init_lexer : ?buf:Stdlib.Buffer.t -> Stdlib.Lexing.lexbuf -> lexbuf
```
Create a fresh lexbuf record.

```ocaml
val tag_error : typename:string -> lexbuf -> 'a
```
```ocaml
val read_int : lexbuf -> int
```
```ocaml
val read_bounded_int : ?min:int -> max:int -> lexbuf -> int
```
```ocaml
val read_tag_1 : int -> lexbuf -> int
```
```ocaml
val read_tag_2 : int -> int -> lexbuf -> int
```
```ocaml
val read_int32 : lexbuf -> int32
```
```ocaml
val read_int64 : lexbuf -> int64
```
```ocaml
val read_number : lexbuf -> float
```
```ocaml
val read_string : lexbuf -> string
```
```ocaml
val read_case : lexbuf -> [ `Cst of int | `NCst of int ]
```
```ocaml
val read_vcase : lexbuf -> [ `Cst of int | `NCst of int ]
```
```ocaml
val read_comma : lexbuf -> unit
```
```ocaml
val read_lbracket : lexbuf -> unit
```
```ocaml
val read_rbracket : lexbuf -> unit
```
```ocaml
val read_comma_or_rbracket : lexbuf -> [ `Comma | `RBracket ]
```