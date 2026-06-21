
# Module `Js_of_ocaml_compiler.Annot_parser`

```ocaml
type token = 
  | TWeakdef
  | TVersion
  | TVNum of string
  | TRequires
  | TProvides
  | TOTHER of string
  | TIf
  | TIdent_percent of string
  | TIdent of string
  | TDeprecated of string
  | TComma
  | TColon
  | TBang
  | TAlways
  | TAlias
  | TA_Shallow
  | TA_Pure
  | TA_Object_literal
  | TA_Mutator
  | TA_Mutable
  | TA_Const
  | RPARENT
  | LT
  | LPARENT
  | LE
  | GT
  | GE
  | EQ
  | EOL
  | EOF
```
```ocaml
exception Error
```
```ocaml
val annot : 
  (Stdlib.Lexing.lexbuf -> token) ->
  Stdlib.Lexing.lexbuf ->
  Primitive.t
```