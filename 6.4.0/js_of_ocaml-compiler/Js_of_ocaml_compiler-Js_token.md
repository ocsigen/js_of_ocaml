
# Module `Js_of_ocaml_compiler.Js_token`

```ocaml
module Annot : sig ... end
```
```ocaml
type t = 
  | T_NUMBER of number_type * string
  | T_BIGINT of bigint_type * string
  | T_STRING of Stdlib.Utf8_string.t * int
  | T_IDENTIFIER of Stdlib.Utf8_string.t * string
  | T_REGEXP of Stdlib.Utf8_string.t * string
  | T_LCURLY
  | T_RCURLY
  | T_LPAREN
  | T_RPAREN
  | T_LBRACKET
  | T_RBRACKET
  | T_SEMICOLON
  | T_COMMA
  | T_PERIOD
  | T_ARROW
  | T_ELLIPSIS
  | T_AT
  | T_POUND
  | T_ACCESSOR
  | T_AS
  | T_ASYNC
  | T_AWAIT
  | T_BREAK
  | T_CASE
  | T_CATCH
  | T_CLASS
  | T_CONST
  | T_CONTINUE
  | T_DEBUGGER
  | T_DEFAULT
  | T_DEFER
  | T_DELETE
  | T_DO
  | T_ELSE
  | T_ENUM
  | T_EXPORT
  | T_EXTENDS
  | T_FALSE
  | T_FINALLY
  | T_FOR
  | T_FROM
  | T_FUNCTION
  | T_GET
  | T_IF
  | T_IMPLEMENTS
  | T_IMPORT
  | T_IN
  | T_INSTANCEOF
  | T_INTERFACE
  | T_LET
  | T_META
  | T_NEW
  | T_NULL
  | T_OF
  | T_PACKAGE
  | T_PRIVATE
  | T_PROTECTED
  | T_PUBLIC
  | T_RETURN
  | T_SET
  | T_STATIC
  | T_SUPER
  | T_SWITCH
  | T_TARGET
  | T_THIS
  | T_THROW
  | T_TRUE
  | T_TRY
  | T_TYPEOF
  | T_USING
  | T_VAR
  | T_VOID
  | T_WHILE
  | T_WITH
  | T_YIELD
  | T_RSHIFT3_ASSIGN
  | T_RSHIFT_ASSIGN
  | T_LSHIFT_ASSIGN
  | T_BIT_XOR_ASSIGN
  | T_BIT_OR_ASSIGN
  | T_BIT_AND_ASSIGN
  | T_MOD_ASSIGN
  | T_DIV_ASSIGN
  | T_MULT_ASSIGN
  | T_EXP_ASSIGN
  | T_MINUS_ASSIGN
  | T_PLUS_ASSIGN
  | T_NULLISH_ASSIGN
  | T_AND_ASSIGN
  | T_OR_ASSIGN
  | T_ASSIGN
  | T_PLING_PERIOD
  | T_PLING_PLING
  | T_PLING
  | T_COLON
  | T_OR
  | T_AND
  | T_BIT_OR
  | T_BIT_XOR
  | T_BIT_AND
  | T_EQUAL
  | T_NOT_EQUAL
  | T_STRICT_EQUAL
  | T_STRICT_NOT_EQUAL
  | T_LESS_THAN_EQUAL
  | T_GREATER_THAN_EQUAL
  | T_LESS_THAN
  | T_GREATER_THAN
  | T_LSHIFT
  | T_RSHIFT
  | T_RSHIFT3
  | T_PLUS
  | T_MINUS
  | T_DIV
  | T_MULT
  | T_EXP
  | T_MOD
  | T_NOT
  | T_BIT_NOT
  | T_INCR
  | T_DECR
  | T_BACKQUOTE
  | T_DOLLARCURLY
  | T_ENCAPSED_STRING of string
  | T_ERROR of string
  | T_EOF
  | T_VIRTUAL_SEMICOLON
  | T_VIRTUAL_SEMICOLON_DO_WHILE
  | T_VIRTUAL_SEMICOLON_EXPORT_DEFAULT
  | T_DECR_NB
  | T_INCR_NB
  | T_LPAREN_ARROW
  | T_YIELDOFF
  | T_YIELDON
  | T_AWAITOFF
  | T_AWAITON
  | T_YIELD_AWAIT_POP
  | TAnnot of Annot.t
  | TComment of string
  | TCommentLineDirective of string
```
```ocaml
and number_type = 
  | BINARY
  | LEGACY_OCTAL
  | LEGACY_NON_OCTAL
  | OCTAL
  | NORMAL
```
```ocaml
and bigint_type = 
  | BIG_BINARY
  | BIG_OCTAL
  | BIG_NORMAL
```
```ocaml
type token = t
```
```ocaml
val to_string : t -> string
```
```ocaml
val to_string_extra : t -> string
```
```ocaml
val is_reserved : string -> t option
```
```ocaml
val is_keyword : string -> t option
```
```ocaml
val all_keywords : t list
```