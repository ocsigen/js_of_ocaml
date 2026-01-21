(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2017 Hugo Heuzard
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

open! Stdlib

module Annot = struct
  type t = string * Primitive.t
end

type t =
  | T_NUMBER of (number_type * string)
  | T_BIGINT of (bigint_type * string)
  | T_STRING of (Utf8_string.t * int)
  | T_IDENTIFIER of (Utf8_string.t * string)
  | T_REGEXP of (Utf8_string.t * string)
  (* /pattern/flags *)
  (* Syntax *)
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
  (* Keywords *)
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
  (* Operators *)
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
  (* Extra tokens *)
  | T_ERROR of string
  | T_EOF
  | T_VIRTUAL_SEMICOLON
  | T_VIRTUAL_SEMICOLON_DO_WHILE
  | T_VIRTUAL_SEMICOLON_EXPORT_DEFAULT
  | T_DECR_NB
  | T_INCR_NB
  | T_LPAREN_ARROW
  | T_YIELDOFF_AWAITOFF
  | T_YIELDOFF_AWAITON
  | T_YIELDON_AWAITOFF
  | T_YIELDON_AWAITON
  | T_YIELD_AWAIT_POP
  | TAnnot of Annot.t
  | TComment of string
  | TCommentLineDirective of string

and number_type =
  | BINARY
  | LEGACY_OCTAL
  | LEGACY_NON_OCTAL (* NonOctalDecimalIntegerLiteral in Annex B *)
  | OCTAL
  | NORMAL

and bigint_type =
  | BIG_BINARY
  | BIG_OCTAL
  | BIG_NORMAL

type token = t

let to_string = function
  | T_YIELDOFF_AWAITOFF -> ""
  | T_YIELDOFF_AWAITON -> ""
  | T_YIELDON_AWAITOFF -> ""
  | T_YIELDON_AWAITON -> ""
  | T_YIELD_AWAIT_POP -> ""
  | TAnnot (s, _) -> s
  | T_ERROR s -> s
  | TComment s -> s
  | TCommentLineDirective s -> s
  | T_NUMBER (_, raw) -> raw
  | T_IDENTIFIER (Utf8 _, s) -> s
  | T_STRING (Utf8 s, _) -> Printf.sprintf "%S" s
  | T_REGEXP (Utf8 s, flags) -> Printf.sprintf "/%s/%s" s flags
  | T_DEBUGGER -> "debugger"
  | T_FUNCTION -> "function"
  | T_IF -> "if"
  | T_IN -> "in"
  | T_INSTANCEOF -> "instanceof"
  | T_RETURN -> "return"
  | T_SWITCH -> "switch"
  | T_THIS -> "this"
  | T_THROW -> "throw"
  | T_TRY -> "try"
  | T_VAR -> "var"
  | T_WHILE -> "while"
  | T_WITH -> "with"
  | T_NULL -> "null"
  | T_FALSE -> "false"
  | T_TRUE -> "true"
  | T_BREAK -> "break"
  | T_CASE -> "case"
  | T_CATCH -> "catch"
  | T_CONTINUE -> "continue"
  | T_DEFAULT -> "default"
  | T_DO -> "do"
  | T_FINALLY -> "finally"
  | T_FOR -> "for"
  | T_ELSE -> "else"
  | T_NEW -> "new"
  | T_LCURLY -> "{"
  | T_RCURLY -> "}"
  | T_LPAREN -> "("
  | T_RPAREN -> ")"
  | T_LBRACKET -> "["
  | T_RBRACKET -> "]"
  | T_SEMICOLON -> ";"
  | T_COMMA -> ","
  | T_PERIOD -> "."
  | T_RSHIFT3_ASSIGN -> ">>>="
  | T_RSHIFT_ASSIGN -> ">>="
  | T_LSHIFT_ASSIGN -> "<<="
  | T_BIT_XOR_ASSIGN -> "^="
  | T_BIT_OR_ASSIGN -> "|="
  | T_BIT_AND_ASSIGN -> "&="
  | T_MOD_ASSIGN -> "%="
  | T_DIV_ASSIGN -> "/="
  | T_MULT_ASSIGN -> "*="
  | T_MINUS_ASSIGN -> "-="
  | T_PLUS_ASSIGN -> "+="
  | T_ASSIGN -> "="
  | T_PLING -> "?"
  | T_COLON -> ":"
  | T_OR -> "||"
  | T_AND -> "&&"
  | T_BIT_OR -> "|"
  | T_BIT_XOR -> "^"
  | T_BIT_AND -> "&"
  | T_EQUAL -> "=="
  | T_NOT_EQUAL -> "!="
  | T_STRICT_EQUAL -> "==="
  | T_STRICT_NOT_EQUAL -> "!=="
  | T_LESS_THAN_EQUAL -> "<="
  | T_GREATER_THAN_EQUAL -> ">="
  | T_LESS_THAN -> "<"
  | T_GREATER_THAN -> ">"
  | T_LSHIFT -> "<<"
  | T_RSHIFT -> ">>"
  | T_RSHIFT3 -> ">>>"
  | T_PLUS -> "+"
  | T_MINUS -> "-"
  | T_DIV -> "/"
  | T_MULT -> "*"
  | T_MOD -> "%"
  | T_ELLIPSIS -> "..."
  | T_NOT -> "!"
  | T_BIT_NOT -> "~"
  | T_INCR -> "++"
  | T_DECR -> "--"
  | T_INCR_NB -> "++"
  | T_DECR_NB -> "--"
  | T_DELETE -> "delete"
  | T_TYPEOF -> "typeof"
  | T_VOID -> "void"
  | T_VIRTUAL_SEMICOLON -> ";"
  | T_VIRTUAL_SEMICOLON_DO_WHILE -> ";"
  | T_VIRTUAL_SEMICOLON_EXPORT_DEFAULT -> ";"
  | T_ARROW -> "=>"
  | T_AT -> "@"
  | T_POUND -> "#"
  | T_CONST -> "const"
  | T_LET -> "let"
  | T_CLASS -> "class"
  | T_EXTENDS -> "extends"
  | T_STATIC -> "static"
  | T_ENUM -> "enum"
  | T_EXPORT -> "export"
  | T_IMPORT -> "import"
  | T_SUPER -> "super"
  | T_IMPLEMENTS -> "implements"
  | T_INTERFACE -> "interface"
  | T_PACKAGE -> "package"
  | T_PRIVATE -> "private"
  | T_PROTECTED -> "protected"
  | T_PUBLIC -> "public"
  | T_YIELD -> "yield"
  | T_OF -> "of"
  | T_ASYNC -> "async"
  | T_AWAIT -> "await"
  | T_GET -> "get"
  | T_SET -> "set"
  | T_USING -> "using"
  | T_FROM -> "from"
  | T_TARGET -> "target"
  | T_META -> "meta"
  | T_EXP_ASSIGN -> "**="
  | T_NULLISH_ASSIGN -> "??="
  | T_AND_ASSIGN -> "&&="
  | T_OR_ASSIGN -> "||="
  | T_PLING_PERIOD -> "?."
  | T_PLING_PLING -> "??"
  | T_EXP -> "**"
  | T_EOF -> ""
  | T_BIGINT (_, raw) -> raw
  | T_LPAREN_ARROW -> "("
  | T_BACKQUOTE -> "`"
  | T_DOLLARCURLY -> "${"
  | T_ENCAPSED_STRING s -> s
  | T_AS -> "as"
  | T_DEFER -> "defer"

let to_string_extra x =
  to_string x
  ^
  match x with
  | T_IDENTIFIER _ -> " (identifier)"
  | T_INCR_NB -> " (INCR_NB)"
  | T_INCR -> " (INCR)"
  | T_DECR_NB -> " (DECR_NB)"
  | T_DECR -> " (DECR)"
  | T_VIRTUAL_SEMICOLON -> " (virtual)"
  | T_VIRTUAL_SEMICOLON_DO_WHILE -> " (virtual-do-while)"
  | T_VIRTUAL_SEMICOLON_EXPORT_DEFAULT -> " (virtual-export-default)"
  | TAnnot _ -> "(annot)"
  | T_ERROR _ -> "(error)"
  | T_LPAREN_ARROW -> "(arrow)"
  | T_ENCAPSED_STRING _ -> "(encaps)"
  | T_YIELDOFF_AWAITOFF -> "(YIELDOFF_AWAITOFF)"
  | T_YIELDOFF_AWAITON -> "(YIELDOFF_AWAITON)"
  | T_YIELDON_AWAITOFF -> "(YIELDON_AWAITOFF)"
  | T_YIELDON_AWAITON -> "(YIELDON_AWAITON)"
  | T_YIELD_AWAIT_POP -> "(YIELD_AWAIT_POP)"
  | T_EOF -> "(EOF)"
  | _ -> ""

let all_keywords =
  [ T_AS
  ; T_ASYNC
  ; T_AWAIT
  ; T_BREAK
  ; T_CASE
  ; T_CATCH
  ; T_CLASS
  ; T_CONST
  ; T_CONTINUE
  ; T_DEBUGGER
  ; T_DEFAULT
  ; T_DEFER
  ; T_DELETE
  ; T_DO
  ; T_ELSE
  ; T_ENUM
  ; T_EXPORT
  ; T_EXTENDS
  ; T_FALSE
  ; T_FINALLY
  ; T_FOR
  ; T_FROM
  ; T_FUNCTION
  ; T_GET
  ; T_IF
  ; T_IMPLEMENTS
  ; T_IMPORT
  ; T_IN
  ; T_INSTANCEOF
  ; T_INTERFACE
  ; T_LET
  ; T_META
  ; T_NEW
  ; T_NULL
  ; T_OF
  ; T_PACKAGE
  ; T_PRIVATE
  ; T_PROTECTED
  ; T_PUBLIC
  ; T_RETURN
  ; T_SET
  ; T_STATIC
  ; T_SUPER
  ; T_SWITCH
  ; T_TARGET
  ; T_THIS
  ; T_THROW
  ; T_TRUE
  ; T_TRY
  ; T_TYPEOF
  ; T_USING
  ; T_VAR
  ; T_VOID
  ; T_WHILE
  ; T_WITH
  ; T_YIELD
  ]

let is_reserved = function
  | "break" -> Some T_BREAK
  | "case" -> Some T_CASE
  | "catch" -> Some T_CATCH
  | "class" -> Some T_CLASS
  | "const" -> Some T_CONST
  | "continue" -> Some T_CONTINUE
  | "debugger" -> Some T_DEBUGGER
  | "default" -> Some T_DEFAULT
  | "delete" -> Some T_DELETE
  | "do" -> Some T_DO
  | "else" -> Some T_ELSE
  | "enum" -> Some T_ENUM
  | "export" -> Some T_EXPORT
  | "extends" -> Some T_EXTENDS
  | "false" -> Some T_FALSE
  | "finally" -> Some T_FINALLY
  | "for" -> Some T_FOR
  | "function" -> Some T_FUNCTION
  | "if" -> Some T_IF
  | "import" -> Some T_IMPORT
  | "in" -> Some T_IN
  | "instanceof" -> Some T_INSTANCEOF
  | "new" -> Some T_NEW
  | "null" -> Some T_NULL
  | "return" -> Some T_RETURN
  | "super" -> Some T_SUPER
  | "switch" -> Some T_SWITCH
  | "this" -> Some T_THIS
  | "throw" -> Some T_THROW
  | "true" -> Some T_TRUE
  | "try" -> Some T_TRY
  | "typeof" -> Some T_TYPEOF
  | "var" -> Some T_VAR
  | "void" -> Some T_VOID
  | "while" -> Some T_WHILE
  | "with" -> Some T_WITH
  (* contextually allowed as identifiers *)
  | "await" -> Some T_AWAIT
  | "yield" -> Some T_YIELD
  (* contextually disallowed as identifiers, in strict mode code*)
  (* reserved words in strict mode code. *)
  | "let" -> Some T_LET
  | "static" -> Some T_STATIC
  (* future reserved words in strict mode code. *)
  | "implements" -> Some T_IMPLEMENTS
  | "interface" -> Some T_INTERFACE
  | "package" -> Some T_PACKAGE
  | "private" -> Some T_PRIVATE
  | "protected" -> Some T_PROTECTED
  | "public" -> Some T_PUBLIC
  | _ -> None

let is_keyword = function
  (* Always allowed as identifier *)
  | "as" -> Some T_AS
  | "async" -> Some T_ASYNC
  | "from" -> Some T_FROM
  | "get" -> Some T_GET
  | "meta" -> Some T_META
  | "of" -> Some T_OF
  | "set" -> Some T_SET
  | "target" -> Some T_TARGET
  (* extentions *)
  | "defer" -> Some T_DEFER
  | "using" -> Some T_USING
  | s -> is_reserved s
