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
  type t = string * Parse_info.t * Primitive.t
end

type t =
  | T_WITH of Parse_info.t
  | T_WHILE of Parse_info.t
  | T_VOID of Parse_info.t
  | T_VIRTUAL_SEMICOLON of Parse_info.t
  | T_VAR of Parse_info.t
  | T_TYPEOF of Parse_info.t
  | T_TRY of Parse_info.t
  | T_TRUE of Parse_info.t
  | T_THROW of Parse_info.t
  | T_THIS of Parse_info.t
  | T_SWITCH of Parse_info.t
  | T_STRING of (string * Parse_info.t * int)
  | T_STRICT_NOT_EQUAL of Parse_info.t
  | T_STRICT_EQUAL of Parse_info.t
  | T_SEMICOLON of Parse_info.t
  | T_RSHIFT_ASSIGN of Parse_info.t
  | T_RSHIFT3_ASSIGN of Parse_info.t
  | T_RSHIFT3 of Parse_info.t
  | T_RSHIFT of Parse_info.t
  | T_RPAREN of Parse_info.t
  | T_RETURN of Parse_info.t
  | T_REGEX of (string * Parse_info.t)
  | T_RCURLY of Parse_info.t
  | T_RBRACKET of Parse_info.t
  | T_PLUS_ASSIGN of Parse_info.t
  | T_PLUS of Parse_info.t
  | T_PLING of Parse_info.t
  | T_PERIOD of Parse_info.t
  | T_OR of Parse_info.t
  | T_NUMBER of (string * Parse_info.t)
  | T_NULL of Parse_info.t
  | T_NOT_EQUAL of Parse_info.t
  | T_NOT of Parse_info.t
  | T_SPREAD of Parse_info.t
  | T_NEW of Parse_info.t
  | T_MULT_ASSIGN of Parse_info.t
  | T_MULT of Parse_info.t
  | T_MOD_ASSIGN of Parse_info.t
  | T_MOD of Parse_info.t
  | T_MINUS_ASSIGN of Parse_info.t
  | T_MINUS of Parse_info.t
  | T_LSHIFT_ASSIGN of Parse_info.t
  | T_LSHIFT of Parse_info.t
  | T_LPAREN of Parse_info.t
  | T_LESS_THAN_EQUAL of Parse_info.t
  | T_LESS_THAN of Parse_info.t
  | T_LCURLY of Parse_info.t
  | T_LBRACKET of Parse_info.t
  | T_INSTANCEOF of Parse_info.t
  | T_INCR_NB of Parse_info.t
  | T_INCR of Parse_info.t
  | T_IN of Parse_info.t
  | T_IF of Parse_info.t
  | T_IDENTIFIER of (string * Parse_info.t)
  | T_GREATER_THAN_EQUAL of Parse_info.t
  | T_GREATER_THAN of Parse_info.t
  | T_FUNCTION of Parse_info.t
  | T_FOR of Parse_info.t
  | T_FINALLY of Parse_info.t
  | T_FALSE of Parse_info.t
  | T_EQUAL of Parse_info.t
  | T_ELSE of Parse_info.t
  | T_DO of Parse_info.t
  | T_DIV_ASSIGN of Parse_info.t
  | T_DIV of Parse_info.t
  | T_DELETE of Parse_info.t
  | T_DEFAULT of Parse_info.t
  | T_DECR_NB of Parse_info.t
  | T_DECR of Parse_info.t
  | T_CONTINUE of Parse_info.t
  | T_COMMA of Parse_info.t
  | T_COLON of Parse_info.t
  | T_CATCH of Parse_info.t
  | T_CASE of Parse_info.t
  | T_BREAK of Parse_info.t
  | T_BIT_XOR_ASSIGN of Parse_info.t
  | T_BIT_XOR of Parse_info.t
  | T_BIT_OR_ASSIGN of Parse_info.t
  | T_BIT_OR of Parse_info.t
  | T_BIT_NOT of Parse_info.t
  | T_BIT_AND_ASSIGN of Parse_info.t
  | T_BIT_AND of Parse_info.t
  | T_ASSIGN of Parse_info.t
  | T_AND of Parse_info.t
  | T_DEBUGGER of Parse_info.t
  | TUnknown of (string * Parse_info.t)
  | TComment of (string * Parse_info.t)
  | TCommentLineDirective of (string * Parse_info.t)
  | TAnnot of Annot.t
  | EOF of Parse_info.t

type token = t

let info = function
  | TAnnot (_, ii, _) -> ii
  | TUnknown (_, ii) -> ii
  | TComment (_, ii) -> ii
  | TCommentLineDirective (_, ii) -> ii
  | EOF ii -> ii
  | T_DEBUGGER ii -> ii
  | T_NUMBER (_, ii) -> ii
  | T_IDENTIFIER (_, ii) -> ii
  | T_STRING (_, ii, _) -> ii
  | T_REGEX (_, ii) -> ii
  | T_FUNCTION ii -> ii
  | T_IF ii -> ii
  | T_IN ii -> ii
  | T_INSTANCEOF ii -> ii
  | T_RETURN ii -> ii
  | T_SWITCH ii -> ii
  | T_THIS ii -> ii
  | T_THROW ii -> ii
  | T_TRY ii -> ii
  | T_VAR ii -> ii
  | T_WHILE ii -> ii
  | T_WITH ii -> ii
  | T_NULL ii -> ii
  | T_FALSE ii -> ii
  | T_TRUE ii -> ii
  | T_BREAK ii -> ii
  | T_CASE ii -> ii
  | T_CATCH ii -> ii
  | T_CONTINUE ii -> ii
  | T_DEFAULT ii -> ii
  | T_DO ii -> ii
  | T_FINALLY ii -> ii
  | T_FOR ii -> ii
  | T_ELSE ii -> ii
  | T_NEW ii -> ii
  | T_LCURLY ii -> ii
  | T_RCURLY ii -> ii
  | T_LPAREN ii -> ii
  | T_RPAREN ii -> ii
  | T_LBRACKET ii -> ii
  | T_RBRACKET ii -> ii
  | T_SEMICOLON ii -> ii
  | T_COMMA ii -> ii
  | T_PERIOD ii -> ii
  | T_RSHIFT3_ASSIGN ii -> ii
  | T_RSHIFT_ASSIGN ii -> ii
  | T_LSHIFT_ASSIGN ii -> ii
  | T_BIT_XOR_ASSIGN ii -> ii
  | T_BIT_OR_ASSIGN ii -> ii
  | T_BIT_AND_ASSIGN ii -> ii
  | T_MOD_ASSIGN ii -> ii
  | T_DIV_ASSIGN ii -> ii
  | T_MULT_ASSIGN ii -> ii
  | T_MINUS_ASSIGN ii -> ii
  | T_PLUS_ASSIGN ii -> ii
  | T_ASSIGN ii -> ii
  | T_PLING ii -> ii
  | T_COLON ii -> ii
  | T_OR ii -> ii
  | T_AND ii -> ii
  | T_BIT_OR ii -> ii
  | T_BIT_XOR ii -> ii
  | T_BIT_AND ii -> ii
  | T_EQUAL ii -> ii
  | T_NOT_EQUAL ii -> ii
  | T_STRICT_EQUAL ii -> ii
  | T_STRICT_NOT_EQUAL ii -> ii
  | T_LESS_THAN_EQUAL ii -> ii
  | T_GREATER_THAN_EQUAL ii -> ii
  | T_LESS_THAN ii -> ii
  | T_GREATER_THAN ii -> ii
  | T_LSHIFT ii -> ii
  | T_RSHIFT ii -> ii
  | T_RSHIFT3 ii -> ii
  | T_PLUS ii -> ii
  | T_MINUS ii -> ii
  | T_DIV ii -> ii
  | T_MULT ii -> ii
  | T_MOD ii -> ii
  | T_SPREAD ii -> ii
  | T_NOT ii -> ii
  | T_BIT_NOT ii -> ii
  | T_INCR ii -> ii
  | T_DECR ii -> ii
  | T_INCR_NB ii -> ii
  | T_DECR_NB ii -> ii
  | T_DELETE ii -> ii
  | T_TYPEOF ii -> ii
  | T_VOID ii -> ii
  | T_VIRTUAL_SEMICOLON ii -> ii

let to_string = function
  | TAnnot (s, _, _) -> s
  | TUnknown (s, _) -> s
  | TComment (s, _) -> s
  | TCommentLineDirective (s, _) -> s
  | T_NUMBER (s, _) -> s
  | T_IDENTIFIER (s, _) -> s
  | T_STRING (s, _, _) -> Printf.sprintf "%S" s
  | T_REGEX (s, _) -> s
  | EOF _ -> ""
  | T_DEBUGGER _ -> "debugger"
  | T_FUNCTION _ -> "function"
  | T_IF _ -> "if"
  | T_IN _ -> "in"
  | T_INSTANCEOF _ -> "instanceof"
  | T_RETURN _ -> "return"
  | T_SWITCH _ -> "switch"
  | T_THIS _ -> "this"
  | T_THROW _ -> "throw"
  | T_TRY _ -> "try"
  | T_VAR _ -> "var"
  | T_WHILE _ -> "while"
  | T_WITH _ -> "with"
  | T_NULL _ -> "null"
  | T_FALSE _ -> "false"
  | T_TRUE _ -> "true"
  | T_BREAK _ -> "break"
  | T_CASE _ -> "case"
  | T_CATCH _ -> "catch"
  | T_CONTINUE _ -> "continue"
  | T_DEFAULT _ -> "default"
  | T_DO _ -> "do"
  | T_FINALLY _ -> "finally"
  | T_FOR _ -> "for"
  | T_ELSE _ -> "else"
  | T_NEW _ -> "new"
  | T_LCURLY _ -> "{"
  | T_RCURLY _ -> "}"
  | T_LPAREN _ -> "("
  | T_RPAREN _ -> ")"
  | T_LBRACKET _ -> "["
  | T_RBRACKET _ -> "]"
  | T_SEMICOLON _ -> ";"
  | T_COMMA _ -> ","
  | T_PERIOD _ -> "."
  | T_RSHIFT3_ASSIGN _ -> ">>>="
  | T_RSHIFT_ASSIGN _ -> ">>="
  | T_LSHIFT_ASSIGN _ -> "<<="
  | T_BIT_XOR_ASSIGN _ -> "^="
  | T_BIT_OR_ASSIGN _ -> "|="
  | T_BIT_AND_ASSIGN _ -> "&="
  | T_MOD_ASSIGN _ -> "%="
  | T_DIV_ASSIGN _ -> "/="
  | T_MULT_ASSIGN _ -> "*="
  | T_MINUS_ASSIGN _ -> "-="
  | T_PLUS_ASSIGN _ -> "+="
  | T_ASSIGN _ -> "="
  | T_PLING _ -> "?"
  | T_COLON _ -> ":"
  | T_OR _ -> "||"
  | T_AND _ -> "&&"
  | T_BIT_OR _ -> "|"
  | T_BIT_XOR _ -> "^"
  | T_BIT_AND _ -> "&"
  | T_EQUAL _ -> "=="
  | T_NOT_EQUAL _ -> "!="
  | T_STRICT_EQUAL _ -> "==="
  | T_STRICT_NOT_EQUAL _ -> "!=="
  | T_LESS_THAN_EQUAL _ -> "<="
  | T_GREATER_THAN_EQUAL _ -> ">="
  | T_LESS_THAN _ -> "<"
  | T_GREATER_THAN _ -> ">"
  | T_LSHIFT _ -> "<<"
  | T_RSHIFT _ -> ">>"
  | T_RSHIFT3 _ -> ">>>"
  | T_PLUS _ -> "+"
  | T_MINUS _ -> "-"
  | T_DIV _ -> "/"
  | T_MULT _ -> "*"
  | T_MOD _ -> "%"
  | T_SPREAD _ -> ".."
  | T_NOT _ -> "!"
  | T_BIT_NOT _ -> "~"
  | T_INCR _ -> "++"
  | T_DECR _ -> "--"
  | T_INCR_NB _ -> "++"
  | T_DECR_NB _ -> "--"
  | T_DELETE _ -> "delete"
  | T_TYPEOF _ -> "typeof"
  | T_VOID _ -> "void"
  | T_VIRTUAL_SEMICOLON _ -> ";"

let to_string_extra x =
  to_string x
  ^
  match x with
  | T_IDENTIFIER _ -> " (identifier)"
  | T_INCR_NB _ -> " (INCR_NB)"
  | T_INCR _ -> " (INCR)"
  | T_DECR_NB _ -> " (DECR_NB)"
  | T_DECR _ -> " (DECR)"
  | T_VIRTUAL_SEMICOLON _ -> " (virtual)"
  | TAnnot _ -> "(annot)"
  | _ -> ""
