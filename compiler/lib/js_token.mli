(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
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
module Annot : sig
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

val info : t -> Parse_info.t

val to_string : t -> string

val to_string_extra : t -> string
