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
  | T_WITH
  | T_WHILE
  | T_VOID
  | T_VIRTUAL_SEMICOLON
  | T_VAR
  | T_TYPEOF
  | T_TRY
  | T_TRUE
  | T_THROW
  | T_THIS
  | T_SWITCH
  | T_STRING of (string * int)
  | T_STRICT_NOT_EQUAL
  | T_STRICT_EQUAL
  | T_SEMICOLON
  | T_RSHIFT_ASSIGN
  | T_RSHIFT3_ASSIGN
  | T_RSHIFT3
  | T_RSHIFT
  | T_RPAREN
  | T_RETURN
  | T_REGEX of string
  | T_RCURLY
  | T_RBRACKET
  | T_PLUS_ASSIGN
  | T_PLUS
  | T_PLING
  | T_PERIOD
  | T_OR
  | T_NUMBER of string
  | T_NULL
  | T_NOT_EQUAL
  | T_NOT
  | T_SPREAD
  | T_NEW
  | T_MULT_ASSIGN
  | T_MULT
  | T_MOD_ASSIGN
  | T_MOD
  | T_MINUS_ASSIGN
  | T_MINUS
  | T_LSHIFT_ASSIGN
  | T_LSHIFT
  | T_LPAREN
  | T_LESS_THAN_EQUAL
  | T_LESS_THAN
  | T_LCURLY
  | T_LBRACKET
  | T_INSTANCEOF
  | T_INCR_NB
  | T_INCR
  | T_IN
  | T_IF
  | T_IDENTIFIER of string
  | T_GREATER_THAN_EQUAL
  | T_GREATER_THAN
  | T_FUNCTION
  | T_FOR
  | T_FINALLY
  | T_FALSE
  | T_EQUAL
  | T_ELSE
  | T_DO
  | T_DIV_ASSIGN
  | T_DIV
  | T_DELETE
  | T_DEFAULT
  | T_DECR_NB
  | T_DECR
  | T_CONTINUE
  | T_COMMA
  | T_COLON
  | T_CATCH
  | T_CASE
  | T_BREAK
  | T_BIT_XOR_ASSIGN
  | T_BIT_XOR
  | T_BIT_OR_ASSIGN
  | T_BIT_OR
  | T_BIT_NOT
  | T_BIT_AND_ASSIGN
  | T_BIT_AND
  | T_ASSIGN
  | T_AND
  | T_DEBUGGER
  | TUnknown of string
  | TComment of string
  | TCommentLineDirective of string
  | TAnnot of Annot.t
  | EOF

type token = t

val to_string : t -> string

val to_string_extra : t -> string
