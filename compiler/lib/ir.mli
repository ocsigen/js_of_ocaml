(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2010 Jérôme Vouillon
 * Laboratoire PPS - CNRS Université Paris Diderot
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

(*
 * IR: A constrained intermediate language to model various language backends.
 *
 * IR has semantics similar to a very narrow subset of JavaScript, with the
 * addition of special nodes that track types, externs, and different shapes of
 * structures emitted by the ocaml compiler.
 *
 *)

open Stdlib

type array_literal = element_list

and element_list = expression option list

and binop =
  | Eq
  | StarEq
  | SlashEq
  | ModEq
  | PlusEq
  | MinusEq
  | Or
  | And
  | Bor
  | Bxor
  | Band
  | EqEq
  | NotEq
  | EqEqEq
  | NotEqEq
  | InstanceOf
  | Lsl
  | Lsr
  | Asr
  | Plus
  | Minus
  | Mul
  | Div
  | Mod
  | Lt
  | Le
  | Gt
  | Ge
  | FloatPlus
  | FloatMinus
  | FloatMul
  | FloatDiv
  | FloatMod
  | FloatLt
  | FloatLe
  | FloatGt
  | FloatGe
  | FloatEqEq
  | FloatNotEq

and unop =
  | Not
  | Neg
  | FloatNeg
  | Typeof
  | IsInt
  | ToInt
  | ToBool
  | IntToString
  | FloatToInt
  | IntToFloat
  | Void
  | Delete
  | Bnot

and arguments = expression list

and property_name_and_value_list = (Id.identifier * expression) list

and expression =
  | ERaw of string
  | ESeq of expression * expression
  | ECond of expression * expression * expression
  | EBin of binop * expression * expression
  | EUn of unop * expression
  | ECall of expression * arguments * Loc.t
  | ECopy of expression * Loc.t
  | EVar of Id.t
  | EFun of function_expression
  | EArityTest of expression
  | EStr of string * [ `Bytes | `Utf8 ]
  | EVectlength of expression
  | EArrAccess of expression * expression
  | EArrLen of expression
  | EArr of array_literal
  | EStructAccess of expression * int
  | EStruct of arguments
  | ETag of int * arguments
  | EDot of expression * Id.identifier
  | EAccess of expression * expression
  | ENew of expression * arguments option
  | EObj of property_name_and_value_list
  | EBool of bool
  | EFloat of float
  | EInt of int
  | EQuote of string
  | ERegexp of string * string option
  | ERuntime

and statement =
  | Block of block
  | Variable_statement of variable_declaration list
  | Empty_statement
  | Expression_statement of expression
  | If_statement of expression * (statement * Loc.t) * (statement * Loc.t) option
  | Loop_statement of statement * Loc.t
  | Continue_statement of Javascript.Label.t option
  | Break_statement of Javascript.Label.t option
  | Return_statement of expression option
  | Labelled_statement of Javascript.Label.t * (statement * Loc.t)
  | Switch_statement of expression * case_clause list * statement_list
  | Throw_statement of expression
  | Try_statement of block * (Id.t * block)
  | Debugger_statement

and block = statement_list

and statement_list = (statement * Loc.t) list

and variable_declaration = Id.t * initialiser option

and case_clause = expression * statement_list

and initialiser = expression * Loc.t

and var_info = (int StringMap.t * int Code.Var.Map.t) option

and function_declaration = Id.t * formal_parameter_list * function_body * Loc.t

and function_expression = Id.t option * formal_parameter_list * function_body * Loc.t

and formal_parameter_list = Id.t list

and function_body = source_elements

and program = source_elements

and source_elements = (source_element * Loc.t) list

and source_element =
  | Statement of statement
  | Function_declaration of function_declaration
