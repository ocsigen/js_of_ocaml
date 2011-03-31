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
   variable_declaration_list_no_in
   variable_declaration_no_in
   initialiser_no_in
...


*)
type foo = unit

(* A.3 Expressions *)

and array_litteral = element_list

and element_list = expression option list

and binop =
    Eq | StarEq | SlashEq | ModEq | PlusEq | MinusEq (*... XXX*)
  | Or | And | Bor | Bxor | Band
  | EqEq | NotEq | EqEqEq | NotEqEq
  | Lt | Le | InstanceOf
  | Lsl | Lsr | Asr
  | Plus | Minus
  | Mul | Div | Mod

and unop = Not | Neg | Pl | Typeof
(*XXX*)

and arguments = expression list

and property_name_and_value_list = (property_name * expression) list

and property_name =
    PNI of identifier
  | PNS of string
  | PNN of float

and expression =
    ESeq of expression * expression
  | ECond of expression * expression * expression
  | EBin of binop * expression * expression
  | EUn of unop * expression
  | ECall of expression * arguments
  | EAccess of expression * expression
  | EDot of expression * identifier
  | ENew of expression * arguments option
  | EVar of identifier
  | EFun of function_expression
  | EStr of string * [`Bytes (*| `Utf8*)]
  | EArr of array_litteral
  | EBool of bool
  | ENum of float
  | EObj of property_name_and_value_list
  | EQuote of string

(****)

(* A.4 Statements *)

and statement =
    Block of block
  | Variable_statement of variable_declaration_list
(*
  | Empty_statement
*)
  | Expression_statement of expression
  | If_statement of expression * statement * statement option
  | Do_while_statement of statement * expression
  | While_statement of expression * statement
  | For_statement of
      expression option * expression option * expression option * statement
(*
  | Iteration_statement
*)
  | Continue_statement of string option
  | Break_statement of string option
  | Return_statement of expression option
(*
  | With_statement
*)
  | Labelled_statement of identifier * statement
  | Switch_statement of expression * case_clause list * statement_list option
  | Throw_statement of expression
  | Try_statement of block * (identifier * block) option * block option
(*
  | Debugger_statement
*)

and block = statement_list

and statement_list = statement list

and variable_statement = variable_declaration_list

and variable_declaration_list = variable_declaration list

and variable_declaration = identifier * initialiser option

and case_clause = expression * statement_list

and initialiser = expression

(*... *)

(****)

(* A.5 Functions and programs *)

and function_declaration =
  identifier * formal_parameter_list * function_body

and function_expression =
  identifier option * formal_parameter_list * function_body

and formal_parameter_list = identifier list

and function_body = source_elements

and program = source_elements

and source_elements = source_element list

and source_element =
    Statement of statement
  | Function_declaration of function_declaration

and identifier = string
