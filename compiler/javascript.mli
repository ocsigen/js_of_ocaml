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

module Label : sig
  type t
  val zero : t
  val succ : t -> t
  val to_string : t -> string
  val of_string : string -> t
end

type loc =
  | Pi of Parse_info.t
  | N

(* A.3 Expressions *)

type identifier = string

type ident_string = {
  name : identifier;
  var : Code.Var.t option }

type ident =
  | S of ident_string
  | V of Code.Var.t

and array_litteral = element_list

and element_list = expression option list

and binop =
    Eq | StarEq | SlashEq | ModEq | PlusEq | MinusEq
  | LslEq | AsrEq | LsrEq | BandEq | BxorEq | BorEq
  | Or | And | Bor | Bxor | Band
  | EqEq | NotEq | EqEqEq | NotEqEq
  | Lt | Le | Gt | Ge | InstanceOf | In
  | Lsl | Lsr | Asr
  | Plus | Minus
  | Mul | Div | Mod

and unop = Not | Neg | Pl | Typeof | Void | Delete | Bnot | IncrA | DecrA | IncrB | DecrB

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
  | ECall of expression * arguments * loc
  | EAccess of expression * expression
  | EDot of expression * identifier
  | ENew of expression * arguments option
  | EVar of ident
  | EFun of function_expression
  | EStr of string * [`Bytes | `Utf8]
      (* A string can either be composed of a sequence of bytes, or be
         UTF-8 encoded. In the second case, the string may contain
         escape sequences. *)
  | EArr of array_litteral
  | EBool of bool
  | ENum of float
  | EObj of property_name_and_value_list
  | EQuote of string
  | ERegexp of string * string option

(****)

(* A.4 Statements *)

and statement =
    Block of block * loc
  | Variable_statement of variable_declaration list
  | Empty_statement of loc
  | Expression_statement of expression * loc
  | If_statement of expression * statement * statement option * loc
  | Do_while_statement of statement * expression * loc
  | While_statement of expression * statement * loc
  | For_statement of (expression option,variable_declaration list) either * expression option * expression option * statement * loc
  | ForIn_statement of  (expression,variable_declaration) either * expression * statement * loc
  | Continue_statement of Label.t option * loc
  | Break_statement of Label.t option * loc
  | Return_statement of expression option * loc
(*
  | With_statement
*)
  | Labelled_statement of Label.t * statement * loc
  | Switch_statement of expression * case_clause list * statement_list option * loc
  | Throw_statement of expression * loc
  | Try_statement of block * (ident * block) option * block option * loc

  | Debugger_statement of loc

and ('left,'right) either =
  | Left of 'left
  | Right of 'right


and block = statement_list

and statement_list = statement list

and variable_declaration = ident * initialiser option

and case_clause = expression * statement_list

and initialiser = expression * loc

(****)

(* A.5 Functions and programs *)

and function_declaration =
  ident * formal_parameter_list * function_body * loc

and function_expression =
  ident option * formal_parameter_list * function_body * loc

and formal_parameter_list = ident list

and function_body = source_elements

and program = source_elements

and source_elements = source_element list

and source_element =
    Statement of statement
  | Function_declaration of function_declaration

val compare_ident : ident -> ident -> int
val string_of_number : float -> string
module IdentSet : Set.S with type elt = ident
module IdentMap : Map.S with type key = ident
