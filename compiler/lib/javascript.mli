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

open Stdlib

module Num : sig
  type t

  (** Conversions *)

  val of_string_unsafe : string -> t

  val of_int32 : int32 -> t

  val of_float : float -> t

  val to_string : t -> string

  val to_int32 : t -> int32

  (** Predicates *)

  val is_zero : t -> bool

  val is_one : t -> bool

  val is_neg : t -> bool

  (** Arithmetic *)

  val add : t -> t -> t

  val neg : t -> t
end

module Label : sig
  type t

  val zero : t

  val succ : t -> t

  val to_string : t -> Utf8_string.t

  val of_string : Utf8_string.t -> t
end

type location =
  | Pi of Parse_info.t
  | N
  (* No location; use the one above *)
  | U

(* Unknown location *)

(* A.3 Expressions *)

type identifier = Utf8_string.t

type ident_string =
  { name : identifier
  ; var : Code.Var.t option
  ; loc : location
  }

type ident =
  | S of ident_string
  | V of Code.Var.t

and array_litteral = element_list

and element_list = element list

and element =
  | ElementHole
  | Element of expression
  | ElementSpread of expression

and binop =
  | Eq
  | StarEq
  | SlashEq
  | ModEq
  | PlusEq
  | MinusEq
  | LslEq
  | AsrEq
  | LsrEq
  | BandEq
  | BxorEq
  | BorEq
  | Or
  | OrEq
  | And
  | AndEq
  | Bor
  | Bxor
  | Band
  | EqEq
  | NotEq
  | EqEqEq
  | NotEqEq
  | Lt
  | Le
  | Gt
  | Ge
  | LtInt
  | LeInt
  | GtInt
  | GeInt
  | InstanceOf
  | In
  | Lsl
  | Lsr
  | Asr
  | Plus
  | Minus
  | Mul
  | Div
  | Mod
  | Exp
  | ExpEq
  | Coalesce
  | CoalesceEq

and unop =
  | Not
  | Neg
  | Pl
  | Typeof
  | Void
  | Delete
  | Bnot
  | IncrA
  | DecrA
  | IncrB
  | DecrB
  | Await

and arguments = argument list

and argument =
  | Arg of expression
  | ArgSpread of expression

and property_list = property list

and property =
  | PropertySpread of expression
  | PropertyGet of identifier * formal_parameter_list * function_body
  | PropertySet of identifier * formal_parameter_list * function_body
  | PropertyMethod of identifier * function_expression
  | Property of property_name * expression
  | PropertyComputed of expression * expression

and property_name =
  | PNI of identifier
  | PNS of Utf8_string.t
  | PNN of Num.t

and expression =
  | ESeq of expression * expression
  | ECond of expression * expression * expression
  | EBin of binop * expression * expression
  | EUn of unop * expression
  | ECall of expression * access_kind * arguments * location
  | EAccess of expression * access_kind * expression
  | EDot of expression * access_kind * identifier
  | ENew of expression * arguments option
  | EVar of ident
  | EFun of function_expression
  | EArrow of arrow_expression
  | EStr of Utf8_string.t
  (* A UTF-8 encoded string that may contain escape sequences. *)
  | EArr of array_litteral
  | EBool of bool
  | ENum of Num.t
  | EObj of property_list
  | ERegexp of string * string option
  | EYield of expression option

and access_kind =
  | ANormal
  | ANullish

(****)

(* A.4 Statements *)
and statement =
  | Block of block
  | Variable_statement of variable_declaration_kind * variable_declaration list
  | Function_declaration of function_declaration
  | Empty_statement
  | Expression_statement of expression
  | If_statement of expression * (statement * location) * (statement * location) option
  | Do_while_statement of (statement * location) * expression
  | While_statement of expression * (statement * location)
  | For_statement of
      (expression option, variable_declaration_kind * variable_declaration list) either
      * expression option
      * expression option
      * (statement * location)
  | ForIn_statement of
      (expression, variable_declaration_kind * for_binding) either
      * expression
      * (statement * location)
  | ForOf_statement of
      (expression, variable_declaration_kind * for_binding) either
      * expression
      * (statement * location)
  | Continue_statement of Label.t option
  | Break_statement of Label.t option
  | Return_statement of expression option
  (*
  | With_statement
*)
  | Labelled_statement of Label.t * (statement * location)
  | Switch_statement of
      expression * case_clause list * statement_list option * case_clause list
  | Throw_statement of expression
  | Try_statement of block * (formal_parameter option * block) option * block option
  | Debugger_statement

and ('left, 'right) either =
  | Left of 'left
  | Right of 'right

and block = statement_list

and statement_list = (statement * location) list

and variable_declaration =
  | DIdent of ident * initialiser option
  | DPattern of binding_pattern * initialiser

and variable_declaration_kind =
  | Var
  | Let
  | Const

and case_clause = expression * statement_list

and initialiser = expression * location

(****)

(* A.5 Functions and programs *)
and function_declaration =
  ident * function_kind * formal_parameter_list * function_body * location

and function_expression =
  ident option * function_kind * formal_parameter_list * function_body * location

and arrow_expression = function_kind * formal_parameter_list * function_body * location

and formal_parameter_list = formal_parameter list

and function_kind =
  { async : bool
  ; generator : bool
  }

and formal_parameter =
  | PPattern of binding_pattern * (expression * location) option
  | PIdent of
      { id : ident
      ; default : (expression * location) option
      }
  | PIdentSpread of ident

and for_binding =
  | ForBindIdent of ident
  | ForBindPattern of binding_pattern

and binding_pattern =
  | Object_binding of binding_property list
  | Array_binding of binding_array_elt list
  | Id of ident

and binding_property =
  | Prop_binding of property_name * binding_pattern * (expression * location) option
  | Prop_rest of ident

and binding_array_elt =
  | Elt_binding of binding_pattern * (expression * location) option
  | Elt_hole
  | Elt_rest of ident

and function_body = statement_list

and program = statement_list

and program_with_annots = (statement_list * (Js_token.Annot.t * Parse_info.t) list) list

val compare_ident : ident -> ident -> int

val is_ident : string -> bool

val is_ident' : Utf8_string.t -> bool

val ident : ?loc:location -> ?var:Code.Var.t -> identifier -> ident

val param : ?loc:location -> ?var:Code.Var.t -> identifier -> formal_parameter

val param' : ident -> formal_parameter

val ident_unsafe : ?loc:location -> ?var:Code.Var.t -> identifier -> ident

val bound_idents_of_params : formal_parameter list -> ident list

val bound_idents_of_param : formal_parameter -> ident list

val bound_idents_of_variable_declaration : variable_declaration -> ident list

val bound_idents_of_pattern : binding_pattern -> ident list

module IdentSet : Set.S with type elt = ident

module IdentMap : Map.S with type key = ident

val dot : expression -> identifier -> expression

val array : expression list -> expression

val call : expression -> expression list -> location -> expression

val variable_declaration : (ident * initialiser) list -> statement
