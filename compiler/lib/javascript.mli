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

type early_error =
  { loc : Parse_info.t
  ; reason : string option
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
  | Property of property_name * expression
  | PropertySpread of expression
  | PropertyMethod of property_name * method_
  | CoverInitializedName of early_error * ident * initialiser

and method_ =
  | MethodGet of function_declaration
  | MethodSet of function_declaration
  | Method of function_declaration

and property_name =
  | PNI of identifier
  | PNS of Utf8_string.t
  | PNN of Num.t
  | PComputed of expression

and expression =
  | ESeq of expression * expression
  | ECond of expression * expression * expression
  | EAssignTarget of binding_pattern
  (* EAssignTarget is used on the LHS of assignment and in for-loops.
     for({name} in o);
     for([fst] in o);
  *)
  | EBin of binop * expression * expression
  | EUn of unop * expression
  | ECall of expression * access_kind * arguments * location
  | ECallTemplate of expression * template * location
  | EAccess of expression * access_kind * expression
  | EDot of expression * access_kind * identifier
  | ENew of expression * arguments option
  | EVar of ident
  | EFun of ident option * function_declaration
  | EClass of ident option * class_declaration
  | EArrow of function_declaration * arrow_info
  | EStr of Utf8_string.t
  (* A UTF-8 encoded string that may contain escape sequences. *)
  | ETemplate of template
  | EArr of array_litteral
  | EBool of bool
  | ENum of Num.t
  | EObj of property_list
  | ERegexp of string * string option
  | EYield of expression option
  | CoverParenthesizedExpressionAndArrowParameterList of early_error
  | CoverCallExpressionAndAsyncArrowHead of early_error

and arrow_info =
  | AUnknown
  | AUse_parent_fun_context
  | ANo_fun_context

and template = template_part list

and template_part =
  | TStr of Utf8_string.t
  | TExp of expression

and access_kind =
  | ANormal
  | ANullish

(****)

(* A.4 Statements *)
and statement =
  | Block of block
  | Variable_statement of variable_declaration_kind * variable_declaration list
  | Function_declaration of ident * function_declaration
  | Class_declaration of ident * class_declaration
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
  | DeclIdent of binding_ident * initialiser option
  | DeclPattern of binding_pattern * initialiser

and variable_declaration_kind =
  | Var
  | Let
  | Const

and case_clause = expression * statement_list

and initialiser = expression * location

(****)
and function_declaration =
  function_kind * formal_parameter_list * function_body * location

and function_kind =
  { async : bool
  ; generator : bool
  }

and class_declaration =
  { extends : expression option
  ; body : class_element list
  }

and class_element =
  | CEMethod of bool (* static *) * class_element_name * method_
  | CEField of bool (* static *) * class_element_name * initialiser option
  | CEStaticBLock of statement_list

and class_element_name =
  | PropName of property_name
  | PrivName of ident

and ('a, 'b) list_with_rest =
  { list : 'a list
  ; rest : 'b option
  }

and formal_parameter_list = (formal_parameter, binding) list_with_rest

and formal_parameter = binding_element

and for_binding = binding

and binding_element = binding * initialiser option

and binding =
  | BindingIdent of binding_ident
  | BindingPattern of binding_pattern

and binding_pattern =
  | ObjectBinding of (binding_property, binding_ident) list_with_rest
  | ArrayBinding of (binding_element option, binding) list_with_rest

and binding_ident = ident

and binding_property =
  | Prop_binding of property_name * binding_element
  | Prop_ident of binding_ident * initialiser option

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

val bound_idents_of_params : formal_parameter_list -> ident list

val bound_idents_of_variable_declaration : variable_declaration -> ident list

val bound_idents_of_pattern : binding_pattern -> ident list

val bound_idents_of_binding : binding -> ident list

module IdentSet : Set.S with type elt = ident

module IdentMap : Map.S with type key = ident

val dot : expression -> identifier -> expression

val array : expression list -> expression

val call : expression -> expression list -> location -> expression

val variable_declaration : (ident * initialiser) list -> statement

val list : 'a list -> ('a, _) list_with_rest

val early_error : ?reason:string -> Parse_info.t -> early_error

val fun_ : ident list -> statement_list -> location -> function_declaration

val assignment_pattern_of_expr : binop option -> expression -> binding_pattern option
