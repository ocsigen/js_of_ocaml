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
open! Stdlib

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
end = struct
  type t = string

  let of_string_unsafe s = s

  let to_string s = s

  let to_int32 s =
    if String.is_prefix s ~prefix:"0"
       && String.length s > 1
       && String.for_all s ~f:(function
              | '0' .. '7' -> true
              | _ -> false)
    then (* legacy octal notation *)
      Int32.of_string ("0o" ^ s)
    else Int32.of_string s

  let of_int32 = Int32.to_string

  let of_float v =
    match Float.classify_float v with
    | FP_nan -> "NaN"
    | FP_zero ->
        (* [1/-0] < 0. seems to be the only way to detect -0 in JavaScript *)
        if Float.(1. /. v < 0.) then "-0." else "0."
    | FP_infinite -> if Float.(v < 0.) then "-Infinity" else "Infinity"
    | FP_normal | FP_subnormal ->
        let vint = int_of_float v in
        if Float.equal (float_of_int vint) v
        then Printf.sprintf "%d." vint
        else
          let s1 = Printf.sprintf "%.12g" v in
          if Float.equal v (float_of_string s1)
          then s1
          else
            let s2 = Printf.sprintf "%.15g" v in
            if Float.equal v (float_of_string s2) then s2 else Printf.sprintf "%.18g" v

  let is_zero s = String.equal s "0"

  let is_one s = String.equal s "1"

  let is_neg s = Char.equal s.[0] '-'

  let neg s =
    match String.drop_prefix s ~prefix:"-" with
    | None -> "-" ^ s
    | Some s -> s

  let add a b = of_int32 (Int32.add (to_int32 a) (to_int32 b))
end

module Label = struct
  type t =
    | L of int
    | S of Utf8_string.t

  let printer = Var_printer.create Var_printer.Alphabet.javascript

  let zero = L 0

  let succ = function
    | L t -> L (succ t)
    | S _ -> assert false

  let to_string = function
    | L t -> Utf8_string.of_string_exn (Var_printer.to_string printer t)
    | S s -> s

  let of_string s = S s
end

type location =
  | Pi of Parse_info.t
  | N
  | U

type identifier = Utf8_string.t

type ident_string =
  { name : identifier
  ; var : Code.Var.t option
  ; loc : location
  }

type ident =
  | S of ident_string
  | V of Code.Var.t

(* A.3 Expressions *)
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
  (* | With_statement of expression * statement *)
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

and function_kind =
  { async : bool
  ; generator : bool
  }

and formal_parameter_list = formal_parameter list

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

let compare_ident t1 t2 =
  match t1, t2 with
  | V v1, V v2 -> Code.Var.compare v1 v2
  | S { name = Utf8 s1; var = v1; loc = _ }, S { name = Utf8 s2; var = v2; loc = _ } -> (
      (* ignore locations *)
      match String.compare s1 s2 with
      | 0 -> Option.compare Code.Var.compare v1 v2
      | n -> n)
  | S _, V _ -> -1
  | V _, S _ -> 1

let is_ident = Flow_lexer.is_valid_identifier_name

let is_ident' (Utf8_string.Utf8 s) = is_ident s

let ident ?(loc = N) ?var (Utf8_string.Utf8 n as name) =
  if not (is_ident' name) then failwith (Printf.sprintf "%s not a valid ident" n);
  S { name; var; loc }

let param' id = PIdent { id; default = None }

let param ?loc ?var name = param' (ident ?loc ?var name)

let ident_unsafe ?(loc = N) ?var name = S { name; var; loc }

let rec bound_idents_of_param p =
  match p with
  | PIdent { id; _ } -> [ id ]
  | PIdentSpread id -> [ id ]
  | PPattern (p, _) -> bound_idents_of_pattern p

and bound_idents_of_params p = List.concat_map p ~f:bound_idents_of_param

and bound_idents_of_pattern p =
  match p with
  | Id id -> [ id ]
  | Object_binding l ->
      List.concat_map l ~f:(function
          | Prop_rest i -> [ i ]
          | Prop_binding (_, p, _) -> bound_idents_of_pattern p)
  | Array_binding l ->
      List.concat_map l ~f:(function
          | Elt_binding (p, _) -> bound_idents_of_pattern p
          | Elt_hole -> []
          | Elt_rest i -> [ i ])

and bound_idents_of_variable_declaration = function
  | DIdent (id, _) -> [ id ]
  | DPattern (p, _) -> bound_idents_of_pattern p

module IdentSet = Set.Make (struct
  type t = ident

  let compare = compare_ident
end)

module IdentMap = Map.Make (struct
  type t = ident

  let compare = compare_ident
end)

let dot e l = EDot (e, ANormal, l)

let variable_declaration l =
  Variable_statement (Var, List.map l ~f:(fun (i, e) -> DIdent (i, Some e)))

let array l = EArr (List.map l ~f:(fun x -> Element x))

let call f args loc = ECall (f, ANormal, List.map args ~f:(fun x -> Arg x), loc)
