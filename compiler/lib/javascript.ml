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
    then (* octal notation *)
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
    | S of string

  let printer = Var_printer.create Var_printer.Alphabet.javascript

  let zero = L 0

  let succ = function
    | L t -> L (succ t)
    | S _ -> assert false

  let to_string = function
    | L t -> Var_printer.to_string printer t
    | S s -> s

  let of_string s = S s
end

type location =
  | Pi of Parse_info.t
  | N
  | U

type identifier = string

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

and element_list = expression option list

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
  | And
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

and spread =
  [ `Spread
  | `Not_spread
  ]

and arguments = (expression * spread) list

and property_name_and_value_list = (property_name * expression) list

and property_name =
  | PNI of identifier
  | PNS of string
  | PNN of Num.t

and expression =
  | ESeq of expression * expression
  | ECond of expression * expression * expression
  | EBin of binop * expression * expression
  | EUn of unop * expression
  | ECall of expression * arguments * location
  | EAccess of expression * expression
  | EDot of expression * identifier
  | ENew of expression * arguments option
  | EVar of ident
  | EFun of function_expression
  | EStr of string * [ `Bytes | `Utf8 ]
  | EArr of array_litteral
  | EBool of bool
  | ENum of Num.t
  | EObj of property_name_and_value_list
  | EQuote of string
  | ERegexp of string * string option

(****)

(* A.4 Statements *)
and statement =
  | Block of block
  | Variable_statement of variable_declaration list
  | Empty_statement
  | Expression_statement of expression
  | If_statement of expression * (statement * location) * (statement * location) option
  | Do_while_statement of (statement * location) * expression
  | While_statement of expression * (statement * location)
  | For_statement of
      (expression option, variable_declaration list) either
      * expression option
      * expression option
      * (statement * location)
  | ForIn_statement of
      (expression, variable_declaration) either * expression * (statement * location)
  | Continue_statement of Label.t option
  | Break_statement of Label.t option
  | Return_statement of expression option
  (* | With_statement of expression * statement *)
  | Labelled_statement of Label.t * (statement * location)
  | Switch_statement of
      expression * case_clause list * statement_list option * case_clause list
  | Throw_statement of expression
  | Try_statement of block * (ident * block) option * block option
  | Debugger_statement

and ('left, 'right) either =
  | Left of 'left
  | Right of 'right

and block = statement_list

and statement_list = (statement * location) list

and variable_declaration = ident * initialiser option

and case_clause = expression * statement_list

and initialiser = expression * location

(****)

(* A.5 Functions and programs *)
and function_declaration = ident * formal_parameter_list * function_body * location

and function_expression = ident option * formal_parameter_list * function_body * location

and formal_parameter_list = ident list

and function_body = source_elements

and program = source_elements

and program_with_annots = ((source_element * location) * Js_token.Annot.t list) list

and source_elements = (source_element * location) list

and source_element =
  | Statement of statement
  | Function_declaration of function_declaration

let compare_ident t1 t2 =
  match t1, t2 with
  | V v1, V v2 -> Code.Var.compare v1 v2
  | S { name = s1; var = v1; loc = _ }, S { name = s2; var = v2; loc = _ } -> (
      (* ignore locations *)
      match String.compare s1 s2 with
      | 0 -> Option.compare Code.Var.compare v1 v2
      | n -> n)
  | S _, V _ -> -1
  | V _, S _ -> 1

exception Not_an_ident

let is_ident =
  let l =
    Array.init 256 ~f:(fun i ->
        let c = Char.chr i in
        match c with
        | 'a' .. 'z' | 'A' .. 'Z' | '_' | '$' -> 1
        | '0' .. '9' -> 2
        | _ -> 0)
  in
  fun s ->
    (not (StringSet.mem s Reserved.keyword))
    &&
    try
      for i = 0 to String.length s - 1 do
        let code = l.(Char.code s.[i]) in
        if i = 0
        then (if code <> 1 then raise Not_an_ident)
        else if code < 1
        then raise Not_an_ident
      done;
      true
    with Not_an_ident -> false

let ident ?(loc = N) ?var name = S { name; var; loc }

module IdentSet = Set.Make (struct
  type t = ident

  let compare = compare_ident
end)

module IdentMap = Map.Make (struct
  type t = ident

  let compare = compare_ident
end)
