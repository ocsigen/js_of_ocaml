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


module Label = struct
  type t =
    | L of int
    | S of string

  let printer = VarPrinter.create ()

  let zero = L 0
  let succ = function
    | L t -> L (succ t)
    | S _ -> assert false
  let to_string = function
    | L t -> VarPrinter.to_string printer t
    | S s -> s
  let of_string s = S s
end

type location =
  | Pi of Parse_info.t
  | N
  | U

type identifier = string

type ident_string = {
  name : identifier;
  var : Code.Var.t option }

type ident =
  | S of ident_string
  | V of Code.Var.t

(* A.3 Expressions *)

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
  | ECall of expression * arguments * location
  | EAccess of expression * expression
  | EDot of expression * identifier
  | ENew of expression * arguments option
  | EVar of ident
  | EFun of function_expression
  | EStr of string * [`Bytes | `Utf8]
  | EArr of array_litteral
  | EBool of bool
  | ENum of float
  | EObj of property_name_and_value_list
  | EQuote of string
  | ERegexp of string * string option

(****)

(* A.4 Statements *)

and statement =
    Block of block
  | Variable_statement of variable_declaration list
  | Empty_statement
  | Expression_statement of expression
  | If_statement of expression * (statement * location) * (statement * location) option
  | Do_while_statement of (statement * location) * expression
  | While_statement of expression * (statement * location)
  | For_statement of  (expression option,variable_declaration list) either * expression option * expression option * (statement * location)
  | ForIn_statement of  (expression,variable_declaration) either * expression * (statement * location)
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

and ('left,'right) either =
  | Left of 'left
  | Right of 'right

and block = statement_list

and statement_list = (statement * location) list

and variable_declaration = ident * initialiser option

and case_clause = expression * statement_list

and initialiser = expression * location

(****)

(* A.5 Functions and programs *)

and function_declaration =
  ident * formal_parameter_list * function_body * location

and function_expression =
  ident option * formal_parameter_list * function_body * location

and formal_parameter_list = ident list

and function_body = source_elements

and program = source_elements

and source_elements = (source_element * location) list

and source_element =
    Statement of statement
  | Function_declaration of function_declaration

let compare_ident t1 t2 =
  match t1, t2 with
    | V v1, V v2 -> Code.Var.compare v1 v2
    | S {name=s1;var=v1}, S{name=s2;var=v2} -> begin
      match String.compare s1 s2 with
        | 0 -> begin match v1,v2 with
            | None,None -> 0
            | None, _ -> -1
            | _, None -> 1
            | Some v1, Some v2 -> Code.Var.compare v1 v2
        end
        | n -> n
    end
    | S _, V _ -> -1
    | V _, S _ -> 1


let string_of_number v =
  if v = infinity
  then "Infinity"
  else if v = neg_infinity
  then "-Infinity"
  else if v <> v
  then "NaN"
  (* [1/-0] = -inf seems to be the only way to detect -0 in JavaScript *)
  else if v = 0. && (1. /. v) = neg_infinity
  then "-0"
  else
    let vint = int_of_float v in
    (* compiler 1000 into 1e3 *)
    if float_of_int vint = v
    then
      let rec div n i =
        if n <> 0 && n mod 10 = 0
        then div (n/10) (succ i)
        else
        if i > 2
        then Printf.sprintf "%de%d" n i
        else string_of_int vint in
      div vint 0
    else
      let s1 = Printf.sprintf "%.12g" v in
      if v = float_of_string s1
      then s1
      else
        let s2 = Printf.sprintf "%.15g" v in
        if v = float_of_string s2
        then s2
        else  Printf.sprintf "%.18g" v

exception Not_an_ident
let is_ident =
  let l = Array.init 256 (fun i ->
    let c = Char.chr i in
    if (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_' || c = '$'
    then 1
    else if (c >= '0' && c <='9')
    then 2
    else 0
  ) in
  fun s ->
    not (Util.StringSet.mem s Reserved.keyword) &&
    try
      for i = 0 to String.length s - 1 do
        let code = l.(Char.code(s.[i])) in
        if i = 0
        then (if code <> 1 then raise Not_an_ident)
        else (if code <  1 then raise Not_an_ident)
      done;
      true
    with Not_an_ident -> false

module IdentSet = Set.Make(struct type t = ident let compare = compare_ident end)
module IdentMap = Map.Make(struct type t = ident let compare = compare_ident end)
