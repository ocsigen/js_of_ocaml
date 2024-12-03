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

  val of_targetint : Targetint.t -> t

  val of_float : float -> t

  val to_string : t -> string

  val to_targetint : t -> Targetint.t

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

  let to_targetint s =
    if
      String.is_prefix s ~prefix:"0"
      && String.length s > 1
      && String.for_all s ~f:(function
           | '0' .. '7' -> true
           | _ -> false)
    then (* legacy octal notation *)
      Targetint.of_string_exn ("0o" ^ s)
    else Targetint.of_string_exn s

  let of_targetint = Targetint.to_string

  external format_float : string -> float -> string = "caml_format_float"

  let fmts = Array.init 19 ~f:(fun i -> "%." ^ string_of_int i ^ "g")

  let float_to_string prec v = format_float fmts.(prec) v

  let rec find_smaller ~f ~bad ~good ~good_s =
    if bad + 1 = good
    then good_s
    else
      let mid = (good + bad) / 2 in
      assert (mid <> good);
      assert (mid <> bad);
      match f mid with
      | None -> find_smaller ~f ~bad:mid ~good ~good_s
      | Some s -> find_smaller ~f ~bad ~good:mid ~good_s:s

  (* Windows uses 3 digits for the exponent, let's fix it. *)
  let fix_exponent s =
    try
      let start = String.index_from s 0 'e' + 1 in
      let start =
        match String.get s start with
        | '-' | '+' -> succ start
        | _ -> start
      in
      let stop = ref start in
      while Char.equal (String.get s !stop) '0' do
        incr stop
      done;
      if start = !stop
      then s
      else
        String.sub s ~pos:0 ~len:start
        ^ String.sub s ~pos:!stop ~len:(String.length s - !stop)
    with Not_found -> s

  let of_float v =
    match Float.classify_float v with
    | FP_nan -> "NaN"
    | FP_zero ->
        (* [1/-0] < 0. seems to be the only way to detect -0 in JavaScript *)
        if Float.(1. /. v < 0.) then "-0." else "0."
    | FP_infinite -> if Float.(v < 0.) then "-Infinity" else "Infinity"
    | FP_normal | FP_subnormal -> (
        let vint = int_of_float v in
        if Float.ieee_equal (float_of_int vint) v
        then Printf.sprintf "%d." vint
        else
          match
            find_smaller
              ~f:(fun prec ->
                let s = float_to_string prec v in
                if Float.ieee_equal v (float_of_string s) then Some s else None)
              ~bad:0
              ~good:18
              ~good_s:"max"
          with
          | "max" -> float_to_string 18 v |> fix_exponent
          | s -> fix_exponent s)

  let is_zero s = String.equal s "0"

  let is_one s = String.equal s "1"

  let is_neg s = Char.equal s.[0] '-'

  let neg s =
    match String.drop_prefix s ~prefix:"-" with
    | None -> "-" ^ s
    | Some s -> s

  let add a b = of_targetint (Targetint.add (to_targetint a) (to_targetint b))
end

module Label = struct
  type t =
    | L of Code.Var.t
    | S of Utf8_string.t

  let fresh () = L (Code.Var.fresh ())

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

type early_error =
  { loc : Parse_info.t
  ; reason : string option
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
  | EAssignTarget of assignment_target
  | EBin of binop * expression * expression
  | EUn of unop * expression
  | ECall of expression * access_kind * arguments * location
  | ECallTemplate of expression * template * location
  | EAccess of expression * access_kind * expression
  | EDot of expression * access_kind * identifier
  | EDotPrivate of expression * access_kind * identifier
  | ENew of expression * arguments option * location
  | EVar of ident
  | EFun of ident option * function_declaration
  | EClass of ident option * class_declaration
  | EArrow of function_declaration * bool * arrow_info
  | EStr of Utf8_string.t
  | ETemplate of template
  | EArr of array_litteral
  | EBool of bool
  | ENum of Num.t
  | EObj of property_list
  | ERegexp of string * string option
  | EYield of
      { delegate : bool
      ; expr : expression option
      }
  | EPrivName of identifier
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
  | ForAwaitOf_statement of
      (expression, variable_declaration_kind * for_binding) either
      * expression
      * (statement * location)
  | Continue_statement of Label.t option
  | Break_statement of Label.t option
  | Return_statement of expression option * location
  | With_statement of expression * (statement * location)
  | Labelled_statement of Label.t * (statement * location)
  | Switch_statement of
      expression * case_clause list * statement_list option * case_clause list
  | Throw_statement of expression
  | Try_statement of block * (formal_parameter option * block) option * block option
  | Debugger_statement
  | Import of import * Parse_info.t
  | Export of export * Parse_info.t

and ('left, 'right) either =
  | Left of 'left
  | Right of 'right

and block = statement_list

and statement_list = (statement * location) list

and variable_declaration =
  | DeclIdent of ident * initialiser option
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
  | CEMethod of bool * class_element_name * method_
  | CEField of bool * class_element_name * initialiser option
  | CEStaticBLock of statement_list

and class_element_name =
  | PropName of property_name
  | PrivName of identifier

and ('a, 'b) list_with_rest =
  { list : 'a list
  ; rest : 'b option
  }

and formal_parameter_list = (formal_parameter, binding) list_with_rest

and formal_parameter = binding_element

and for_binding = binding

and binding_element = binding * initialiser option

and binding =
  | BindingIdent of ident
  | BindingPattern of binding_pattern

and binding_pattern =
  | ObjectBinding of (binding_property, ident) list_with_rest
  | ArrayBinding of (binding_element option, binding) list_with_rest

and object_target_elt =
  | TargetPropertyId of ident_prop * initialiser option
  | TargetProperty of property_name * expression * initialiser option
  | TargetPropertySpread of expression
  | TargetPropertyMethod of property_name * method_

and array_target_elt =
  | TargetElementId of ident * initialiser option
  | TargetElementHole
  | TargetElement of expression
  | TargetElementSpread of expression

and assignment_target =
  | ObjectTarget of object_target_elt list
  | ArrayTarget of array_target_elt list

and ident_prop = Prop_and_ident of ident

and binding_property =
  | Prop_binding of property_name * binding_element
  | Prop_ident of ident_prop * initialiser option

and function_body = statement_list

and program = statement_list

and export =
  | ExportVar of variable_declaration_kind * variable_declaration list
  | ExportFun of ident * function_declaration
  | ExportClass of ident * class_declaration
  | ExportNames of (ident * Utf8_string.t) list
  (* default *)
  | ExportDefaultFun of ident option * function_declaration
  | ExportDefaultClass of ident option * class_declaration
  | ExportDefaultExpression of expression
  (* from *)
  | ExportFrom of
      { kind : export_from_kind
      ; from : Utf8_string.t
      }
  | CoverExportFrom of early_error

and export_from_kind =
  | Export_all of Utf8_string.t option
  | Export_names of (Utf8_string.t * Utf8_string.t) list

and import =
  { from : Utf8_string.t
  ; kind : import_kind
  }

and import_default = ident

and import_kind =
  | Namespace of import_default option * ident
  (* import * as name from "fname" *)
  (* import defaultname, * as name from "fname" *)
  | Named of import_default option * (Utf8_string.t * ident) list
  (* import { 'a' as a, ...} from "fname" *)
  (* import defaultname, { 'a' as a, ...} from "fname" *)
  | Default of import_default
  (* import defaultname from "fname" *)
  | SideEffect (* import "fname" *)

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

let param' id = BindingIdent id, None

let param ?loc ?var name = param' (ident ?loc ?var name)

let ident_unsafe ?(loc = N) ?var name = S { name; var; loc }

let rec bound_idents_of_binding p =
  match p with
  | BindingIdent id -> [ id ]
  | BindingPattern p -> bound_idents_of_pattern p

and bound_idents_of_params { list; rest } =
  List.concat_map list ~f:bound_idents_of_element
  @
  match rest with
  | None -> []
  | Some p -> bound_idents_of_binding p

and bound_idents_of_pattern p =
  match p with
  | ObjectBinding { list; rest } -> (
      List.concat_map list ~f:(function
        | Prop_ident (Prop_and_ident i, _) -> [ i ]
        | Prop_binding (_, e) -> bound_idents_of_element e)
      @
      match rest with
      | None -> []
      | Some x -> [ x ])
  | ArrayBinding { list; rest } -> (
      List.concat_map list ~f:(function
        | None -> []
        | Some e -> bound_idents_of_element e)
      @
      match rest with
      | None -> []
      | Some x -> bound_idents_of_binding x)

and bound_idents_of_variable_declaration = function
  | DeclIdent (id, _) -> [ id ]
  | DeclPattern (p, _) -> bound_idents_of_pattern p

and bound_idents_of_element (b, _) = bound_idents_of_binding b

module IdentSet = Set.Make (struct
  type t = ident

  let compare = compare_ident
end)

module IdentMap = Map.Make (struct
  type t = ident

  let compare = compare_ident
end)

let dot e l = EDot (e, ANormal, l)

let variable_declaration ?(kind = Var) l =
  Variable_statement (kind, List.map l ~f:(fun (i, e) -> DeclIdent (i, Some e)))

let array l = EArr (List.map l ~f:(fun x -> Element x))

let call f args loc = ECall (f, ANormal, List.map args ~f:(fun x -> Arg x), loc)

let list list = { list; rest = None }

let early_error ?reason loc = { loc; reason }

let fun_ params body loc =
  ( { async = false; generator = false }
  , list (List.map params ~f:(fun x -> BindingIdent x, None))
  , body
  , loc )

let rec assignment_target_of_expr' x =
  match x with
  | EObj l ->
      let list =
        List.map l ~f:(function
          | Property (PNI n, EVar (S { name = n'; _ } as id)) when Utf8_string.equal n n'
            -> TargetPropertyId (Prop_and_ident id, None)
          | Property (n, e) ->
              let e, i =
                match e with
                | EBin (Eq, e, i) -> e, Some (i, N)
                | _ -> e, None
              in
              TargetProperty (n, assignment_target_of_expr' e, i)
          | CoverInitializedName (_, i, (e, loc)) ->
              TargetPropertyId (Prop_and_ident i, Some (assignment_target_of_expr' e, loc))
          | PropertySpread e -> TargetPropertySpread (assignment_target_of_expr' e)
          | PropertyMethod (n, m) -> TargetPropertyMethod (n, m))
      in
      EAssignTarget (ObjectTarget list)
  | EArr l ->
      let list =
        List.map l ~f:(function
          | ElementHole -> TargetElementHole
          | Element (EVar x) -> TargetElementId (x, None)
          | Element (EBin (Eq, EVar x, rhs)) -> TargetElementId (x, Some (rhs, N))
          | Element e -> TargetElement (assignment_target_of_expr' e)
          | ElementSpread e -> TargetElementSpread (assignment_target_of_expr' e))
      in
      EAssignTarget (ArrayTarget list)
  | _ -> x

and assignment_target_of_expr op x =
  match op with
  | None | Some Eq -> assignment_target_of_expr' x
  | _ -> x
