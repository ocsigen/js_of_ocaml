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
open Javascript
open! Stdlib

module Expand = struct
  let isIntCheck jsExpr = EBin (EqEqEq, EUn (Typeof, jsExpr), EStr ("number", `Bytes))

  let toInt jsExpr = EBin (Bor, jsExpr, ENum (Num.of_int32 0l))

  let intToString jsExpr = EBin (Plus, EStr ("", `Bytes), jsExpr)
end

let rec from_statement_list lst =
  List.map ~f:(fun (stmt, loc) -> from_statement stmt, loc) lst

and from_arguments args = List.map ~f:(fun x -> from_expression x, `Not_spread) args

and from_unop unop jsExpr =
  match unop with
  | Ir.Not -> EUn (Javascript.Not, jsExpr)
  | IsInt -> Expand.isIntCheck jsExpr
  | ToBool | FloatToInt | ToInt -> Expand.toInt jsExpr
  | IntToFloat -> jsExpr
  | IntToString -> Expand.intToString jsExpr
  | FloatNeg | Neg -> EUn (Javascript.Neg, jsExpr)
  | Typeof -> EUn (Javascript.Typeof, jsExpr)
  | Void -> EUn (Javascript.Void, jsExpr)
  | Delete -> EUn (Javascript.Delete, jsExpr)
  | Bnot -> EUn (Javascript.Bnot, jsExpr)

and from_binop = function
  | Ir.Eq -> Javascript.Eq
  | StarEq -> StarEq
  | SlashEq -> SlashEq
  | ModEq -> ModEq
  | PlusEq -> PlusEq
  | MinusEq -> MinusEq
  | Or -> Or
  | And -> And
  | Bor -> Bor
  | Bxor -> Bxor
  | Band -> Band
  | EqEq -> EqEq
  | NotEq -> NotEq
  | FloatEqEq -> EqEq
  | FloatNotEq -> NotEq
  | EqEqEq -> EqEqEq
  | NotEqEq -> NotEqEq
  | Lt -> Lt
  | Le -> Le
  | Gt -> Gt
  | Ge -> Ge
  | FloatLt -> Lt
  | FloatLe -> Le
  | FloatGt -> Gt
  | FloatGe -> Ge
  | InstanceOf -> InstanceOf
  | Lsl -> Lsl
  | Lsr -> Lsr
  | Asr -> Asr
  | Plus -> Plus
  | FloatPlus -> Plus
  | Minus -> Minus
  | FloatMinus -> Minus
  | Mul -> Mul
  | FloatMul -> Mul
  | Div -> Div
  | FloatDiv -> Div
  | Mod -> Mod
  | FloatMod -> Mod

and from_expression_loc (e, loc) = from_expression e, loc

and from_expression e =
  match e with
  | Ir.ERaw s -> Javascript.ERaw s
  | Ir.ESeq (e1, e2) -> Javascript.ESeq (from_expression e1, from_expression e2)
  | ETag (index, itms) ->
      Javascript.EArr
        (Some (Javascript.ENum (Num.of_int32 (Int32.of_int index)))
        :: List.map ~f:(fun itm -> Some (from_expression itm)) itms)
  | EStruct itms ->
      Javascript.EArr (List.map ~f:(fun itm -> Some (from_expression itm)) itms)
  | EAccess (e1, e2) -> Javascript.EAccess (from_expression e1, from_expression e2)
  | EStructAccess (e, i) ->
      Javascript.EAccess
        (from_expression e, Javascript.ENum (Num.of_int32 (Int32.of_int i)))
  | EArrAccess (e1, e2) -> Javascript.EAccess (from_expression e1, from_expression e2)
  | EVectlength e ->
      EBin (Minus, EDot (from_expression e, "length"), ENum (Num.of_int32 1l))
  | EArrLen e -> EDot (from_expression e, "length")
  | EArityTest e -> EDot (from_expression e, "length")
  | ECond (e1, e2, e3) ->
      ECond (from_expression e1, from_expression e2, from_expression e3)
  | EBin (binop, e1, e2) -> EBin (from_binop binop, from_expression e1, from_expression e2)
  | EUn (unop, e) -> from_unop unop (from_expression e)
  | ECall (e, args, loc) -> ECall (from_expression e, from_arguments args, loc)
  | ECopy (e, loc) -> ECall (EDot (from_expression e, "slice"), [], loc)
  | EVar ident -> EVar ident
  | EFun (ident_opt, ident_lst, body, loc) ->
      EFun (ident_opt, ident_lst, from_source_elements_and_locs body, loc)
  | EStr (x, y) -> EStr (x, y)
  | EArr arr_literal ->
      EArr (List.map ~f:(Stdlib.Option.map ~f:from_expression) arr_literal)
  | EDot (e, ident) -> EDot (from_expression e, ident)
  | ENew (e, optargs) ->
      ENew (from_expression e, Stdlib.Option.map ~f:from_arguments optargs)
  | EObj lst -> EObj (List.map ~f:(fun (nm, e) -> PNS nm, from_expression e) lst)
  | EBool b -> EBool b
  | EFloat flt -> ENum (Num.of_float flt)
  | EInt i -> ENum (Num.of_int32 (Int32.of_int i))
  | EQuote s -> EQuote s
  | ERegexp (s, sopt) -> ERegexp (s, sopt)
  | ERuntime -> EDot (EVar (Id.ident Constant.global_object), "jsoo_runtime")

and from_statement e =
  match e with
  | Ir.Block stms -> Block (from_statement_list stms)
  | Ir.Variable_statement lst ->
      Javascript.Variable_statement
        (List.map
           ~f:(fun (ident, initopt) ->
             ident, Stdlib.Option.map ~f:from_expression_loc initopt)
           lst)
  | Ir.Empty_statement -> Empty_statement
  | Ir.Expression_statement expr -> Expression_statement (from_expression expr)
  | Ir.If_statement (expr, (ifstmt, ifloc), elsopt) ->
      If_statement
        ( from_expression expr
        , (from_statement ifstmt, ifloc)
        , match elsopt with
          | None -> None
          | Some (elstmt, elloc) -> Some (from_statement elstmt, elloc) )
  | Ir.Loop_statement (stmt, loc) ->
      For_statement (Left None, None, None, (from_statement stmt, loc))
  | Ir.Continue_statement lbl -> Continue_statement lbl
  | Ir.Break_statement lbl -> Break_statement lbl
  | Ir.Return_statement eo -> Return_statement (Stdlib.Option.map ~f:from_expression eo)
  | Ir.Labelled_statement (lbl, (stmt, loc)) ->
      Labelled_statement (lbl, (from_statement stmt, loc))
  | Ir.Switch_statement (e, case_clause_list, stmt_lst) ->
      let e = from_expression e in
      let case_clause_lst = from_case_clause_list case_clause_list in
      let stmt_lst =
        match stmt_lst with
        | [] -> None
        | _ -> Some (from_statement_list stmt_lst)
      in
      Switch_statement (e, case_clause_lst, stmt_lst, [])
  | Ir.Throw_statement e -> Throw_statement (from_expression e)
  | Ir.Try_statement (b1, (ident, b2)) ->
      let b1 = from_statement_list b1 in
      let ident_block = ident, from_statement_list b2 in
      Try_statement (b1, Some ident_block, None)
  | Ir.Debugger_statement -> Debugger_statement

and from_case_clause_list lst =
  List.map ~f:(fun (e, stmts) -> from_expression e, from_statement_list stmts) lst

and from_source_element = function
  | Ir.Statement stmt -> Statement (from_statement stmt)
  | Ir.Function_declaration (ident, formal_parameter_list, function_body, location) ->
      Javascript.Function_declaration
        ( ident
        , formal_parameter_list
        , from_source_elements_and_locs function_body
        , location )

and from_source_elements_and_locs lst =
  List.map ~f:(fun (src, loc) -> from_source_element src, loc) lst

let run = from_source_elements_and_locs
