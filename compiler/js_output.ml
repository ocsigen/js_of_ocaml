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
XXX Beware automatic semi-colon insertion...
         a=b
         ++c
      is not the same as
         a=b ++c
===> see so-called "restricted productions":
     the space cannot be replaced by a newline in the following expressions:
       e ++, e --, continue e, break e, return e, throw e
*)

open Javascript

module PP = Pretty_print

let opt_identifier f i =
  match i with
    None   -> ()
  | Some i -> PP.space f; PP.string f i

let rec formal_parameter_list f l =
  match l with
    []     -> ()
  | [i]    -> PP.string f i
  | i :: r -> PP.string f i; PP.string f ","; PP.break f;
              formal_parameter_list f r

(*
 0 Expression
 1 AssignementExpression
 2 ConditionalExpression
 3 LogicalORExpression
 4 LogicalANDExpression
 5 BitwiseORExpression
 6 BitwiseXORExpression
 7 BitwiseANDExpression
 8 EqualityExpression
 9 RelationalExpression
10 ShiftExpression
11 AdditiveExpression
12 MultiplicativeExpression
13 UnaryExpression
14 PostfixExpression
15 LeftHandsideExpression
   NewExpression
   CallExpression
16 MemberExpression
   FunctionExpression
   PrimaryExpression
*)

let op_prec op =
  match op with
    Eq | StarEq | SlashEq | ModEq | PlusEq | MinusEq -> 1, 13, 1
(*
  | Or -> 3, 3, 4
  | And -> 4, 4, 5
  | Bor -> 5, 5, 6
  | Bxor -> 6, 6, 7
  | Band -> 7, 7, 8
*)
  | Or -> 3, 3, 3
  | And -> 4, 4, 4
  | Bor -> 5, 5, 5
  | Bxor -> 6, 6, 6
  | Band -> 7, 7, 7
  | EqEq | NotEq | EqEqEq | NotEqEq -> 8, 8, 9
  | Lt | Le | InstanceOf -> 9, 9, 10
  | Lsl | Lsr | Asr -> 10, 10, 11
  | Plus | Minus -> 11, 11, 12
  | Mul | Div | Mod -> 12, 12, 13

let op_str op =
  match op with
    Eq      -> "="
  | StarEq  -> "*="
  | SlashEq -> "/="
  | ModEq   -> "%="
  | PlusEq  -> "+="
  | MinusEq -> "-="
  | Or      -> "||"
  | And     -> "&&"
  | Bor     -> "|"
  | Bxor    -> "^"
  | Band    -> "&"
  | EqEq    -> "=="
  | NotEq   -> "!="
  | EqEqEq  -> "==="
  | NotEqEq -> "!=="
  | Lt      -> "<"
  | Le      -> "<="
  | Lsl     -> "<<"
  | Lsr     -> ">>>"
  | Asr     -> ">>"
  | Plus    -> "+"
  | Minus   -> "-"
  | Mul     -> "*"
  | Div     -> "/"
  | Mod     -> "%"
  | InstanceOf -> assert false

let unop_str op =
  match op with
    Not -> "!"
  | Neg -> "-"
  | Pl  -> "+"
  | Typeof -> assert false

(*XXX May need to be updated... *)
let rec ends_with_if_without_else st =
  match st with
    If_statement (_, _, Some st) -> ends_with_if_without_else st
  | If_statement (_, _, None)    -> true
  | While_statement (_, st)      -> ends_with_if_without_else st
  | _                            -> false

let rec need_paren l e =
  match e with
    ESeq (e, _) ->
      l <= 0 && need_paren 0 e
  | ECond (e, _, _) ->
      l <= 2 && need_paren 3 e
  | EBin (op, e, _) ->
      let (out, lft, rght) = op_prec op in
      l <= out && need_paren lft e
  | ECall (e, _) | EAccess (e, _) | EDot (e, _) ->
      l <= 15 && need_paren 15 e
  | EVar _ | EStr _ | EArr _ | EBool _ | ENum _ | EQuote _ | EUn _ | ENew _ ->
      false
  | EFun _ | EObj _ ->
      true

let string_escape s =
  let l = String.length s in
  let b = Buffer.create (4 * l) in
  let conv = "0123456789abcdef" in
  for i = 0 to l - 1 do
    let c = s.[i] in
    match c with
      '\000' when i = l - 1 || s.[i + 1] < '0' || s.[i + 1] > '9' ->
        Buffer.add_string b "\\0"
    | '\b' ->
        Buffer.add_string b "\\b"
    | '\t' ->
        Buffer.add_string b "\\t"
    | '\n' ->
        Buffer.add_string b "\\n"
(* This escape sequence is not supported by IE < 9
    | '\011' ->
        Buffer.add_string b "\\v"
*)
    | '\012' ->
        Buffer.add_string b "\\f"
    | '\r' ->
        Buffer.add_string b "\\r"
    | '"' ->
        Buffer.add_string b "\\\""
    | '\\' ->
        Buffer.add_string b "\\\\"
    | '\000' .. '\031' | '\127' .. '\255' ->
        let c = Char.code c in
        Buffer.add_string b "\\x";
        Buffer.add_char b conv.[c lsr 4];
        Buffer.add_char b conv.[c land 0xf]
    | _ ->
        Buffer.add_char b c
  done;
  Buffer.contents b

let rec expression l f e =
  match e with
    EVar v ->
      PP.string f v
  | ESeq (e1, e2) ->
      if l > 0 then begin PP.start_group f 1; PP.string f "(" end;
      expression 0 f e1;
      PP.string f ",";
      PP.break f;
      expression 0 f e2;
      if l > 0 then begin PP.string f ")"; PP.end_group f end
  | EFun (i, l, b) ->
      PP.start_group f 1;
      PP.start_group f 0;
      PP.start_group f 0;
      PP.string f "function";
      opt_identifier f i;
      PP.end_group f;
      PP.break f;
      PP.start_group f 1;
      PP.string f "(";
      formal_parameter_list f l;
      PP.string f ")";
      PP.end_group f;
      PP.end_group f;
      PP.break f;
      PP.start_group f 1;
      PP.string f "{";
      function_body f b;
      PP.string f "}";
      PP.end_group f;
      PP.end_group f
  | ECall (e, el) ->
      if l > 15 then begin PP.start_group f 1; PP.string f "(" end;
      PP.start_group f 1;
      expression 15 f e;
      PP.break f;
      PP.start_group f 1;
      PP.string f "(";
      arguments f el;
      PP.string f ")";
      PP.end_group f;
      PP.end_group f;
      if l > 15 then begin PP.string f ")"; PP.end_group f end
  | EStr (s, `Bytes) ->
      PP.string f "\"";
      PP.string f (string_escape s);
      PP.string f "\""
  | EBool b ->
      PP.string f (if b then "true" else "false")
  | ENum v ->
      if v = infinity then
        PP.string f "Infinity"
      else if v = neg_infinity then begin
        if l > 13 then
          PP.string f "(-Infinity)"
        else
          PP.string f "-Infinity"
      end else if v <> v then
        PP.string f "NaN"
      else begin
        let s =
          let s1 = Printf.sprintf "%.12g" v in
          if v = float_of_string s1 then s1 else
          let s2 = Printf.sprintf "%.15g" v in
          if v = float_of_string s2 then s2 else
          Printf.sprintf "%.18g" v
        in
        if
          (* Negative numbers may need to be parenthesized. *)
          (l > 13 && (v < 0. || (v = 0. && 1. /. v < 0.)))
            ||
          (* Parenthesize as well when followed by a dot. *)
          (l = 15)
        then begin
          PP.string f "("; PP.string f s; PP.string f ")"
        end else
          PP.string f s
      end
  | EUn (Typeof, e) ->
      if l > 13 then begin PP.start_group f 1; PP.string f "(" end;
      PP.start_group f 0;
      PP.string f "typeof";
      PP.space f;
      expression 13 f e;
      PP.end_group f;
      if l > 13 then begin PP.string f ")"; PP.end_group f end
  | EUn (op, e) ->
      if l > 13 then begin PP.start_group f 1; PP.string f "(" end;
      PP.string f (unop_str op);
      expression 13 f e;
      if l > 13 then begin PP.string f ")"; PP.end_group f end
  | EBin (InstanceOf, e1, e2) ->
      let (out, lft, rght) = op_prec InstanceOf in
      if l > out then begin PP.start_group f 1; PP.string f "(" end;
      PP.start_group f 0;
      expression lft f e1;
      PP.space f;
      PP.string f "instanceof";
      PP.space f;
      expression rght f e2;
      PP.end_group f;
      if l > out then begin PP.string f ")"; PP.end_group f end
  | EBin (op, e1, e2) ->
      let (out, lft, rght) = op_prec op in
      if l > out then begin PP.start_group f 1; PP.string f "(" end;
      expression lft f e1;
      PP.string f (op_str op);
      PP.break f;
      expression rght f e2;
      if l > out then begin PP.string f ")"; PP.end_group f end
  | EArr el ->
      PP.start_group f 1;
      PP.string f "[";
      element_list f el;
      PP.string f "]";
      PP.end_group f
  | EAccess (e, e') ->
      if l > 15 then begin PP.start_group f 1; PP.string f "(" end;
      PP.start_group f 1;
      expression 15 f e;
      PP.break f;
      PP.start_group f 1;
      PP.string f "[";
      expression 0 f e';
      PP.string f "]";
      PP.end_group f;
      PP.end_group f;
      if l > 15 then begin PP.string f ")"; PP.end_group f end
  | EDot (e, nm) ->
      if l > 15 then begin PP.start_group f 1; PP.string f "(" end;
      expression 15 f e;
      PP.string f ".";
      PP.string f nm;
      if l > 15 then begin PP.string f ")"; PP.end_group f end
  | ENew (e, None) -> (*FIX: should omit parentheses when possible*)
      if l > 15 then begin PP.start_group f 1; PP.string f "(" end;
      PP.start_group f 1;
      PP.string f "new";
      PP.space f;
      expression 16 f e;
      PP.break f;
      PP.string f "()";
      PP.end_group f;
      if l > 15 then begin PP.string f ")"; PP.end_group f end
  | ENew (e, Some el) ->
      if l > 15 then begin PP.start_group f 1; PP.string f "(" end;
      PP.start_group f 1;
      PP.string f "new";
      PP.space f;
      expression 16 f e;
      PP.break f;
      PP.start_group f 1;
      PP.string f "(";
      arguments f el;
      PP.string f ")";
      PP.end_group f;
      PP.end_group f;
      if l > 15 then begin PP.string f ")"; PP.end_group f end
  | ECond (e, e1, e2) ->
      if l > 2 then begin PP.start_group f 1; PP.string f "(" end;
      PP.start_group f 1;
      PP.start_group f 0;
      expression 3 f e;
      PP.end_group f;
      PP.break f;
      PP.start_group f 1;
      PP.string f "?";
      expression 1 f e1;
      PP.end_group f;
      PP.break f;
      PP.start_group f 1;
      PP.string f ":";
      expression 1 f e2;
      PP.end_group f;
      PP.end_group f;
      if l > 2 then begin PP.string f ")"; PP.end_group f end
  | EObj lst ->
      PP.start_group f 1;
      PP.string f "{";
      property_name_and_value_list f lst;
      PP.string f "}";
      PP.end_group f
  | EQuote s ->
      PP.string f "(";
      PP.string f s;
      PP.string f ")"

and property_name f n =
  match n with
    PNI s -> PP.string f s
  | PNS s -> PP.string f "\""; PP.string f s; PP.string f "\""
  | PNN v -> expression 0 f (ENum v)

and property_name_and_value_list f l =
  match l with
    [] ->
      ()
  | [(pn, e)] ->
      PP.start_group f 0;
      property_name f pn;
      PP.string f ":";
      PP.break f;
      expression 1 f e;
      PP.end_group f
  | (pn, e) :: r ->
      PP.start_group f 0;
      property_name f pn;
      PP.string f ":";
      PP.break f;
      expression 1 f e;
      PP.end_group f;
      PP.string f ",";
      PP.break f;
      property_name_and_value_list f r

and element_list f el =
  match el with
    []     ->
      ()
  | [e]    ->
      begin match e with
        None   -> PP.string f ","
      | Some e -> PP.start_group f 0; expression 1 f e; PP.end_group f
      end
  | e :: r ->
      begin match e with
        None   -> ()
      | Some e -> PP.start_group f 0; expression 1 f e; PP.end_group f
      end;
      PP.string f ","; PP.break f; element_list f r

and function_body f b = source_elements f b

and arguments f l =
  match l with
    []     -> ()
  | [e]    -> PP.start_group f 0; expression 1 f e; PP.end_group f
  | e :: r -> PP.start_group f 0; expression 1 f e; PP.end_group f;
              PP.string f ","; PP.break f; arguments f r

and variable_declaration f (i, init) =
  match init with
    None   ->
      PP.string f i
  | Some e ->
      PP.start_group f 1;
      PP.string f i; PP.string f "="; PP.break f; expression 1 f e;
      PP.end_group f

and variable_declaration_list f l =
  match l with
    []     -> assert false
  | [d]    -> variable_declaration f d
  | d :: r -> variable_declaration f d; PP.string f ","; PP.break f;
              variable_declaration_list f r

and opt_expression l f e =
  match e with
    None   -> ()
  | Some e -> expression l f e

and statement f s =
  match s with
    Block b ->
      block f b
  | Variable_statement l ->
      begin match l with
        []  ->
          ()
      | [(i, None)] ->
          PP.start_group f 1;
          PP.string f "var";
          PP.space f;
          PP.string f i;
          PP.string f ";";
          PP.end_group f
      | [(i, Some e)] ->
          PP.start_group f 1;
          PP.string f "var";
          PP.space f;
          PP.string f i;
          PP.string f "=";
          PP.genbreak f "" 1;
          PP.start_group f 0;
          expression 1 f e;
          PP.string f ";";
          PP.end_group f;
          PP.end_group f
      | l ->
          PP.start_group f 1;
          PP.string f "var";
          PP.space f;
          variable_declaration_list f l;
          PP.string f ";";
          PP.end_group f
      end
  | Expression_statement e ->
      (* Parentheses are required when the expression
         starts syntactically with "{" or "function" *)
      if need_paren 0 e then begin
        PP.start_group f 1;
        PP.string f "(";
        expression 0 f e;
        PP.string f ");";
        PP.end_group f
      end else begin
        PP.start_group f 0;
        expression 0 f e;
        PP.string f ";";
        PP.end_group f
      end
  | If_statement (e, s1, (Some _ as s2)) when ends_with_if_without_else s1 ->
      (* Dangling else issue... *)
      statement f (If_statement (e, Block [s1], s2))
  | If_statement (e, s1, Some (Block _ as s2)) ->
      PP.start_group f 0;
      PP.start_group f 1;
      PP.string f "if";
      PP.break f;
      PP.start_group f 1;
      PP.string f "(";
      expression 0 f e;
      PP.string f ")";
      PP.end_group f;
      PP.end_group f;
      PP.genbreak f "" 1;
      PP.start_group f 0;
      statement f s1;
      PP.end_group f;
      PP.break f;
      PP.string f "else";
      PP.genbreak f "" 1;
      PP.start_group f 0;
      statement f s2;
      PP.end_group f;
      PP.end_group f
  | If_statement (e, s1, Some s2) ->
      PP.start_group f 0;
      PP.start_group f 1;
      PP.string f "if";
      PP.break f;
      PP.start_group f 1;
      PP.string f "(";
      expression 0 f e;
      PP.string f ")";
      PP.end_group f;
      PP.end_group f;
      PP.genbreak f "" 1;
      PP.start_group f 0;
      statement f s1;
      PP.end_group f;
      PP.break f;
      PP.string f "else";
      PP.genbreak f " " 1;
      PP.start_group f 0;
      statement f s2;
      PP.end_group f;
      PP.end_group f
  | If_statement (e, s1, None) ->
      PP.start_group f 1;
      PP.start_group f 0;
      PP.string f "if";
      PP.break f;
      PP.start_group f 1;
      PP.string f "(";
      expression 0 f e;
      PP.string f ")";
      PP.end_group f;
      PP.end_group f;
      PP.break f;
      PP.start_group f 0;
      statement f s1;
      PP.end_group f;
      PP.end_group f
  | While_statement (e, s) ->
      PP.start_group f 1;
      PP.start_group f 0;
      PP.string f "while";
      PP.break f;
      PP.start_group f 1;
      PP.string f "(";
      expression 0 f e;
      PP.string f ")";
      PP.end_group f;
      PP.end_group f;
      PP.break f;
      PP.start_group f 0;
      statement f s;
      PP.end_group f;
      PP.end_group f
  | Do_while_statement (Block _ as s, e) ->
      PP.start_group f 0;
      PP.string f "do";
      PP.genbreak f "" 1;
      PP.start_group f 0;
      statement f s;
      PP.end_group f;
      PP.break f;
      PP.string f "while";
      PP.genbreak f "" 1;
      PP.start_group f 1;
      PP.string f "(";
      expression 0 f e;
      PP.string f ")";
      PP.end_group f;
      PP.end_group f
  | Do_while_statement (s, e) ->
      PP.start_group f 0;
      PP.string f "do";
      PP.genbreak f " " 1;
      PP.start_group f 0;
      statement f s;
      PP.end_group f;
      PP.break f;
      PP.string f "while";
      PP.genbreak f "" 1;
      PP.start_group f 1;
      PP.string f "(";
      expression 0 f e;
      PP.string f ")";
      PP.end_group f;
      PP.end_group f
  | For_statement (e1, e2, e3, s) ->
      PP.start_group f 1;
      PP.start_group f 0;
      PP.string f "for";
      PP.break f;
      PP.start_group f 1;
      PP.string f "(";
      opt_expression 0 f e1;
      PP.string f ";"; PP.break f;
      opt_expression 0 f e2;
      PP.string f ";"; PP.break f;
      opt_expression 0 f e3;
      PP.string f ")";
      PP.end_group f;
      PP.end_group f;
      PP.break f;
      PP.start_group f 0;
      statement f s;
      PP.end_group f;
      PP.end_group f
  | Continue_statement None ->
      PP.string f "continue;"
  | Continue_statement (Some s) ->
      PP.string f "continue ";
      PP.string f s;
      PP.string f ";"
  | Break_statement None ->
      PP.string f "break;"
  | Break_statement (Some s) ->
      PP.string f "break ";
      PP.string f s;
      PP.string f ";"
  | Return_statement e ->
      begin match e with
        None   ->
          PP.string f "return;"
      | Some (EFun (i, l, b)) ->
          PP.start_group f 1;
          PP.start_group f 0;
          PP.start_group f 0;
          PP.string f "return function";
          opt_identifier f i;
          PP.end_group f;
          PP.break f;
          PP.start_group f 1;
          PP.string f "(";
          formal_parameter_list f l;
          PP.string f ")";
          PP.end_group f;
          PP.end_group f;
          PP.break f;
          PP.start_group f 1;
          PP.string f "{";
          function_body f b;
          PP.string f "};";
          PP.end_group f;
          PP.end_group f
      | Some e ->
          PP.start_group f 7;
          PP.string f "return ";
          PP.start_group f 0;
          expression 0 f e;
          PP.string f ";";
          PP.end_group f;
          PP.end_group f
          (* There MUST be a space between the return and its
             argument. A line return will not work *)
      end
  | Labelled_statement (i, s) ->
      PP.string f i;
      PP.string f ":";
      PP.break f;
      statement f s
  | Switch_statement (e, cc, def) ->
      PP.start_group f 1;
      PP.start_group f 0;
      PP.string f "switch";
      PP.break f;
      PP.start_group f 1;
      PP.string f "(";
      expression 0 f e;
      PP.string f ")";
      PP.end_group f;
      PP.end_group f;
      PP.break f;
      PP.start_group f 1;
      PP.string f "{";
      List.iter
        (fun (e, sl) ->
           PP.start_group f 1;
           PP.start_group f 1;
           PP.string f "case";
           PP.space f;
           expression 0 f e;
           PP.string f ":";
           PP.end_group f;
           PP.break f;
           PP.start_group f 0;
           statement_list f sl;
           PP.end_group f;
           PP.end_group f;
           PP.break f)
        cc;
      begin match def with
        None ->
          ()
      | Some def ->
          PP.start_group f 1;
          PP.string f "default:";
          PP.break f;
          PP.start_group f 0;
          statement_list f def;
          PP.end_group f;
          PP.end_group f
      end;
      PP.string f "}";
      PP.end_group f;
      PP.end_group f
  | Throw_statement e ->
      PP.start_group f 6;
      PP.string f "throw ";
      PP.start_group f 0;
      expression 0 f e;
      PP.string f ";";
      PP.end_group f;
      PP.end_group f
      (* There must be a space between the return and its
         argument. A line return would not work *)
  | Try_statement (b, ctch, fin) ->
      PP.start_group f 0;
      PP.string f "try";
      PP.genbreak f " " 1;
      block f b;
      begin match ctch with
        None ->
          ()
      | Some (i, b) ->
          PP.break f;
          PP.start_group f 1;
          PP.string f "catch(";
          PP.string f i;
          PP.string f ")";
          PP.break f;
          block f b;
          PP.end_group f
      end;
      begin match fin with
        None ->
          ()
      | Some b ->
          PP.break f;
          PP.start_group f 1;
          PP.string f "finally";
          PP.space f;
          block f b;
          PP.end_group f
      end;
      PP.end_group f

and statement_list f b =
  match b with
    []     -> ()
  | [s]    -> statement f s
  | s :: r -> statement f s; PP.break f; statement_list f r

and block f b =
  PP.start_group f 1;
  PP.string f "{";
  statement_list f b;
  PP.string f "}";
  PP.end_group f

and source_element f se =
  match se with
    Statement s ->
      statement f s
  | Function_declaration (i, l, b) ->
      PP.start_group f 1;
      PP.start_group f 0;
      PP.start_group f 0;
      PP.string f "function";
      PP.space f;
      PP.string f i;
      PP.end_group f;
      PP.break f;
      PP.start_group f 1;
      PP.string f "(";
      formal_parameter_list f l;
      PP.string f ")";
      PP.end_group f;
      PP.end_group f;
      PP.break f;
      PP.start_group f 1;
      PP.string f "{";
      function_body f b;
      PP.string f "}";
      PP.end_group f;
      PP.end_group f

and source_elements f se =
  match se with
    []     -> ()
  | [s]    -> source_element f s
  | s :: r -> source_element f s; PP.break f; source_elements f r

let program f se =
  PP.start_group f 0; source_elements f se; PP.end_group f; PP.newline f
