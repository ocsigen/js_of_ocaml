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

(* Beware automatic semi-colon insertion...
   {v
     a=b
     ++c
   v}
   is not the same as
   {v
     a=b ++c
   v}

   see so-called "restricted productions":
   the space cannot be replaced by a newline in the following expressions:
   {v
     e ++
     e --
     continue e
     break e
     return e
     throw
   v}
*)
open! Stdlib

let stats = Debug.find "output"

open Javascript
module PP = Pretty_print

module Make (D : sig
  val push_mapping : Pretty_print.pos -> Source_map.map -> unit

  val get_file_index : string -> int

  val get_name_index : string -> int

  val source_map_enabled : bool
end) =
struct
  open D

  let debug_enabled = Config.Flag.debuginfo ()

  let output_debug_info f loc =
    (if debug_enabled
    then
      match loc with
      | Pi { Parse_info.src = None | Some ""; name = None | Some ""; _ } | N -> ()
      | U ->
          PP.non_breaking_space f;
          PP.string f "/*<<?>>*/";
          PP.non_breaking_space f
      | Pi { Parse_info.src; name; line; col; _ } ->
          let file =
            match name, src with
            | (None | Some ""), Some file -> file
            | Some file, (None | Some "") -> file
            | Some file, Some _file -> file
            | None, None -> assert false
          in
          PP.non_breaking_space f;
          PP.string f (Format.sprintf "/*<<%s:%d:%d>>*/" file line col);
          PP.non_breaking_space f);
    if source_map_enabled
    then
      match loc with
      | N -> ()
      | U | Pi { Parse_info.src = None | Some ""; _ } ->
          push_mapping
            (PP.pos f)
            { Source_map.gen_line = -1
            ; gen_col = -1
            ; ori_source = -1
            ; ori_line = -1
            ; ori_col = -1
            ; ori_name = None
            }
      | Pi { Parse_info.src = Some file; line; col; _ } ->
          push_mapping
            (PP.pos f)
            { Source_map.gen_line = -1
            ; gen_col = -1
            ; ori_source = get_file_index file
            ; ori_line = line
            ; ori_col = col
            ; ori_name = None
            }

  let output_debug_info_ident f nm loc =
    if source_map_enabled
    then
      match loc with
      | None | Some { Parse_info.src = Some "" | None; _ } -> ()
      | Some { Parse_info.src = Some file; line; col; _ } ->
          push_mapping
            (PP.pos f)
            { Source_map.gen_line = -1
            ; gen_col = -1
            ; ori_source = get_file_index file
            ; ori_line = line
            ; ori_col = col
            ; ori_name = Some (get_name_index nm)
            }

  let ident f = function
    | S { name = Utf8 name; var = Some v; _ } ->
        output_debug_info_ident f name (Code.Var.get_loc v);
        if false then PP.string f (Printf.sprintf "/* %d */" (Code.Var.idx v));
        PP.string f name
    | S { name = Utf8 name; var = None; loc = Pi pi } ->
        output_debug_info_ident f name (Some pi);
        PP.string f name
    | S { name = Utf8 name; var = None; loc = U | N } -> PP.string f name
    | V v ->
        assert (Debug.find "shortvar" ());
        PP.string f ("<" ^ Code.Var.to_string v ^ ">")

  let opt_identifier f i =
    match i with
    | None -> ()
    | Some i ->
        PP.space f;
        ident f i

  let early_error _ = assert false

  type prec =
    | Expression (* 0  *)
    | AssignementExpression (* 1  *)
    | ConditionalExpression (* 2  *)
    | ShortCircuitExpression
    | CoalesceExpression
    | LogicalORExpression (* 3  *)
    | LogicalANDExpression (* 4  *)
    | BitwiseORExpression (* 5  *)
    | BitwiseXORExpression (* 6  *)
    | BitwiseANDExpression (* 7  *)
    | EqualityExpression (* 8  *)
    | RelationalExpression (* 9  *)
    | ShiftExpression (* 10 *)
    | AdditiveExpression (* 11 *)
    | MultiplicativeExpression (* 12 *)
    | ExponentiationExpression
    | UnaryExpression (* 13 *)
    | UpdateExpression (* 14 *)
    | LeftHandSideExpression (* 15 *)
    | NewExpression
    | CallOrMemberExpression
    | MemberExpression (* 16 *)

  module Prec = struct
    let compare (a : prec) (b : prec) = Poly.compare a b

    [@@@ocaml.warning "-32"]

    let ( <= ) a b = compare a b <= 0

    let ( >= ) a b = compare a b >= 0

    let ( < ) a b = compare a b < 0

    let ( > ) a b = compare a b > 0

    let ( = ) a b = compare a b = 0
  end

  let op_prec op =
    match op with
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
    | OrEq
    | AndEq
    | ExpEq
    | CoalesceEq -> AssignementExpression, LeftHandSideExpression, AssignementExpression
    | Coalesce -> CoalesceExpression, BitwiseORExpression, BitwiseORExpression
    | Or -> LogicalORExpression, LogicalORExpression, LogicalORExpression
    | And -> LogicalANDExpression, LogicalANDExpression, LogicalANDExpression
    | Bor -> BitwiseORExpression, BitwiseORExpression, BitwiseORExpression
    | Bxor -> BitwiseXORExpression, BitwiseXORExpression, BitwiseXORExpression
    | Band -> BitwiseANDExpression, BitwiseANDExpression, BitwiseANDExpression
    | EqEq | NotEq | EqEqEq | NotEqEq ->
        EqualityExpression, EqualityExpression, RelationalExpression
    | Gt | GtInt | Ge | GeInt | Lt | LtInt | Le | LeInt | InstanceOf | In ->
        RelationalExpression, RelationalExpression, ShiftExpression
    | Lsl | Lsr | Asr -> ShiftExpression, ShiftExpression, AdditiveExpression
    | Plus | Minus -> AdditiveExpression, AdditiveExpression, MultiplicativeExpression
    | Mul | Div | Mod ->
        MultiplicativeExpression, MultiplicativeExpression, ExponentiationExpression
    | Exp -> ExponentiationExpression, UpdateExpression, ExponentiationExpression

  let op_str op =
    match op with
    | Eq -> "="
    | StarEq -> "*="
    | SlashEq -> "/="
    | ModEq -> "%="
    | PlusEq -> "+="
    | MinusEq -> "-="
    | Or -> "||"
    | OrEq -> "||="
    | And -> "&&"
    | AndEq -> "&&="
    | Bor -> "|"
    | Bxor -> "^"
    | Band -> "&"
    | EqEq -> "=="
    | NotEq -> "!="
    | EqEqEq -> "==="
    | NotEqEq -> "!=="
    | LslEq -> "<<="
    | AsrEq -> ">>="
    | LsrEq -> ">>>="
    | BandEq -> "&="
    | BxorEq -> "^="
    | BorEq -> "|="
    | Lt | LtInt -> "<"
    | Le | LeInt -> "<="
    | Gt | GtInt -> ">"
    | Ge | GeInt -> ">="
    | Lsl -> "<<"
    | Lsr -> ">>>"
    | Asr -> ">>"
    | Plus -> "+"
    | Minus -> "-"
    | Mul -> "*"
    | Div -> "/"
    | Mod -> "%"
    | Exp -> "**"
    | ExpEq -> "**="
    | CoalesceEq -> "??="
    | Coalesce -> "??"
    | InstanceOf | In -> assert false

  let unop_str op =
    match op with
    | Not -> "!"
    | Neg -> "-"
    | Pl -> "+"
    | Bnot -> "~"
    | IncrA | IncrB | DecrA | DecrB | Typeof | Void | Delete | Await -> assert false

  let rec ends_with_if_without_else st =
    match fst st with
    | Labelled_statement (_, st)
    | If_statement (_, _, Some st)
    | While_statement (_, st)
    | For_statement (_, _, _, st)
    | ForIn_statement (_, _, st) -> ends_with_if_without_else st
    | ForOf_statement (_, _, st) -> ends_with_if_without_else st
    | If_statement (_, _, None) -> true
    | Block _
    | Variable_statement _
    | Empty_statement
    | Expression_statement _
    | Continue_statement _
    | Break_statement _
    | Return_statement _
    | Throw_statement _
    | Do_while_statement _
    | Switch_statement _
    | Try_statement _
    | Function_declaration _
    | Class_declaration _
    | Debugger_statement -> false

  let starts_with ~obj ~funct ~let_identifier ~async_identifier l e =
    let rec traverse l e =
      match e with
      | EObj _ -> obj
      | EFun _ -> funct
      | EVar (S { name = Utf8 "let"; _ }) -> let_identifier
      | EVar (S { name = Utf8 "async"; _ }) -> async_identifier
      | ESeq (e, _) -> Prec.(l <= Expression) && traverse Expression e
      | ECond (e, _, _) ->
          Prec.(l <= ConditionalExpression) && traverse ShortCircuitExpression e
      | EAssignTarget (ObjectBinding _) -> obj
      | EAssignTarget (ArrayBinding _) -> false
      | EBin (op, e, _) ->
          let out, lft, _rght = op_prec op in
          Prec.(l <= out) && traverse lft e
      | EUn ((IncrA | DecrA), e) ->
          Prec.(l <= UpdateExpression) && traverse LeftHandSideExpression e
      | ECallTemplate (e, _, _) | ECall (e, _, _, _) | EAccess (e, _, _) | EDot (e, _, _)
        -> traverse CallOrMemberExpression e
      | EArrow _
      | EVar _
      | EStr _
      | ETemplate _
      | EArr _
      | EBool _
      | ENum _
      | ERegexp _
      | EUn _
      | ENew _
      | EClass _
      | EYield _ -> false
      | CoverCallExpressionAndAsyncArrowHead e
      | CoverParenthesizedExpressionAndArrowParameterList e -> early_error e
    in
    traverse l e

  let best_string_quote s =
    let simple = ref 0 and double = ref 0 in
    for i = 0 to String.length s - 1 do
      match s.[i] with
      | '\'' -> incr simple
      | '"' -> incr double
      | _ -> ()
    done;
    if !simple < !double then '\'' else '"'

  let pp_string f ?(quote = '"') s =
    let l = String.length s in
    let b = Buffer.create (String.length s + 2) in
    Buffer.add_char b quote;
    for i = 0 to l - 1 do
      let c = s.[i] in
      match c with
      | '\000' when i = l - 1 || not (Char.is_num s.[i + 1]) -> Buffer.add_string b "\\0"
      | '\b' -> Buffer.add_string b "\\b"
      | '\t' -> Buffer.add_string b "\\t"
      | '\n' -> Buffer.add_string b "\\n"
      (* This escape sequence is not supported by IE < 9
         | '\011' -> "\\v"
      *)
      | '\012' -> Buffer.add_string b "\\f"
      (* https://github.com/ocsigen/js_of_ocaml/issues/898 *)
      | '/' when i > 0 && Char.equal s.[i - 1] '<' -> Buffer.add_string b "\\/"
      | '\r' -> Buffer.add_string b "\\r"
      | '\000' .. '\031' | '\127' ->
          Buffer.add_string b "\\x";
          Buffer.add_char_hex b c
      | _ ->
          if Char.equal c quote
          then (
            Buffer.add_char b '\\';
            Buffer.add_char b c)
          else Buffer.add_char b c
    done;
    Buffer.add_char b quote;
    PP.string f (Buffer.contents b)

  let rec comma_list f f_elt l =
    match l with
    | [] -> ()
    | [ x ] ->
        PP.start_group f 0;
        f_elt f x;
        PP.end_group f
    | x :: r ->
        PP.start_group f 0;
        f_elt f x;
        PP.end_group f;
        PP.string f ",";
        PP.space f;
        comma_list f f_elt r

  let comma_list_rest f f_elt l f_rest rest =
    match l, rest with
    | [], None -> ()
    | [], Some rest ->
        PP.start_group f 0;
        PP.string f "...";
        f_rest f rest;
        PP.end_group f
    | l, None -> comma_list f f_elt l
    | l, Some r ->
        comma_list f f_elt l;
        PP.string f ",";
        PP.space f;
        PP.start_group f 0;
        PP.string f "...";
        f_rest f r;
        PP.end_group f

  let rec expression (l : prec) f e =
    match e with
    | EVar v -> ident f v
    | ESeq (e1, e2) ->
        if Prec.(l > Expression)
        then (
          PP.start_group f 1;
          PP.string f "(");
        expression Expression f e1;
        PP.string f ",";
        PP.space f;
        expression Expression f e2;
        if Prec.(l > Expression)
        then (
          PP.string f ")";
          PP.end_group f)
    | EFun (i, (k, l, b, pc)) ->
        let prefix =
          match k with
          | { async = false; generator = false } -> "function"
          | { async = true; generator = false } -> "async function"
          | { async = true; generator = true } -> "async function*"
          | { async = false; generator = true } -> "function*"
        in
        function_declaration f prefix ident i l b pc
    | EClass (i, cl_decl) ->
        PP.string f "class";
        (match i with
        | None -> ()
        | Some i ->
            PP.space f;
            ident f i);
        class_declaration f cl_decl
    | EArrow ((k, p, b, pc), _) ->
        if Prec.(l > AssignementExpression)
        then (
          PP.start_group f 1;
          PP.string f "(");
        PP.start_group f 1;
        PP.start_group f 0;
        (match k with
        | { async = true; generator = false } ->
            PP.string f "async";
            PP.space f
        | { async = false; generator = false } -> ()
        | { async = true | false; generator = true } -> assert false);
        PP.break f;
        (match p with
        | { list = [ ((BindingIdent _, None) as x) ]; rest = None } ->
            formal_parameter f x;
            PP.string f "=>"
        | _ ->
            PP.start_group f 1;
            PP.string f "(";
            formal_parameter_list f p;
            PP.string f ")=>";
            PP.end_group f);
        PP.end_group f;
        (match b with
        | [ (Return_statement (Some e), loc) ] ->
            (* Should not starts with '{' *)
            PP.start_group f 1;
            PP.break1 f;
            output_debug_info f loc;
            parenthesized_expression ~obj:true AssignementExpression f e;
            PP.end_group f
        | l ->
            let b =
              match l with
              | [ (Block l, _) ] -> l
              | l -> l
            in
            PP.string f "{";
            PP.break f;
            function_body f b;
            output_debug_info f pc;
            PP.string f "}");
        PP.end_group f;
        if Prec.(l > AssignementExpression)
        then (
          PP.string f ")";
          PP.end_group f)
    | ECall (e, access_kind, el, loc) ->
        (* Need parentheses also if within an expression [new e] *)
        if Prec.(l = NewExpression || l > CallOrMemberExpression)
        then (
          PP.start_group f 1;
          PP.string f "(");
        output_debug_info f loc;
        PP.start_group f 1;
        expression CallOrMemberExpression f e;
        PP.break f;
        PP.start_group f 1;
        (match access_kind with
        | ANormal -> PP.string f "("
        | ANullish -> PP.string f "?.(");
        arguments f el;
        PP.string f ")";
        PP.end_group f;
        PP.end_group f;
        if Prec.(l = NewExpression || l > CallOrMemberExpression)
        then (
          PP.string f ")";
          PP.end_group f)
    | ECallTemplate (e, t, loc) ->
        (* Need parentheses also if within an expression [new e] *)
        if Prec.(l = NewExpression || l > CallOrMemberExpression)
        then (
          PP.start_group f 1;
          PP.string f "(");
        output_debug_info f loc;
        PP.start_group f 1;
        expression CallOrMemberExpression f e;
        PP.break f;
        PP.start_group f 1;
        template f t;
        PP.end_group f;
        PP.end_group f;
        if Prec.(l = NewExpression || l > CallOrMemberExpression)
        then (
          PP.string f ")";
          PP.end_group f)
    | EStr (Utf8 s) ->
        let quote = best_string_quote s in
        pp_string f ~quote s
    | ETemplate l -> template f l
    | EBool b -> PP.string f (if b then "true" else "false")
    | ENum num ->
        let s = Num.to_string num in
        let need_parent =
          if Num.is_neg num
          then
            Prec.(l > UnaryExpression)
            (* Negative numbers may need to be parenthesized. *)
          else
            Prec.(l >= CallOrMemberExpression)
            (* Parenthesize as well when followed by a dot. *)
            && (not (Char.equal s.[0] 'I'))
            (* Infinity *)
            && not (Char.equal s.[0] 'N')
          (* NaN *)
        in
        if need_parent then PP.string f "(";
        PP.string f s;
        if need_parent then PP.string f ")"
    | EUn (((Typeof | Void | Delete | Await) as op), e) ->
        let p = UnaryExpression in
        if Prec.(l > p)
        then (
          PP.start_group f 1;
          PP.string f "(");
        PP.start_group f 0;
        let name =
          match op with
          | Typeof -> "typeof"
          | Void -> "void"
          | Delete -> "delete"
          | Await -> "await"
          | _ -> assert false
        in
        PP.string f name;
        PP.space f;
        expression p f e;
        PP.end_group f;
        if Prec.(l > p)
        then (
          PP.string f ")";
          PP.end_group f)
    | EUn (((IncrB | DecrB) as op), e) ->
        let p = UpdateExpression in
        if Prec.(l > p)
        then (
          PP.start_group f 1;
          PP.string f "(");
        if Poly.(op = IncrB) then PP.string f "++" else PP.string f "--";
        expression UnaryExpression f e;
        if Prec.(l > p)
        then (
          PP.string f ")";
          PP.end_group f)
    | EUn (((IncrA | DecrA) as op), e) ->
        let p = UpdateExpression in
        if Prec.(l > p)
        then (
          PP.start_group f 1;
          PP.string f "(");
        expression LeftHandSideExpression f e;
        if Poly.(op = IncrA) then PP.string f "++" else PP.string f "--";
        if Prec.(l > p)
        then (
          PP.string f ")";
          PP.end_group f)
    | EUn (op, e) ->
        let p = UnaryExpression in
        let need_parent = Prec.(l > p) in
        if need_parent
        then (
          PP.start_group f 1;
          PP.string f "(");
        PP.string f (unop_str op);
        PP.space f;
        expression p f e;
        if need_parent
        then (
          PP.string f ")";
          PP.end_group f)
    | EBin (((InstanceOf | In) as op), e1, e2) ->
        let out, lft, rght = op_prec InstanceOf in
        if Prec.(l > out)
        then (
          PP.start_group f 1;
          PP.string f "(");
        PP.start_group f 0;
        expression lft f e1;
        PP.space f;
        let name =
          match op with
          | InstanceOf -> "instanceof"
          | In -> "in"
          | _ -> assert false
        in
        PP.string f name;
        PP.space f;
        expression rght f e2;
        PP.end_group f;
        if Prec.(l > out)
        then (
          PP.string f ")";
          PP.end_group f)
    | EBin
        ( (( Eq
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
           | OrEq
           | AndEq
           | ExpEq
           | CoalesceEq ) as op)
        , e1
        , e2 ) ->
        let out, lft, rght = op_prec op in
        let lft =
          (* We can have e sequence of coalesce: e1 ?? e2 ?? e3,
             but each expressions should be a BitwiseORExpression *)
          match e1, op with
          | EBin (Coalesce, _, _), Coalesce -> CoalesceExpression
          | _ -> lft
        in
        PP.start_group f 0;
        if Prec.(l > out) then PP.string f "(";
        PP.start_group f 0;
        expression lft f e1;
        PP.space f;
        PP.string f (op_str op);
        PP.end_group f;
        PP.start_group f 1;
        PP.space f;
        expression rght f e2;
        PP.end_group f;
        if Prec.(l > out) then PP.string f ")";
        PP.end_group f
    | EBin (op, e1, e2) ->
        let out, lft, rght = op_prec op in
        let lft =
          (* We can have e sequence of coalesce: e1 ?? e2 ?? e3,
             but each expressions should be a BitwiseORExpression *)
          match e1, op with
          | EBin (Coalesce, _, _), Coalesce -> CoalesceExpression
          | _ -> lft
        in
        PP.start_group f 0;
        if Prec.(l > out) then PP.string f "(";
        expression lft f e1;
        PP.space f;
        PP.start_group f 1;
        PP.string f (op_str op);
        PP.space f;
        expression rght f e2;
        if Prec.(l > out) then PP.string f ")";
        PP.end_group f;
        PP.end_group f
    | EAssignTarget p -> pattern f p
    | EArr el ->
        PP.start_group f 1;
        PP.string f "[";
        element_list f el;
        PP.string f "]";
        PP.end_group f
    | EAccess (e, access_kind, e') ->
        PP.start_group f 1;
        let l' =
          match l with
          | NewExpression | MemberExpression -> MemberExpression
          | _ -> CallOrMemberExpression
        in
        expression l' f e;
        PP.break f;
        PP.start_group f 1;
        (match access_kind with
        | ANormal -> PP.string f "["
        | ANullish -> PP.string f "?.[");
        expression Expression f e';
        PP.string f "]";
        PP.end_group f;
        PP.end_group f
    | EDot (e, access_kind, Utf8 nm) ->
        (* We keep tracks of whether call expression are allowed
           without parentheses within this expression *)
        let l' =
          match l with
          | NewExpression | MemberExpression -> MemberExpression
          | _ -> CallOrMemberExpression
        in
        expression l' f e;
        (match access_kind with
        | ANormal -> PP.string f "."
        | ANullish -> PP.string f "?.");
        PP.string f nm
    | ENew (e, None) ->
        if Prec.(l > NewExpression)
        then (
          PP.start_group f 1;
          PP.string f "(");
        PP.start_group f 1;
        PP.string f "new";
        PP.space f;
        expression NewExpression f e;
        PP.break f;
        PP.string f "()";
        PP.end_group f;
        if Prec.(l > NewExpression)
        then (
          PP.string f ")";
          PP.end_group f)
    | ENew (e, Some el) ->
        PP.start_group f 1;
        PP.string f "new";
        PP.space f;
        expression MemberExpression f e;
        PP.break f;
        PP.start_group f 1;
        PP.string f "(";
        arguments f el;
        PP.string f ")";
        PP.end_group f;
        PP.end_group f
    | ECond (e, e1, e2) ->
        if Prec.(l > ConditionalExpression)
        then (
          PP.start_group f 1;
          PP.string f "(");
        PP.start_group f 1;
        PP.start_group f 0;
        expression ShortCircuitExpression f e;
        PP.end_group f;
        PP.space f;
        PP.start_group f 1;
        PP.start_group f 0;
        PP.string f "?";
        PP.space f;
        PP.end_group f;
        expression AssignementExpression f e1;
        PP.end_group f;
        PP.space f;
        PP.start_group f 1;
        PP.start_group f 0;
        PP.string f ":";
        PP.space f;
        PP.end_group f;
        expression AssignementExpression f e2;
        PP.end_group f;
        PP.end_group f;
        if Prec.(l > ConditionalExpression)
        then (
          PP.string f ")";
          PP.end_group f)
    | EObj lst ->
        PP.start_group f 1;
        PP.string f "{";
        property_list f lst;
        PP.string f "}";
        PP.end_group f
    | ERegexp (s, opt) -> (
        PP.string f "/";
        PP.string f s;
        PP.string f "/";
        match opt with
        | None -> ()
        | Some o -> PP.string f o)
    | EYield e -> (
        match e with
        | None -> PP.string f "yield"
        | Some e ->
            if Prec.(l > AssignementExpression)
            then (
              PP.start_group f 1;
              PP.string f "(");
            PP.start_group f 7;
            PP.string f "yield";
            PP.non_breaking_space f;
            PP.start_group f 0;
            expression AssignementExpression f e;
            PP.end_group f;
            PP.end_group f;
            if Prec.(l > AssignementExpression)
            then (
              PP.start_group f 1;
              PP.string f ")"
              (* There MUST be a space between the yield and its
                 argument. A line return will not work *)))
    | CoverCallExpressionAndAsyncArrowHead e
    | CoverParenthesizedExpressionAndArrowParameterList e -> early_error e

  and template f l =
    PP.string f "`";
    List.iter l ~f:(function
        | TStr (Utf8 s) -> PP.string f s
        | TExp e ->
            PP.string f "${";
            expression AssignementExpression f e;
            PP.string f "}");
    PP.string f "`"

  and property_name f n =
    match n with
    | PNI (Utf8 s) -> PP.string f s
    | PNS (Utf8 s) ->
        let quote = best_string_quote s in
        pp_string f ~quote s
    | PNN v -> expression Expression f (ENum v)
    | PComputed e ->
        PP.string f "[";
        expression Expression f e;
        PP.string f "]"

  and property_list f l = comma_list f property l

  and property f p =
    match p with
    | Property (pn, e) ->
        PP.start_group f 0;
        property_name f pn;
        PP.string f ":";
        PP.space f;
        expression AssignementExpression f e;
        PP.end_group f
    | PropertySpread e ->
        PP.string f "...";
        expression AssignementExpression f e
    | PropertyMethod (n, m) -> method_ f property_name n m
    | CoverInitializedName (e, _, _) -> early_error e

  and method_ : 'a. _ -> (PP.t -> 'a -> unit) -> 'a -> method_ -> unit =
    fun (type a) f (name : PP.t -> a -> unit) (n : a) (m : method_) ->
     match m with
     | MethodGet (k, l, b, loc') | MethodSet (k, l, b, loc') ->
         (match k with
         | { async = false; generator = false } -> ()
         | _ -> assert false);
         let prefix =
           match m with
           | MethodGet _ -> "get"
           | MethodSet _ -> "set"
           | _ -> assert false
         in
         function_declaration f prefix name (Some n) l b loc'
     | Method (k, l, b, loc') ->
         let fpn f () =
           (match k with
           | { async = false; generator = false } -> ()
           | { async = false; generator = true } ->
               PP.string f "*";
               PP.space f
           | { async = true; generator = false } ->
               PP.string f "async";
               PP.space f
           | { async = true; generator = true } ->
               PP.string f "async*";
               PP.space f);
           name f n
         in
         function_declaration f "" fpn (Some ()) l b loc'

  and element_list f el = comma_list f element el

  and element f (e : element) =
    match e with
    | ElementHole -> ()
    | Element e ->
        PP.start_group f 0;
        expression AssignementExpression f e;
        PP.end_group f
    | ElementSpread e ->
        PP.start_group f 0;
        PP.string f "...";
        expression AssignementExpression f e;
        PP.end_group f

  and formal_parameter f e = binding_element f e

  and formal_parameter_list f { list; rest } =
    comma_list_rest f formal_parameter list binding rest

  and function_body f b = statement_list f ~skip_last_semi:true b

  and argument f a =
    PP.start_group f 0;
    (match a with
    | Arg e -> expression AssignementExpression f e
    | ArgSpread e ->
        PP.string f "...";
        expression AssignementExpression f e);
    PP.end_group f

  and arguments f l = comma_list f argument l

  and variable_declaration f x =
    match x with
    | DeclIdent (i, None) -> ident f i
    | DeclIdent (i, Some (e, loc)) ->
        PP.start_group f 1;
        output_debug_info f loc;
        PP.start_group f 0;
        ident f i;
        PP.space f;
        PP.string f "=";
        PP.end_group f;
        PP.start_group f 1;
        PP.space f;
        expression AssignementExpression f e;
        PP.end_group f;
        PP.end_group f
    | DeclPattern (p, (e, loc)) ->
        PP.start_group f 1;
        output_debug_info f loc;
        PP.start_group f 0;
        pattern f p;
        PP.space f;
        PP.string f "=";
        PP.end_group f;
        PP.start_group f 1;
        PP.space f;
        expression AssignementExpression f e;
        PP.end_group f;
        PP.end_group f

  and binding_property f x =
    match x with
    | Prop_binding (pn, e) ->
        property_name f pn;
        PP.string f ":";
        PP.space f;
        binding_element f e
    | Prop_ident (i, None) -> ident f i
    | Prop_ident (i, Some (e, loc)) ->
        ident f i;
        PP.space f;
        PP.string f "=";
        PP.space f;
        output_debug_info f loc;
        expression AssignementExpression f e

  and binding_element f (b, (e : initialiser option)) =
    match e with
    | None -> binding f b
    | Some (e, loc) ->
        output_debug_info f loc;
        binding f b;
        PP.space f;
        PP.string f "=";
        PP.space f;
        expression AssignementExpression f e

  and binding f x =
    match x with
    | BindingIdent id -> ident f id
    | BindingPattern p -> pattern f p

  and binding_array_elt f x =
    match x with
    | None -> ()
    | Some e -> binding_element f e

  and pattern f p =
    match p with
    | ObjectBinding { list; rest } ->
        PP.start_group f 1;
        PP.string f "{";
        comma_list_rest f binding_property list ident rest;
        PP.string f "}";
        PP.end_group f
    | ArrayBinding { list; rest } ->
        PP.start_group f 1;
        PP.string f "[";
        comma_list_rest f binding_array_elt list binding rest;
        PP.string f "]";
        PP.end_group f

  and variable_declaration_list_aux f l =
    match l with
    | [] -> assert false
    | [ d ] -> variable_declaration f d
    | d :: r ->
        variable_declaration f d;
        PP.string f ",";
        PP.space f;
        variable_declaration_list_aux f r

  and variable_declaration_kind f kind =
    match kind with
    | Var -> PP.string f "var"
    | Let -> PP.string f "let"
    | Const -> PP.string f "const"

  and variable_declaration_list kind close f = function
    | [] -> ()
    | [ x ] ->
        let x, loc =
          match x with
          | DeclIdent (_, None) as x -> x, N
          | DeclIdent (i, Some (e, loc)) -> DeclIdent (i, Some (e, N)), loc
          | DeclPattern (p, (e, loc)) -> DeclPattern (p, (e, N)), loc
        in
        PP.start_group f 1;
        output_debug_info f loc;
        variable_declaration_kind f kind;
        PP.space f;
        variable_declaration f x;
        if close then PP.string f ";";
        PP.end_group f
    | l ->
        PP.start_group f 1;
        variable_declaration_kind f kind;
        PP.space f;
        variable_declaration_list_aux f l;
        if close then PP.string f ";";
        PP.end_group f

  and parenthesized_expression
      ?(last_semi = fun () -> ())
      ?(obj = false)
      ?(funct = false)
      ?(let_identifier = false)
      ?(async_identifier = false)
      l
      f
      e =
    if starts_with ~obj ~funct ~let_identifier ~async_identifier l e
    then (
      PP.start_group f 1;
      PP.string f "(";
      expression l f e;
      PP.string f ")";
      last_semi ();
      PP.end_group f)
    else (
      PP.start_group f 0;
      expression l f e;
      last_semi ();
      PP.end_group f)

  and for_binding f k v =
    variable_declaration_kind f k;
    PP.space f;
    binding f v

  and statement1 ?last f s =
    match s with
    | Block _, _ -> statement ?last f s
    | _ ->
        PP.space ~indent:1 f;
        PP.start_group f 0;
        statement ?last f s;
        PP.end_group f

  and statement ?(last = false) f (s, loc) =
    let can_omit_semi = PP.compact f && last in
    let last_semi () = if can_omit_semi then () else PP.string f ";" in
    output_debug_info f loc;
    match s with
    | Block b -> block f b
    | Variable_statement (k, l) -> variable_declaration_list k (not can_omit_semi) f l
    | Function_declaration (i, (k, l, b, loc')) ->
        let prefix =
          match k with
          | { async = false; generator = false } -> "function"
          | { async = true; generator = false } -> "async function"
          | { async = true; generator = true } -> "async function*"
          | { async = false; generator = true } -> "function*"
        in
        function_declaration f prefix ident (Some i) l b loc'
    | Class_declaration (i, cl_decl) ->
        PP.string f "class";
        PP.space f;
        ident f i;
        class_declaration f cl_decl
    | Empty_statement -> PP.string f ";"
    | Debugger_statement ->
        PP.string f "debugger";
        last_semi ()
    | Expression_statement e ->
        (* Parentheses are required when the expression
           starts syntactically with "{", "function", "async function"
           or "let [" *)
        parenthesized_expression
          ~last_semi
          ~obj:true
          ~funct:true
          ~let_identifier:true
          Expression
          f
          e
    | If_statement (e, s1, (Some _ as s2)) when ends_with_if_without_else s1 ->
        (* Dangling else issue... *)
        statement ~last f (If_statement (e, (Block [ s1 ], N), s2), N)
    | If_statement (e, s1, s2) ->
        let rec ite kw e s1 s2 =
          (let last_in_s1 =
             match s2 with
             | None -> Some last
             | Some _ -> None
           in
           PP.start_group f 0;
           PP.start_group f 1;
           PP.string f kw;
           PP.break f;
           PP.start_group f 1;
           PP.string f "(";
           expression Expression f e;
           PP.string f ")";
           PP.end_group f;
           PP.end_group f;
           statement1 ?last:last_in_s1 f s1);
          match s2 with
          | None -> PP.end_group f
          | Some (If_statement (e, s1, s2), _) when not (ends_with_if_without_else s1) ->
              PP.space f;
              ite "else if" e s1 s2;
              PP.end_group f
          | Some s2 ->
              PP.space f;
              PP.string f "else";
              statement1 ~last f s2;
              PP.end_group f
        in
        ite "if" e s1 s2
    | While_statement (e, s) ->
        PP.start_group f 0;
        PP.start_group f 0;
        PP.string f "while";
        PP.break f;
        PP.start_group f 1;
        PP.string f "(";
        expression Expression f e;
        PP.string f ")";
        PP.end_group f;
        PP.end_group f;
        statement1 ~last f s;
        PP.end_group f
    | Do_while_statement (s, e) ->
        PP.start_group f 0;
        PP.string f "do";
        statement1 f s;
        PP.break f;
        PP.string f "while";
        PP.break1 f;
        PP.start_group f 1;
        PP.string f "(";
        expression Expression f e;
        PP.string f ")";
        last_semi ();
        PP.end_group f;
        PP.end_group f
    | For_statement (e1, e2, e3, s) ->
        PP.start_group f 0;
        PP.start_group f 0;
        PP.string f "for";
        PP.break f;
        PP.start_group f 1;
        PP.string f "(";
        (match e1 with
        | Left None -> ()
        | Left (Some e) ->
            (* Should not starts with "let [" *)
            parenthesized_expression ~let_identifier:true Expression f e
        | Right (k, l) -> variable_declaration_list k false f l);
        PP.string f ";";
        (match e2 with
        | None -> ()
        | Some e2 ->
            PP.space f;
            expression Expression f e2);
        PP.string f ";";
        (match e3 with
        | None -> ()
        | Some e3 ->
            PP.space f;
            expression Expression f e3);
        PP.string f ")";
        PP.end_group f;
        PP.end_group f;
        statement1 ~last f s;
        PP.end_group f
    | ForIn_statement (e1, e2, s) ->
        PP.start_group f 0;
        PP.start_group f 0;
        PP.string f "for";
        PP.break f;
        PP.start_group f 1;
        PP.string f "(";
        (match e1 with
        | Left e ->
            (* Should not starts with "let [" *)
            parenthesized_expression ~let_identifier:true Expression f e
        | Right (k, v) -> for_binding f k v);
        PP.space f;
        PP.string f "in";
        PP.break f;
        PP.space f;
        expression Expression f e2;
        PP.string f ")";
        PP.end_group f;
        PP.end_group f;
        statement1 ~last f s;
        PP.end_group f
    | ForOf_statement (e1, e2, s) ->
        PP.start_group f 0;
        PP.start_group f 0;
        PP.string f "for";
        PP.break f;
        PP.start_group f 1;
        PP.string f "(";
        (match e1 with
        | Left e ->
            (* Should not starts with "let" or "async of" *)
            parenthesized_expression
              ~let_identifier:true
              ~async_identifier:true
              Expression
              f
              e
        | Right (k, v) -> for_binding f k v);
        PP.space f;
        PP.string f "of";
        PP.break f;
        PP.space f;
        expression Expression f e2;
        PP.string f ")";
        PP.end_group f;
        PP.end_group f;
        statement1 ~last f s;
        PP.end_group f
    | Continue_statement None ->
        PP.string f "continue";
        last_semi ()
    | Continue_statement (Some s) ->
        PP.string f "continue ";
        let (Utf8 l) = Javascript.Label.to_string s in
        PP.string f l;
        last_semi ()
    | Break_statement None ->
        PP.string f "break";
        last_semi ()
    | Break_statement (Some s) ->
        PP.string f "break ";
        let (Utf8 l) = Javascript.Label.to_string s in
        PP.string f l;
        last_semi ()
    | Return_statement e -> (
        match e with
        | None ->
            PP.string f "return";
            last_semi ()
        | Some (EFun (i, ({ async = false; generator = false }, l, b, pc))) ->
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
            PP.string f "{";
            PP.break f;
            function_body f b;
            output_debug_info f pc;
            PP.string f "}";
            last_semi ();
            PP.end_group f
        | Some e ->
            PP.start_group f 7;
            PP.string f "return";
            PP.non_breaking_space f;
            PP.start_group f 0;
            expression Expression f e;
            last_semi ();
            PP.end_group f;
            PP.end_group f
            (* There MUST be a space between the return and its
               argument. A line return will not work *))
    | Labelled_statement (i, s) ->
        let (Utf8 l) = Javascript.Label.to_string i in
        PP.string f l;
        PP.string f ":";
        PP.space f;
        statement ~last f s
    | Switch_statement (e, cc, def, cc') ->
        PP.start_group f 1;
        PP.start_group f 0;
        PP.string f "switch";
        PP.break f;
        PP.start_group f 1;
        PP.string f "(";
        expression Expression f e;
        PP.string f ")";
        PP.end_group f;
        PP.end_group f;
        PP.start_group f 1;
        PP.string f "{";
        PP.break f;
        let output_one last (e, sl) =
          PP.start_group f 1;
          PP.string f "case";
          PP.space f;
          expression Expression f e;
          PP.string f ":";
          PP.end_group f;
          PP.start_group f 1;
          (match sl with
          | _ :: _ -> PP.space f
          | [] -> PP.break f);
          PP.start_group f 0;
          statement_list ~skip_last_semi:last f sl;
          PP.end_group f;
          PP.end_group f
        in
        let rec loop last = function
          | [] -> ()
          | [ x ] ->
              output_one last x;
              if not last then PP.break f
          | x :: xs ->
              output_one false x;
              PP.break f;
              loop last xs
        in
        loop (Option.is_none def && List.is_empty cc') cc;
        (match def with
        | None -> ()
        | Some def ->
            PP.start_group f 1;
            PP.string f "default:";
            PP.space f;
            PP.start_group f 0;
            let last = List.is_empty cc' in
            statement_list ~skip_last_semi:last f def;
            PP.end_group f;
            PP.end_group f;
            if not last then PP.break f);
        loop true cc';
        PP.end_group f;
        PP.end_group f;
        PP.break f;
        PP.string f "}"
    | Throw_statement e ->
        PP.start_group f 6;
        PP.string f "throw";
        PP.non_breaking_space f;
        PP.start_group f 0;
        expression Expression f e;
        last_semi ();
        PP.end_group f;
        PP.end_group f
    (* There must be a space between the return and its
       argument. A line return would not work *)
    | Try_statement (b, ctch, fin) ->
        PP.start_group f 0;
        PP.string f "try";
        block f b;
        (match ctch with
        | None -> ()
        | Some (i, b) ->
            PP.break f;
            (match i with
            | None -> PP.string f "catch"
            | Some i ->
                PP.string f "catch(";
                formal_parameter f i;
                PP.string f ")");
            block f b);
        (match fin with
        | None -> ()
        | Some b ->
            PP.break f;
            PP.string f "finally";
            block f b);
        PP.end_group f

  and statement_list f ?skip_last_semi b =
    match b with
    | [] -> ()
    | [ s ] -> statement f ?last:skip_last_semi s
    | s :: r ->
        statement f s;
        PP.space f;
        statement_list f ?skip_last_semi r

  and block f b =
    PP.start_group f 0;
    PP.start_group f 1;
    PP.string f "{";
    PP.break f;
    statement_list ~skip_last_semi:true f b;
    PP.end_group f;
    PP.break f;
    PP.string f "}";
    PP.end_group f

  and function_declaration :
      type a. 'pp -> string -> ('pp -> a -> unit) -> a option -> _ -> _ -> _ -> unit =
   fun f prefix (pp_name : _ -> a -> unit) (name : a option) l body loc ->
    PP.start_group f 0;
    PP.start_group f 0;
    PP.start_group f 0;
    PP.string f prefix;
    (match name with
    | None -> ()
    | Some name ->
        if not (String.is_empty prefix) then PP.space f;
        pp_name f name);
    PP.end_group f;
    PP.break f;
    PP.start_group f 1;
    PP.string f "(";
    formal_parameter_list f l;
    PP.string f ")";
    PP.end_group f;
    PP.end_group f;
    PP.start_group f 1;
    PP.string f "{";
    PP.break f;
    function_body f body;
    PP.end_group f;
    PP.break f;
    output_debug_info f loc;
    PP.string f "}";
    PP.end_group f

  and class_declaration f x =
    Option.iter x.extends ~f:(fun e ->
        PP.space f;
        PP.string f "extends";
        PP.space f;
        expression Expression f e);
    PP.string f "{";
    List.iter x.body ~f:(fun x ->
        match x with
        | CEMethod (static, n, m) ->
            if static
            then (
              PP.string f "static";
              PP.space f);
            method_ f class_element_name n m;
            PP.break f
        | CEField (static, n, i) ->
            if static
            then (
              PP.string f "static";
              PP.space f);
            class_element_name f n;
            (match i with
            | None -> ()
            | Some (e, loc) ->
                PP.space f;
                PP.string f "=";
                PP.space f;
                output_debug_info f loc;
                expression Expression f e);
            PP.break f
        | CEStaticBLock l ->
            PP.string f "static";
            block f l;
            PP.break f);
    PP.string f "}"

  and class_element_name f x =
    match x with
    | PropName n -> property_name f n
    | PrivName i ->
        PP.string f "#";
        ident f i

  and program f s = statement_list f s
end

let part_of_ident =
  let a =
    Array.init 256 ~f:(fun i ->
        match Char.chr i with
        | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '$' -> true
        | _ -> false)
  in
  fun c -> Array.unsafe_get a (Char.code c)

let need_space a b =
  (* do not concat 2 different identifier *)
  (part_of_ident a && part_of_ident b)
  (* do not generate end_of_line_comment.
     handle the case of "num / /* comment */ b " *)
  ||
  match a, b with
  | '/', '/'
  (* https://github.com/ocsigen/js_of_ocaml/issues/507 *)
  | '-', '-'
  | '+', '+' -> true
  | _, _ -> false

let hashtbl_to_list htb =
  Hashtbl.fold (fun k v l -> (k, v) :: l) htb []
  |> List.sort ~cmp:(fun (_, a) (_, b) -> compare a b)
  |> List.map ~f:fst

let program f ?source_map p =
  let temp_mappings = ref [] in
  let files = Hashtbl.create 17 in
  let names = Hashtbl.create 17 in
  let contents : string option list ref option =
    match source_map with
    | None | Some { Source_map.sources_content = None; _ } -> None
    | Some { Source_map.sources_content = Some _; _ } -> Some (ref [])
  in
  let push_mapping, get_file_index, get_name_index, source_map_enabled =
    let source_map_enabled =
      match source_map with
      | None -> false
      | Some sm ->
          let rec loop s sc =
            match s, sc with
            | [], _ -> ()
            | x :: xs, [] ->
                Hashtbl.add files x (Hashtbl.length files);
                Option.iter contents ~f:(fun r -> r := None :: !r);
                loop xs []
            | x :: xs, y :: ys ->
                Hashtbl.add files x (Hashtbl.length files);
                Option.iter contents ~f:(fun r -> r := y :: !r);
                loop xs ys
          in
          loop sm.sources (Option.value ~default:[] sm.sources_content);
          List.iter sm.Source_map.names ~f:(fun f ->
              Hashtbl.add names f (Hashtbl.length names));
          true
    in
    let find_source file =
      match Builtins.find file with
      | Some f -> Some (Builtins.File.content f)
      | None ->
          if Sys.file_exists file && not (Sys.is_directory file)
          then
            let content = Fs.read_file file in
            Some content
          else None
    in
    ( (fun pos m -> temp_mappings := (pos, m) :: !temp_mappings)
    , (fun file ->
        try Hashtbl.find files file
        with Not_found ->
          let pos = Hashtbl.length files in
          Hashtbl.add files file pos;
          Option.iter contents ~f:(fun r -> r := find_source file :: !r);
          pos)
    , (fun name ->
        try Hashtbl.find names name
        with Not_found ->
          let pos = Hashtbl.length names in
          Hashtbl.add names name pos;
          pos)
    , source_map_enabled )
  in
  let module O = Make (struct
    let push_mapping = push_mapping

    let get_name_index = get_name_index

    let get_file_index = get_file_index

    let source_map_enabled = source_map_enabled
  end) in
  PP.set_needed_space_function f need_space;
  if Config.Flag.effects () then PP.set_adjust_indentation_function f (fun n -> n mod 40);
  PP.start_group f 0;
  O.program f p;
  PP.end_group f;
  PP.newline f;
  let sm =
    match source_map with
    | None -> None
    | Some sm ->
        let sources = hashtbl_to_list files in
        let names = hashtbl_to_list names in
        let sources_content =
          match contents with
          | None -> None
          | Some r -> Some (List.rev !r)
        in
        let sources =
          List.map sources ~f:(fun filename ->
              match Builtins.find filename with
              | None -> filename
              | Some _ -> Filename.concat "/builtin" filename)
        in
        let mappings =
          List.rev_append_map !temp_mappings sm.mappings ~f:(fun (pos, m) ->
              { m with
                (* [p_line] starts at zero, [gen_line] at 1 *)
                Source_map.gen_line = pos.PP.p_line + 1
              ; Source_map.gen_col = pos.PP.p_col
              })
        in
        Some { sm with Source_map.sources; names; sources_content; mappings }
  in
  (if stats ()
  then
    let size i = Printf.sprintf "%.2fKo" (float_of_int i /. 1024.) in
    let _percent n d =
      Printf.sprintf "%.1f%%" (float_of_int n *. 100. /. float_of_int d)
    in
    let total_s = PP.total f in
    Format.eprintf "total size : %s@." (size total_s));
  sm
