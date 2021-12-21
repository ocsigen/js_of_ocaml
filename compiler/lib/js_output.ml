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
  val source_map : Source_map.t option
end) =
struct
  let temp_mappings = ref []

  let push_mapping, get_file_index, get_name_index, source_map_enabled =
    let idx_files = ref 0 in
    let idx_names = ref 0 in
    let files = Hashtbl.create 17 in
    let names = Hashtbl.create 17 in
    match D.source_map with
    | None -> (fun _ _ -> ()), (fun _ -> -1), (fun _ -> -1), false
    | Some sm ->
        List.iter (List.rev sm.Source_map.sources) ~f:(fun f ->
            Hashtbl.add files f !idx_files;
            incr idx_files);
        ( (fun pos m -> temp_mappings := (pos, m) :: !temp_mappings)
        , (fun file ->
            try Hashtbl.find files file
            with Not_found ->
              let pos = !idx_files in
              Hashtbl.add files file pos;
              incr idx_files;
              sm.Source_map.sources <- file :: sm.Source_map.sources;
              pos)
        , (fun name ->
            try Hashtbl.find names name
            with Not_found ->
              let pos = !idx_names in
              Hashtbl.add names name pos;
              incr idx_names;
              sm.Source_map.names <- name :: sm.Source_map.names;
              pos)
        , true )

  let debug_enabled = Config.Flag.debuginfo ()

  let output_debug_info f loc =
    (if debug_enabled
    then
      match loc with
      | Pi { Parse_info.name = Some file; line; col; _ }
      | Pi { Parse_info.src = Some file; line; col; _ } ->
          PP.non_breaking_space f;
          PP.string f (Format.sprintf "/*<<%s %d %d>>*/" file line col);
          PP.non_breaking_space f
      | N -> ()
      | U | Pi _ ->
          PP.non_breaking_space f;
          PP.string f "/*<<?>>*/";
          PP.non_breaking_space f);
    if source_map_enabled
    then
      match loc with
      | N -> ()
      | U | Pi { Parse_info.src = None; _ } ->
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
      | None -> ()
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
      | Some _ -> ()

  let ident f = function
    | S { name; var = Some v; _ } ->
        output_debug_info_ident f name (Code.Var.get_loc v);
        PP.string f name
    | S { name; var = None; loc = Pi pi } ->
        output_debug_info_ident f name (Some pi);
        PP.string f name
    | S { name; var = None; loc = U | N } -> PP.string f name
    | V _v -> assert false

  let opt_identifier f i =
    match i with
    | None -> ()
    | Some i ->
        PP.space f;
        ident f i

  let rec formal_parameter_list f l =
    match l with
    | [] -> ()
    | [ i ] -> ident f i
    | i :: r ->
        ident f i;
        PP.string f ",";
        PP.break f;
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
    | BorEq -> 1, 13, 1
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
    | Gt | Ge | Lt | Le | InstanceOf | In -> 9, 9, 10
    | Lsl | Lsr | Asr -> 10, 10, 11
    | Plus | Minus -> 11, 11, 12
    | Mul | Div | Mod -> 12, 12, 13

  let op_str op =
    match op with
    | Eq -> "="
    | StarEq -> "*="
    | SlashEq -> "/="
    | ModEq -> "%="
    | PlusEq -> "+="
    | MinusEq -> "-="
    | Or -> "||"
    | And -> "&&"
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
    | Lt -> "<"
    | Le -> "<="
    | Gt -> ">"
    | Ge -> ">="
    | Lsl -> "<<"
    | Lsr -> ">>>"
    | Asr -> ">>"
    | Plus -> "+"
    | Minus -> "-"
    | Mul -> "*"
    | Div -> "/"
    | Mod -> "%"
    | InstanceOf | In -> assert false

  let unop_str op =
    match op with
    | Not -> "!"
    | Neg -> "-"
    | Pl -> "+"
    | Bnot -> "~"
    | IncrA | IncrB | DecrA | DecrB | Typeof | Void | Delete -> assert false

  let rec ends_with_if_without_else st =
    match fst st with
    | Labelled_statement (_, st)
    | If_statement (_, _, Some st)
    | While_statement (_, st)
    | For_statement (_, _, _, st)
    | ForIn_statement (_, _, st) -> ends_with_if_without_else st
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
    | Debugger_statement -> false

  let rec need_paren l e =
    match e with
    | ESeq (e, _) -> l <= 0 && need_paren 0 e
    | ECond (e, _, _) -> l <= 2 && need_paren 3 e
    | EBin (op, e, _) ->
        let out, lft, _rght = op_prec op in
        l <= out && need_paren lft e
    | ECall (e, _, _) | EAccess (e, _) | EDot (e, _) -> l <= 15 && need_paren 15 e
    | EVar _ | EStr _ | EArr _ | EBool _ | ENum _ | EQuote _ | ERegexp _ | EUn _ | ENew _
      -> false
    | EFun _ | EObj _ -> true

  let best_string_quote s =
    let simple = ref 0 and double = ref 0 in
    for i = 0 to String.length s - 1 do
      match s.[i] with
      | '\'' -> incr simple
      | '"' -> incr double
      | _ -> ()
    done;
    if !simple < !double then '\'' else '"'

  let array_conv = Array.init 16 ~f:(fun i -> "0123456789abcdef".[i])

  let pp_string f ?(quote = '"') ?(utf = false) s =
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
      | '\\' when not utf -> Buffer.add_string b "\\\\"
      | '\r' -> Buffer.add_string b "\\r"
      | '\000' .. '\031' | '\127' ->
          let c = Char.code c in
          Buffer.add_string b "\\x";
          Buffer.add_char b (Array.unsafe_get array_conv (c lsr 4));
          Buffer.add_char b (Array.unsafe_get array_conv (c land 0xf))
      | '\128' .. '\255' when not utf ->
          let c = Char.code c in
          Buffer.add_string b "\\x";
          Buffer.add_char b (Array.unsafe_get array_conv (c lsr 4));
          Buffer.add_char b (Array.unsafe_get array_conv (c land 0xf))
      | _ ->
          if Char.equal c quote
          then (
            Buffer.add_char b '\\';
            Buffer.add_char b c)
          else Buffer.add_char b c
    done;
    Buffer.add_char b quote;
    PP.string f (Buffer.contents b)

  let rec expression l f e =
    match e with
    | EVar v -> ident f v
    | ESeq (e1, e2) ->
        if l > 0
        then (
          PP.start_group f 1;
          PP.string f "(");
        expression 0 f e1;
        PP.string f ",";
        PP.break f;
        expression 0 f e2;
        if l > 0
        then (
          PP.string f ")";
          PP.end_group f)
    | EFun (i, l, b, pc) ->
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
        output_debug_info f pc;
        PP.string f "}";
        PP.end_group f;
        PP.end_group f
    | ECall (e, el, loc) ->
        if l > 15
        then (
          PP.start_group f 1;
          PP.string f "(");
        output_debug_info f loc;
        PP.start_group f 1;
        expression 15 f e;
        PP.break f;
        PP.start_group f 1;
        PP.string f "(";
        arguments f el;
        PP.string f ")";
        PP.end_group f;
        PP.end_group f;
        if l > 15
        then (
          PP.string f ")";
          PP.end_group f)
    | EStr (s, kind) ->
        let quote = best_string_quote s in
        pp_string f ~utf:Poly.(kind = `Utf8) ~quote s
    | EBool b -> PP.string f (if b then "true" else "false")
    | ENum num ->
        let s = Num.to_string num in
        let need_parent =
          if Num.is_neg num
          then l > 13 (* Negative numbers may need to be parenthesized. *)
          else
            l = 15
            (* Parenthesize as well when followed by a dot. *)
            && (not (Char.equal s.[0] 'I'))
            (* Infinity *)
            && not (Char.equal s.[0] 'N')
          (* NaN *)
        in
        if need_parent then PP.string f "(";
        PP.string f s;
        if need_parent then PP.string f ")"
    | EUn (Typeof, e) ->
        if l > 13
        then (
          PP.start_group f 1;
          PP.string f "(");
        PP.start_group f 0;
        PP.string f "typeof";
        PP.space f;
        expression 13 f e;
        PP.end_group f;
        if l > 13
        then (
          PP.string f ")";
          PP.end_group f)
    | EUn (Void, e) ->
        if l > 13
        then (
          PP.start_group f 1;
          PP.string f "(");
        PP.start_group f 0;
        PP.string f "void";
        PP.space f;
        expression 13 f e;
        PP.end_group f;
        if l > 13
        then (
          PP.string f ")";
          PP.end_group f)
    | EUn (Delete, e) ->
        if l > 13
        then (
          PP.start_group f 1;
          PP.string f "(");
        PP.start_group f 0;
        PP.string f "delete";
        PP.space f;
        expression 13 f e;
        PP.end_group f;
        if l > 13
        then (
          PP.string f ")";
          PP.end_group f)
    | EUn (((IncrA | DecrA | IncrB | DecrB) as op), e) ->
        if l > 13
        then (
          PP.start_group f 1;
          PP.string f "(");
        if Poly.(op = IncrA) || Poly.(op = DecrA) then expression 13 f e;
        if Poly.(op = IncrA) || Poly.(op = IncrB)
        then PP.string f "++"
        else PP.string f "--";
        if Poly.(op = IncrB) || Poly.(op = DecrB) then expression 13 f e;
        if l > 13
        then (
          PP.string f ")";
          PP.end_group f)
    | EUn (op, e) ->
        if l > 13
        then (
          PP.start_group f 1;
          PP.string f "(");
        PP.string f (unop_str op);
        PP.space f;
        expression 13 f e;
        if l > 13
        then (
          PP.string f ")";
          PP.end_group f)
    | EBin (InstanceOf, e1, e2) ->
        let out, lft, rght = op_prec InstanceOf in
        if l > out
        then (
          PP.start_group f 1;
          PP.string f "(");
        PP.start_group f 0;
        expression lft f e1;
        PP.space f;
        PP.string f "instanceof";
        PP.space f;
        expression rght f e2;
        PP.end_group f;
        if l > out
        then (
          PP.string f ")";
          PP.end_group f)
    | EBin (In, e1, e2) ->
        let out, lft, rght = op_prec InstanceOf in
        if l > out
        then (
          PP.start_group f 1;
          PP.string f "(");
        PP.start_group f 0;
        expression lft f e1;
        PP.space f;
        PP.string f "in";
        PP.space f;
        expression rght f e2;
        PP.end_group f;
        if l > out
        then (
          PP.string f ")";
          PP.end_group f)
    | EBin (op, e1, e2) ->
        let out, lft, rght = op_prec op in
        if l > out
        then (
          PP.start_group f 1;
          PP.string f "(");
        expression lft f e1;
        PP.space f;
        PP.string f (op_str op);
        PP.space f;
        expression rght f e2;
        if l > out
        then (
          PP.string f ")";
          PP.end_group f)
    | EArr el ->
        PP.start_group f 1;
        PP.string f "[";
        element_list f el;
        PP.string f "]";
        PP.end_group f
    | EAccess (e, e') ->
        if l > 15
        then (
          PP.start_group f 1;
          PP.string f "(");
        PP.start_group f 1;
        expression 15 f e;
        PP.break f;
        PP.start_group f 1;
        PP.string f "[";
        expression 0 f e';
        PP.string f "]";
        PP.end_group f;
        PP.end_group f;
        if l > 15
        then (
          PP.string f ")";
          PP.end_group f)
    | EDot (e, nm) ->
        if l > 15
        then (
          PP.start_group f 1;
          PP.string f "(");
        expression 15 f e;
        PP.string f ".";
        PP.string f nm;
        if l > 15
        then (
          PP.string f ")";
          PP.end_group f)
    | ENew (e, None) ->
        (*FIX: should omit parentheses when possible*)
        if l > 15
        then (
          PP.start_group f 1;
          PP.string f "(");
        PP.start_group f 1;
        PP.string f "new";
        PP.space f;
        expression 16 f e;
        PP.break f;
        PP.string f "()";
        PP.end_group f;
        if l > 15
        then (
          PP.string f ")";
          PP.end_group f)
    | ENew (e, Some el) ->
        if l > 15
        then (
          PP.start_group f 1;
          PP.string f "(");
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
        if l > 15
        then (
          PP.string f ")";
          PP.end_group f)
    | ECond (e, e1, e2) ->
        if l > 2
        then (
          PP.start_group f 1;
          PP.string f "(");
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
        if l > 2
        then (
          PP.string f ")";
          PP.end_group f)
    | EObj lst ->
        PP.start_group f 1;
        PP.string f "{";
        property_name_and_value_list f lst;
        PP.string f "}";
        PP.end_group f
    | ERegexp (s, opt) -> (
        PP.string f "/";
        PP.string f s;
        PP.string f "/";
        match opt with
        | None -> ()
        | Some o -> PP.string f o)
    | EQuote s ->
        PP.string f "(";
        PP.string f s;
        PP.string f ")"

  and property_name f n =
    match n with
    | PNI s -> PP.string f s
    | PNS s ->
        let quote = best_string_quote s in
        pp_string f ~utf:true ~quote s
    | PNN v -> expression 0 f (ENum v)

  and property_name_and_value_list f l =
    match l with
    | [] -> ()
    | [ (pn, e) ] ->
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
    | [] -> ()
    | [ e ] -> (
        match e with
        | None -> PP.string f ","
        | Some e ->
            PP.start_group f 0;
            expression 1 f e;
            PP.end_group f)
    | e :: r ->
        (match e with
        | None -> ()
        | Some e ->
            PP.start_group f 0;
            expression 1 f e;
            PP.end_group f);
        PP.string f ",";
        PP.break f;
        element_list f r

  and function_body f b = source_elements f ~skip_last_semi:true b

  and arguments f l =
    match l with
    | [] -> ()
    | [ (e, s) ] ->
        PP.start_group f 0;
        (match s with
        | `Spread -> PP.string f "..."
        | `Not_spread -> ());
        expression 1 f e;
        PP.end_group f
    | (e, s) :: r ->
        PP.start_group f 0;
        (match s with
        | `Spread -> PP.string f "..."
        | `Not_spread -> ());
        expression 1 f e;
        PP.end_group f;
        PP.string f ",";
        PP.break f;
        arguments f r

  and variable_declaration f (i, init) =
    match init with
    | None -> ident f i
    | Some (e, pc) ->
        PP.start_group f 1;
        output_debug_info f pc;
        ident f i;
        PP.string f "=";
        PP.break f;
        expression 1 f e;
        PP.end_group f

  and variable_declaration_list_aux f l =
    match l with
    | [] -> assert false
    | [ d ] -> variable_declaration f d
    | d :: r ->
        variable_declaration f d;
        PP.string f ",";
        PP.break f;
        variable_declaration_list_aux f r

  and variable_declaration_list close f = function
    | [] -> ()
    | [ (i, None) ] ->
        PP.start_group f 1;
        PP.string f "var";
        PP.space f;
        ident f i;
        if close then PP.string f ";";
        PP.end_group f
    | [ (i, Some (e, pc)) ] ->
        PP.start_group f 1;
        output_debug_info f pc;
        PP.string f "var";
        PP.space f;
        ident f i;
        PP.string f "=";
        PP.break1 f;
        PP.start_group f 0;
        expression 1 f e;
        if close then PP.string f ";";
        PP.end_group f;
        PP.end_group f
    | l ->
        PP.start_group f 1;
        PP.string f "var";
        PP.space f;
        variable_declaration_list_aux f l;
        if close then PP.string f ";";
        PP.end_group f

  and opt_expression l f e =
    match e with
    | None -> ()
    | Some e -> expression l f e

  and statement ?(last = false) f (s, loc) =
    let last_semi () = if last then () else PP.string f ";" in
    output_debug_info f loc;
    match s with
    | Block b -> block f b
    | Variable_statement l -> variable_declaration_list (not last) f l
    | Empty_statement -> PP.string f ";"
    | Debugger_statement ->
        PP.string f "debugger";
        last_semi ()
    | Expression_statement e ->
        (* Parentheses are required when the expression
           starts syntactically with "{" or "function" *)
        if need_paren 0 e
        then (
          PP.start_group f 1;
          PP.string f "(";
          expression 0 f e;
          PP.string f ")";
          last_semi ();
          PP.end_group f)
        else (
          PP.start_group f 0;
          expression 0 f e;
          last_semi ();
          PP.end_group f)
    | If_statement (e, s1, (Some _ as s2)) when ends_with_if_without_else s1 ->
        (* Dangling else issue... *)
        statement ~last f (If_statement (e, (Block [ s1 ], N), s2), N)
    | If_statement (e, s1, Some ((Block _, _) as s2)) ->
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
        PP.break1 f;
        PP.start_group f 0;
        statement f s1;
        PP.end_group f;
        PP.break f;
        PP.string f "else";
        PP.break1 f;
        PP.start_group f 0;
        statement ~last f s2;
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
        PP.break1 f;
        PP.start_group f 0;
        statement f s1;
        PP.end_group f;
        PP.break f;
        PP.string f "else";
        PP.space ~indent:1 f;
        PP.start_group f 0;
        statement ~last f s2;
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
        statement ~last f s1;
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
        statement ~last f s;
        PP.end_group f;
        PP.end_group f
    | Do_while_statement (((Block _, _) as s), e) ->
        PP.start_group f 0;
        PP.string f "do";
        PP.break1 f;
        PP.start_group f 0;
        statement f s;
        PP.end_group f;
        PP.break f;
        PP.string f "while";
        PP.break1 f;
        PP.start_group f 1;
        PP.string f "(";
        expression 0 f e;
        PP.string f ")";
        last_semi ();
        PP.end_group f;
        PP.end_group f
    | Do_while_statement (s, e) ->
        PP.start_group f 0;
        PP.string f "do";
        PP.space ~indent:1 f;
        PP.start_group f 0;
        statement f s;
        PP.end_group f;
        PP.break f;
        PP.string f "while";
        PP.break f;
        PP.start_group f 1;
        PP.string f "(";
        expression 0 f e;
        PP.string f ")";
        last_semi ();
        PP.end_group f;
        PP.end_group f
    | For_statement (e1, e2, e3, s) ->
        PP.start_group f 1;
        PP.start_group f 0;
        PP.string f "for";
        PP.break f;
        PP.start_group f 1;
        PP.string f "(";
        (match e1 with
        | Left e -> opt_expression 0 f e
        | Right l -> variable_declaration_list false f l);
        PP.string f ";";
        PP.break f;
        opt_expression 0 f e2;
        PP.string f ";";
        PP.break f;
        opt_expression 0 f e3;
        PP.string f ")";
        PP.end_group f;
        PP.end_group f;
        PP.break f;
        PP.start_group f 0;
        statement ~last f s;
        PP.end_group f;
        PP.end_group f
    | ForIn_statement (e1, e2, s) ->
        PP.start_group f 1;
        PP.start_group f 0;
        PP.string f "for";
        PP.break f;
        PP.start_group f 1;
        PP.string f "(";
        (match e1 with
        | Left e -> expression 0 f e
        | Right v -> variable_declaration_list false f [ v ]);
        PP.space f;
        PP.string f "in";
        PP.break f;
        PP.space f;
        expression 0 f e2;
        PP.string f ")";
        PP.end_group f;
        PP.end_group f;
        PP.break f;
        PP.start_group f 0;
        statement ~last f s;
        PP.end_group f;
        PP.end_group f
    | Continue_statement None ->
        PP.string f "continue";
        last_semi ()
    | Continue_statement (Some s) ->
        PP.string f "continue ";
        PP.string f (Javascript.Label.to_string s);
        last_semi ()
    | Break_statement None ->
        PP.string f "break";
        last_semi ()
    | Break_statement (Some s) ->
        PP.string f "break ";
        PP.string f (Javascript.Label.to_string s);
        last_semi ()
    | Return_statement e -> (
        match e with
        | None ->
            PP.string f "return";
            last_semi ()
        | Some (EFun (i, l, b, pc)) ->
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
            output_debug_info f pc;
            PP.string f "}";
            last_semi ();
            PP.end_group f;
            PP.end_group f
        | Some e ->
            PP.start_group f 7;
            PP.string f "return";
            PP.non_breaking_space f;
            PP.start_group f 0;
            expression 0 f e;
            last_semi ();
            PP.end_group f;
            PP.end_group f
            (* There MUST be a space between the return and its
               argument. A line return will not work *))
    | Labelled_statement (i, s) ->
        PP.string f (Javascript.Label.to_string i);
        PP.string f ":";
        PP.break f;
        statement ~last f s
    | Switch_statement (e, cc, def, cc') ->
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
        let output_one last (e, sl) =
          PP.start_group f 1;
          PP.start_group f 1;
          PP.string f "case";
          PP.space f;
          expression 0 f e;
          PP.string f ":";
          PP.end_group f;
          PP.break f;
          PP.start_group f 0;
          statement_list ~skip_last_semi:last f sl;
          PP.end_group f;
          PP.end_group f;
          PP.break f
        in
        let rec loop last = function
          | [] -> ()
          | [ x ] -> output_one last x
          | x :: xs ->
              output_one false x;
              loop last xs
        in
        loop (Option.is_none def && List.is_empty cc') cc;
        (match def with
        | None -> ()
        | Some def ->
            PP.start_group f 1;
            PP.string f "default:";
            PP.break f;
            PP.start_group f 0;
            statement_list ~skip_last_semi:(List.is_empty cc') f def;
            PP.end_group f;
            PP.end_group f);
        loop true cc';
        PP.string f "}";
        PP.end_group f;
        PP.end_group f
    | Throw_statement e ->
        PP.start_group f 6;
        PP.string f "throw";
        PP.non_breaking_space f;
        PP.start_group f 0;
        expression 0 f e;
        last_semi ();
        PP.end_group f;
        PP.end_group f
    (* There must be a space between the return and its
       argument. A line return would not work *)
    | Try_statement (b, ctch, fin) ->
        PP.start_group f 0;
        PP.string f "try";
        PP.space ~indent:1 f;
        block f b;
        (match ctch with
        | None -> ()
        | Some (i, b) ->
            PP.break f;
            PP.start_group f 1;
            PP.string f "catch(";
            ident f i;
            PP.string f ")";
            PP.break f;
            block f b;
            PP.end_group f);
        (match fin with
        | None -> ()
        | Some b ->
            PP.break f;
            PP.start_group f 1;
            PP.string f "finally";
            PP.space f;
            block f b;
            PP.end_group f);
        PP.end_group f

  and statement_list f ?skip_last_semi b =
    match b with
    | [] -> ()
    | [ s ] -> statement f ?last:skip_last_semi s
    | s :: r ->
        statement f s;
        PP.break f;
        statement_list f ?skip_last_semi r

  and block f b =
    PP.start_group f 1;
    PP.string f "{";
    statement_list ~skip_last_semi:true f b;
    PP.string f "}";
    PP.end_group f

  and source_element f ?skip_last_semi se =
    match se with
    | Statement s, loc -> statement f ?last:skip_last_semi (s, loc)
    | Function_declaration (i, l, b, loc'), loc ->
        output_debug_info f loc;
        PP.start_group f 1;
        PP.start_group f 0;
        PP.start_group f 0;
        PP.string f "function";
        PP.space f;
        ident f i;
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
        output_debug_info f loc';
        PP.string f "}";
        PP.end_group f;
        PP.end_group f

  and source_elements f ?skip_last_semi se =
    match se with
    | [] -> ()
    | [ s ] -> source_element f ?skip_last_semi s
    | s :: r ->
        source_element f s;
        PP.break f;
        source_elements f ?skip_last_semi r

  and program f s = source_elements f s
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

let program f ?source_map p =
  let smo =
    match source_map with
    | None -> None
    | Some (_, sm) -> Some sm
  in
  let module O = Make (struct
    let source_map = smo
  end) in
  PP.set_needed_space_function f need_space;
  PP.start_group f 0;
  O.program f p;
  PP.end_group f;
  PP.newline f;
  (match source_map with
  | None -> ()
  | Some (out_file, sm) ->
      let sm =
        { sm with
          Source_map.sources = List.rev sm.Source_map.sources
        ; Source_map.names = List.rev sm.Source_map.names
        }
      in
      let sources = sm.Source_map.sources in
      let sources_content =
        match sm.Source_map.sources_content with
        | None -> None
        | Some [] ->
            Some
              (List.map sources ~f:(fun file ->
                   match Builtins.find file with
                   | Some f -> Some (Builtins.File.content f)
                   | None ->
                       if Sys.file_exists file
                       then
                         let content = Fs.read_file file in
                         Some content
                       else None))
        | Some _ -> assert false
      in
      let sources =
        List.map sources ~f:(fun filename ->
            match Builtins.find filename with
            | None -> filename
            | Some _ -> Filename.concat "/builtin" filename)
      in
      let mappings =
        List.map !O.temp_mappings ~f:(fun (pos, m) ->
            { m with
              (* [p_line] starts at zero, [gen_line] at 1 *)
              Source_map.gen_line = pos.PP.p_line + 1
            ; Source_map.gen_col = pos.PP.p_col
            })
      in
      let sm = { sm with Source_map.sources; sources_content; mappings } in
      let urlData =
        match out_file with
        | None ->
            let data = Source_map_io.to_string sm in
            "data:application/json;base64," ^ Base64.encode_exn data
        | Some out_file ->
            Source_map_io.to_file sm out_file;
            Filename.basename out_file
      in
      PP.newline f;
      PP.string f (Printf.sprintf "//# sourceMappingURL=%s\n" urlData));
  if stats ()
  then
    let size i = Printf.sprintf "%.2fKo" (float_of_int i /. 1024.) in
    let _percent n d =
      Printf.sprintf "%.1f%%" (float_of_int n *. 100. /. float_of_int d)
    in
    let total_s = PP.total f in
    Format.eprintf "total size : %s@." (size total_s)
