
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

let opt_identifier f i =
  match i with
    None   -> ()
  | Some i -> Format.fprintf f "@ %s" i

let rec formal_parameter_list f l =
  match l with
    []     -> ()
  | [i]    -> Format.fprintf f "%s" i
  | i :: r -> Format.fprintf f "%s,@,%a" i formal_parameter_list r

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
  | Or -> 3, 3, 4
  | And -> 4, 4, 5
  | Bor -> 5, 5, 6
  | Bxor -> 6, 6, 7
  | Band -> 7, 7, 8
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
  | EVar _ | EStr _ | EArr _ | ENum _ | EQuote _ | EUn _ | ENew _ ->
      false
  | EFun _ | EObj _ ->
      true

let rec expression l f e =
  match e with
    EVar v ->
      Format.fprintf f "%s" v
  | ESeq (e1, e2) ->
      if l > 0 then
        Format.fprintf f "@[<1>(%a,@,%a)@]" (expression 0) e1 (expression 1) e2
      else
        Format.fprintf f "%a,@,%a" (expression 0) e1 (expression 1) e2
  | EFun (i, l, b) ->
      Format.fprintf f "@[<2>function%a@,@[<1>(%a)@]@,@[<1>{%a}@]@]"
        opt_identifier i formal_parameter_list l function_body b
  | ECall (e, el) ->
      if l > 15 then Format.fprintf f "@[<1>(";
      Format.fprintf f "@[<2>%a@,@[<1>(%a)@]@]" (expression 15) e arguments el;
      if l > 15 then Format.fprintf f ")@]"
  | EStr s ->
      (* XXX Properly quote the string *)
      Format.fprintf f "\"%s\"" (String.escaped s)
  | ENum v ->
      if v = infinity then
        Format.fprintf f "Infinity"
      else if v = neg_infinity then begin
        if l > 13 then
          Format.fprintf f "(-Infinity)"
        else
          Format.fprintf f "-Infinity"
      end else if v <> v then
        Format.fprintf f "NaN"
      else if v > 0. || (v = 0. && 1. /. v > 0.) then
        Format.fprintf f "%g" v
      else if l > 13 then
        Format.fprintf f "(%g)" v
      else
        Format.fprintf f "%g" v
  | EUn (op, e) ->
      if l > 13 then Format.fprintf f "@[<1>(";
      Format.fprintf f "%s%a" (unop_str op) (expression 13) e;
      if l > 13 then Format.fprintf f ")@]"
  | EBin (InstanceOf, e1, e2) ->
      let (out, lft, rght) = op_prec InstanceOf in
      if l > out then Format.fprintf f "@[<1>(";
      Format.fprintf f "@[%a@ instanceof@ %a@]"
        (expression lft) e1 (expression rght) e2;
      if l > out then Format.fprintf f ")@]"
  | EBin (op, e1, e2) ->
      let (out, lft, rght) = op_prec op in
      if l > out then Format.fprintf f "@[<1>(";
      Format.fprintf f "%a%s%a"
        (expression lft) e1 (op_str op) (expression rght) e2;
      if l > out then Format.fprintf f ")@]"
  | EArr el ->
      Format.fprintf f "@[<1>[%a]@]" element_list el
  | EAccess (e, e') ->
      if l > 15 then Format.fprintf f "@[<1>(";
      Format.fprintf f "@[<2>%a@,@[<1>[%a]@]@]"
        (expression 15) e (expression 0) e';
      if l > 15 then Format.fprintf f ")@]"
  | EDot (e, nm) ->
      if l > 15 then Format.fprintf f "@[<1>(";
      Format.fprintf f "%a.%s" (expression 15) e nm;
      if l > 15 then Format.fprintf f ")@]"
  | ENew (e, None) ->
      if l > 15 then Format.fprintf f "@[<1>(";
      Format.fprintf f "@[<2>new %a@]" (expression 15) e;
      if l > 15 then Format.fprintf f ")@]"
  | ENew (e, Some el) ->
      if l > 15 then Format.fprintf f "@[<1>(";
      Format.fprintf f "@[<2>new %a@,@[<1>(%a)@]@]"
        (expression 16) e arguments el;
      if l > 15 then Format.fprintf f ")@]"
  | ECond (e, e1, e2) ->
      if l > 2 then Format.fprintf f "@[<1>(";
      Format.fprintf f "%a?%a:%a"
        (expression 3) e (expression 1) e1 (expression 1) e2;
      if l > 2 then Format.fprintf f ")@]"
  | EObj lst ->
      Format.fprintf f "@[<1>{%a}@]" property_name_and_value_list lst
  | EQuote s ->
      Format.fprintf f "(%s)" s

and property_name f n =
  match n with
    PNI s -> Format.fprintf f "%s" s
  | PNS s -> Format.fprintf f "\"%s\"" s
  | PNN v -> expression 0 f (ENum v)

and property_name_and_value_list f l =
  match l with
    [] ->
      ()
  | [(pn, e)] ->
      Format.fprintf f "@[%a:@,%a@]" property_name pn (expression 1) e
  | (pn, e) :: r ->
      Format.fprintf f "@[%a:@,%a@],@,%a"
        property_name pn (expression 1) e property_name_and_value_list r

and element_list f el =
  match el with
    []     ->
      ()
  | [e]    ->
      begin match e with
        None   -> Format.fprintf f ","
      | Some e -> expression 1 f e
      end
  | e :: r ->
      begin match e with
        None   -> Format.fprintf f ",@,%a" element_list r
      | Some e -> Format.fprintf f "%a,@,%a" (expression 1) e element_list r
      end

and function_body f b = source_elements f b

and arguments f l =
  match l with
    []     -> ()
  | [e]    -> Format.fprintf f "%a" (expression 1) e
  | e :: r -> Format.fprintf f "%a,@,%a"  (expression 1) e arguments r

and variable_declaration f (i, init) =
  match init with
    None   -> Format.fprintf f "%s" i
  | Some e -> Format.fprintf f "@[<2>%s=@,%a@]" i (expression 1) e

and variable_declaration_list f l =
  match l with
    []     -> assert false
  | [d]    -> Format.fprintf f "%a" variable_declaration d
  | d :: r -> Format.fprintf f "%a,@,%a"
                variable_declaration d variable_declaration_list r

and statement f s =
  match s with
    Block b ->
      block f b
  | Variable_statement l ->
      begin match l with
        []  ->
          ()
      | [(i, None)] ->
          Format.fprintf f "@[<2>var@ %s;@]" i
      | [(i, Some e)] ->
          Format.fprintf f "@[<2>var %s=@,%a;@]" i (expression 1) e
      | l ->
          Format.fprintf f "@[<2>var@ %a;@]" variable_declaration_list l
      end
  | Expression_statement e ->
      (* Parentheses are required when the expression
         starts syntactically with "{" or "function" *)
      if need_paren 0 e then
        Format.fprintf f "@[<1>(%a);@]" (expression 0) e
      else
        Format.fprintf f "@[%a;@]" (expression 0) e
  | If_statement (e, s1, Some s2) ->
      Format.fprintf f "@[<1>if@,@[(%a)@]@,@[%a@]@;<0 -1>else@ @[<1>%a@]@]"
        (expression 0) e statement s1 statement s2
  | If_statement (e, s1, None) ->
      Format.fprintf f "@[<1>if@ @[(%a)@]@ @[%a@]@]"
        (expression 0) e statement s1
  | Return_statement e ->
      begin match e with
        None   -> Format.fprintf f "return;"
      | Some e -> Format.fprintf f "@[<1>return @[%a;@]@]" (expression 0) e
                  (* There must be a space between the return and its
                     argument. A line return would not work *)
      end
  | Switch_statement (e, cc, def) ->
      Format.fprintf f "@[<1>switch@ (%a)@ {@," (expression 0) e;
      List.iter
        (fun (e, sl) ->
           Format.fprintf f "@[<1>@[<2>case@ %a:@]@,%a@]@,"
             (expression 0) e statement_list sl)
        cc;
      begin match def with
        None     -> ()
      | Some def -> Format.fprintf f "@[<1>default:@ %a@]" statement_list def
      end;
      Format.fprintf f "}@]"
  | Throw_statement e ->
      Format.fprintf f "@[<1>throw @[%a;@]@]" (expression 0) e
      (* There must be a space between the return and its
         argument. A line return would not work *)
  | Try_statement (b, ctch, fin) ->
      Format.fprintf f "@[<1>try@ %a" block b;
      begin match ctch with
        None        -> ()
      | Some (i, b) -> Format.fprintf f "@;<0 -1>@[<1>catch(%s)@,%a@]" i block b
      end;
      begin match fin with
        None   -> ()
      | Some b -> Format.fprintf f "finally@ %a" block b
      end;
      Format.fprintf f "@]"

and statement_list f b =
  match b with
    []     -> ()
  | [s]    -> statement f s
  | s :: r -> Format.fprintf f "%a@,%a" statement s statement_list r

and block f b =
  Format.fprintf f "@[<1>{%a}@]" statement_list b

and source_element f se =
  match se with
    Statement s ->
      statement f s
  | Function_declaration (i, l, b) ->
      Format.fprintf f "@[<2>function@ %s@,@[<1>(%a)@]@,@[<1>{%a}@]@]"
        i formal_parameter_list l function_body b

and source_elements f se =
  match se with
    []     -> ()
  | [s]    -> source_element f s
  | s :: r -> Format.fprintf f "%a@,%a" source_element s source_elements r

let program f se = Format.fprintf f "@[%a@]@." source_elements se
