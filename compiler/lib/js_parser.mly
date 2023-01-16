(* Js_of_ocaml compiler *)
(* Copyright (C) 2013 Hugo Heuzard *)

%{

(* Yoann Padioleau
 *
 * Copyright (C) 2010-2014 Facebook
 * Copyright (C) 2019-2022 r2c
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *)

(*************************************************************************)
(* Prelude *)
(*************************************************************************)
(* This file contains a grammar for Javascript (ES6 and more), as well
 * as partial support for Typescript.
 *
 * reference:
 *  - https://en.wikipedia.org/wiki/JavaScript_syntax
 *  - http://www.ecma-international.org/publications/standards/Ecma-262.htm
 *  - https://github.com/Microsoft/TypeScript/blob/master/doc/spec.md#A
 *
 * src: originally ocamlyacc-ified from Marcel Laverdet 'fbjs2' via Emacs
 * macros, itself extracted from the official ECMAscript specification at:
 * http://www.ecma-international.org/publications/standards/ecma-262.htm
 * back in the day (probably ES4 or ES3).
 *
 * I have heavily extended the grammar to provide the first parser for Flow.
 * I have extended it also to deal with many new Javascript features
 * (see cst_js.ml top comment).
 *
 * The grammar is close to the ECMA grammar but I've simplified a few things
 * when I could:
 *  - less intermediate grammar rules for advanced features
 *    (they are inlined in the original grammar rule)
 *  - by using my retagging-tokens technique (see parsing_hacks_js.ml)
 *    I could also get rid of some of the ugliness in the ECMA grammar
 *    that has to deal with ambiguous constructs
 *    (they conflate together expressions and arrow parameters, object
 *    values and object matching, etc.).
 *    Instead, in this grammar things are clearly separated.
 *  - I've used some macros to factorize rules, including some tricky
 *    macros to factorize expression rules.
 *)

open Js_token
open Javascript

let var pi name = ident_unsafe ~loc:(pi) name

let pi pos = (Parse_info.t_of_pos pos)

let p pos = Pi (pi pos)

let utf8_s = Stdlib.Utf8_string.of_string_exn

%}

(*************************************************************************)
(* Tokens                                                              *)
(*************************************************************************)

%token <string> T_ERROR
%token T_EOF

(*-----------------------------------------*)
(* The space/comment tokens *)
(*-----------------------------------------*)

%token <Js_token.Annot.t> TAnnot
%token <string> TComment
%token <string> TCommentLineDirective

(*-----------------------------------------*)
(* normal tokens                           *)
(*-----------------------------------------*)

(* tokens with a value *)
%token<Js_token.number_type * string> T_NUMBER
%token<Js_token.bigint_type * string> T_BIGINT
%token<Stdlib.Utf8_string.t * string> T_IDENTIFIER
%token<Stdlib.Utf8_string.t * int> T_STRING
%token<Stdlib.Utf8_string.t * string> T_REGEXP
%token<Stdlib.Utf8_string.t> T_TEMPLATE_PART

(*-----------------------------------------*)
(* Keyword tokens *)
(*-----------------------------------------*)
(* coupling: if you add an element here, expand also ident_keyword_bis
 * and also maybe the special hack for regexp in lexer_js.mll *)
%token
T_FUNCTION T_CONST T_VAR T_LET
T_IF T_ELSE
T_WHILE T_FOR T_DO T_CONTINUE T_BREAK
T_SWITCH T_CASE T_DEFAULT
T_RETURN
T_THROW T_TRY T_CATCH T_FINALLY
T_YIELD T_ASYNC T_AWAIT
T_NEW T_IN T_OF T_THIS T_SUPER T_WITH
T_NULL T_FALSE T_TRUE
T_CLASS T_INTERFACE T_EXTENDS T_IMPLEMENTS T_STATIC
T_IMPORT T_EXPORT
T_INSTANCEOF T_TYPEOF
T_DELETE T_VOID
T_ENUM
T_PUBLIC T_PRIVATE T_PROTECTED
T_PACKAGE
T_DEBUGGER
T_GET T_SET
T_FROM
T_TARGET
T_META
(*-----------------------------------------*)
(* Punctuation tokens *)
(*-----------------------------------------*)

(* Syntax *)
%token
T_LCURLY "{" T_RCURLY "}"
T_LPAREN "(" T_RPAREN ")"
T_LBRACKET "[" T_RBRACKET "]"
T_SEMICOLON ";" T_COMMA "," T_PERIOD "." T_COLON ":"
T_PLING_PERIOD
T_PLING "?"
T_ARROW
T_AT
T_ELLIPSIS "..."
T_POUND
T_PLING_PLING

(* Operators *)
%token
 T_OR T_AND
 T_BIT_OR T_BIT_XOR T_BIT_AND
 T_PLUS T_MINUS
 T_DIV T_MULT "*" T_MOD
 T_NOT T_BIT_NOT
 T_RSHIFT3_ASSIGN T_RSHIFT_ASSIGN T_LSHIFT_ASSIGN
 T_BIT_XOR_ASSIGN T_BIT_OR_ASSIGN T_BIT_AND_ASSIGN T_MOD_ASSIGN T_DIV_ASSIGN
 T_MULT_ASSIGN T_MINUS_ASSIGN T_PLUS_ASSIGN
 T_ASSIGN "="
 T_EQUAL T_NOT_EQUAL T_STRICT_EQUAL T_STRICT_NOT_EQUAL
 T_LESS_THAN_EQUAL T_GREATER_THAN_EQUAL T_LESS_THAN T_GREATER_THAN
 T_LSHIFT T_RSHIFT T_RSHIFT3
 T_INCR T_DECR
 T_EXP
 T_OR_ASSIGN T_AND_ASSIGN
 T_NULLISH_ASSIGN
 T_EXP_ASSIGN
(*-----------------------------------------*)
(* Extra tokens: *)
(*-----------------------------------------*)

%token T_VIRTUAL_SEMICOLON
%token T_LPAREN_ARROW
%token T_INCR_NB T_DECR_NB

(*-----------------------------------------*)
(* Priorities                              *)
(*-----------------------------------------*)

(* must be at the top so that it has the lowest priority *)
(* %nonassoc LOW_PRIORITY_RULE *)

(* Special if / else associativity*)
%nonassoc p_IF
%nonassoc T_ELSE

(* unused according to menhir:
%nonassoc p_POSTFIX
%right
 T_RSHIFT3_ASSIGN T_RSHIFT_ASSIGN T_LSHIFT_ASSIGN
 T_BIT_XOR_ASSIGN T_BIT_OR_ASSIGN T_BIT_AND_ASSIGN T_MOD_ASSIGN T_DIV_ASSIGN
 T_MULT_ASSIGN T_MINUS_ASSIGN T_PLUS_ASSIGN "="
*)

%left T_OR T_PLING_PLING
%left T_AND
%left T_BIT_OR
%left T_BIT_XOR
%left T_BIT_AND
%left T_EQUAL T_NOT_EQUAL T_STRICT_EQUAL T_STRICT_NOT_EQUAL
%left T_LESS_THAN_EQUAL T_GREATER_THAN_EQUAL T_LESS_THAN T_GREATER_THAN
      T_IN T_INSTANCEOF
%left T_LSHIFT T_RSHIFT T_RSHIFT3
%left T_PLUS T_MINUS
%left T_DIV T_MULT T_MOD

%right T_EXP

%right T_NOT T_BIT_NOT T_INCR T_DECR T_INCR_NB T_DECR_NB T_DELETE T_TYPEOF T_VOID T_AWAIT

(*************************************************************************)
(* Rules type decl                                                       *)
(*************************************************************************)

%start <[ `Annot of Js_token.Annot.t * Parse_info.t | `Item of Javascript.statement * Javascript.location] list > program
%start <Javascript.expression> standalone_expression

%%

(*************************************************************************)
(* Macros *)
(*************************************************************************)

listc(X):
 | X              { [$1] }
 | listc(X) "," X { $1 @ [$3] }

optl(X):
 | (* empty *) { [] }
 | X           { $1 }

(*************************************************************************)
(* Toplevel                                                            *)
(*************************************************************************)

standalone_expression:
 | e=expr T_EOF { e }

program:
 | l=module_item* T_EOF { l }

annot:
  | a=TAnnot { a, pi $symbolstartpos }

module_item:
  | item { `Item $1 }
  | annot { `Annot $1 }

(*************************************************************************)
(* statement                                                           *)
(*************************************************************************)

item:
 | stmt { $1 }
 | decl { $1 }

decl:
 | function_decl
   { Function_declaration $1, p $symbolstartpos }
 | generator_decl
   { Function_declaration $1, p $symbolstartpos }
 | async_decl
   { Function_declaration $1, p $symbolstartpos }
 | lexical_decl    { $1, p $symbolstartpos }

(*************************************************************************)
(* Variable decl *)
(*************************************************************************)

(* part of 'stmt' *)

variable_stmt:
 | T_VAR l=listc(variable_decl) sc { Variable_statement (Var, l) }

(* part of 'decl' *)
lexical_decl:
 (* es6: *)
 | T_CONST l=listc(variable_decl) sc { Variable_statement (Const, l)}
 | T_LET l=listc(variable_decl) sc { Variable_statement (Let, l)}

variable_decl:
 | i=ident e=initializer_?               { DIdent (i,e) }
 | p=binding_pattern e=initializer_   { DPattern (p, e) }

initializer_:
 | "=" e=assignment_expr { e, p $symbolstartpos }

for_variable_decl:
 | T_VAR l=listc(variable_decl_no_in)   { Var, l }
 (* es6: *)
 | T_CONST l=listc(variable_decl_no_in) { Const, l }
 | T_LET l=listc(variable_decl_no_in)   { Let, l }

variable_decl_no_in:
 | i=ident e=initializer_no_in              { DIdent (i,Some e) }
 | i=ident                                  { DIdent (i, None) }
 | p=binding_pattern e=initializer_no_in { DPattern (p, e) }

(* 'for ... in' and 'for ... of' declare only one variable *)
for_single_variable_decl:
 | T_VAR b=for_binding   { Var, b }
 (* es6: *)
 | T_CONST b=for_binding { Const, b }
 | T_LET  b=for_binding  { Let, b }

for_binding:
 | i=ident               { ForBindIdent (i) }
 | p=binding_pattern     { ForBindPattern (p) }

(*----------------------------*)
(* pattern *)
(*----------------------------*)

binding_pattern:
 | object_binding_pattern { $1 }
 | array_binding_pattern  { $1 }

object_binding_pattern:
 | "{" "}"                               { Object_binding [] }
 | "{" l=listc(binding_property) ","?  "}" { Object_binding l }

binding_property:
  | i=ident e=initializer_? { let id = match i with
                                   | S { name; _ } -> name
                                   | _ -> assert false in
                           Prop_binding (PNI id, Id i, e) }
 | pn=property_name ":" e=binding_element { Prop_binding (pn, fst e, snd e) }
 (* can appear only at the end of a binding_property_list in ECMA *)
 | "..." id=ident      { Prop_rest id }

(* in theory used also for formal parameter as is *)
binding_element:
 | i=ident         e=initializer_? { Id i, e }
 | p=binding_pattern    e=initializer_? { p, e }

(* array destructuring *)

(* TODO use elision below.
 * invent a new Hole category or maybe an array_argument special
 * type like for the (call)argument type.
 *)
array_binding_pattern:
 | "[" "]"                      { Array_binding [] }
 | "[" l=binding_element_list "]" { Array_binding l }

binding_start_element:
 | ","                  { [Elt_hole] }
 | b=binding_element ","  { [Elt_binding (fst b, snd b)] }

binding_start_list:
(* always ends in a "," *)
 | binding_start_element                     { $1 }
 | binding_start_list binding_start_element  { $1 @ $2 }

(* can't use listc() here, it's $1 not [$1] below *)
binding_element_list:
 | binding_start_list                         { $1 }
 | binding_elision_element                    { [$1] }
 | binding_start_list binding_elision_element { $1 @ [$2] }

binding_elision_element:
 | e=binding_element        { Elt_binding (fst e, snd e) }
 (* can appear only at the end of a binding_property_list in ECMA *)
 | "..." i=ident            { Elt_rest i  }

(*************************************************************************)
(* Function declarations (and exprs) *)
(*************************************************************************)

function_decl:
 | T_FUNCTION name=ident args=call_signature "{" b=function_body "}"
    { (name, {async = false; generator = false}, args, b, p $startpos($6)) }

function_expr:
 | T_FUNCTION name=ident? args=call_signature "{" b=function_body "}"
   { EFun (name, {async = false; generator = false}, args, b, p $symbolstartpos) }

call_signature: "(" args=formal_parameter_list_opt ")"
  { args }

function_body: optl(stmt_list) { $1 }

(*----------------------------*)
(* parameters *)
(*----------------------------*)

formal_parameter_list_opt:
 | (*empty*)                   { [] }
 | formal_parameter_list ","?  { List.rev $1 }

(* must be written in a left-recursive way (see conflicts.txt) *)
formal_parameter_list:
 | formal_parameter_list "," formal_parameter { $3::$1 }
 | formal_parameter                           { [$1] }

(* The ECMA and Typescript grammars imposes more restrictions
 * (some require_parameter, optional_parameter, rest_parameter)
 * but I've simplified.
 * We could also factorize with binding_element as done by ECMA.
 *)
formal_parameter:
 | id=ident                { PIdent {id; default = None} }
 (* es6: default parameter *)
 | id=ident e=initializer_   { PIdent {id; default = Some e} }
  (* until here this is mostly equivalent to the 'binding_element' rule *)
 | p=binding_pattern e=initializer_? { PPattern (p,e) }
 (* es6: spread *)
 | "..." id=ident          { PIdentSpread id }

(*************************************************************************)
(* generators                                                *)
(*************************************************************************)

generator_decl:
 | T_FUNCTION "*" name=ident args=call_signature "{" b=function_body "}"
   { (name, {async = false; generator = true}, args, b, p $symbolstartpos) }

generator_expr:
 | T_FUNCTION "*" name=ident? args=call_signature "{" b=function_body "}"
   { EFun (name, {async = false; generator = true}, args, b, p $symbolstartpos) }

(*************************************************************************)
(* asynchronous functions                                                *)
(*************************************************************************)

async_decl:
 | T_ASYNC T_FUNCTION  name=ident args=call_signature "{" b=function_body "}"
   { (name, {async = true; generator = false}, args, b, p $symbolstartpos) }

async_function_expr:
 | T_ASYNC T_FUNCTION name=ident? args=call_signature "{" b=function_body "}"
   { EFun (name, {async = true; generator = false}, args, b, p $symbolstartpos) }

(*************************************************************************)
(* Stmt *)
(*************************************************************************)
%inline
stmt: s=stmt1 { s, p $symbolstartpos }

stmt1:
 | block     { Block $1 }
 | variable_stmt   { $1 }
 | empty_stmt      { $1 }
 | expr_stmt       { $1 }
 | if_stmt         { $1 }
 | iteration_stmt  { $1 }
 | continue_stmt   { $1 }
 | break_stmt      { $1 }
 | return_stmt     { $1 }
 | labelled_stmt   { $1 }
 | switch_stmt     { $1 }
 | throw_stmt      { $1 }
 | try_stmt        { $1 }
 | debugger_stmt   { $1 }

label:
  | T_IDENTIFIER {
          let name, _raw = $1 in
          Label.of_string name }

(* Library definitions *)

block: "{" l=optl(stmt_list) "}" { l }

stmt_list: item+ { $1 }

empty_stmt:
 | T_SEMICOLON { Empty_statement }

expr_stmt:
 | expr_no_stmt sc { Expression_statement $1 }

if_stmt:
 | T_IF "(" c=expr ")" t=stmt T_ELSE e=stmt
     { If_statement (c, t, Some e) }
 | T_IF "(" c=expr ")" t=stmt %prec p_IF
     { If_statement (c, t, None) }

iteration_stmt:
 | T_DO body=stmt T_WHILE "(" condition=expr ")" sc 
    { Do_while_statement (body, condition) }
 | T_WHILE "(" condition=expr ")" body=stmt
     { While_statement (condition, body) }

 | T_FOR "(" i=expr_no_in? ";" c=expr? ";" incr=expr? ")" st=stmt
   { For_statement (Left i, c, incr, st) }
 | T_FOR "(" l=for_variable_decl ";" c=expr? ";" incr=expr? ")" st=stmt
   { For_statement (Right l, c, incr, st) }

 | T_FOR "(" left=left_hand_side_expr T_IN right=expr ")" body=stmt
   { ForIn_statement (Left left, right, body) }
 | T_FOR "(" left=for_single_variable_decl T_IN right=expr ")" body=stmt
   { ForIn_statement (Right left, right, body) }

 | T_FOR "(" left=left_hand_side_expr T_OF right=assignment_expr ")" body=stmt
   { ForOf_statement (Left left, right, body) }
 | T_FOR "(" left=for_single_variable_decl T_OF right=assignment_expr ")" body=stmt
   { ForOf_statement (Right left, right, body) }

initializer_no_in:
 | "=" e=assignment_expr_no_in { e, p $symbolstartpos }

continue_stmt:
 | T_CONTINUE l=label? sc { (Continue_statement (l)) }

break_stmt:
 | T_BREAK l=label? sc { (Break_statement (l)) }

return_stmt:
 | T_RETURN e=expr? sc { (Return_statement e) }

switch_stmt:
 | T_SWITCH "(" subject=expr ")" cb=case_block
   { let c1, d, c2 = cb in
     Switch_statement (subject, c1, d, c2)
   }

labelled_stmt:
 | l=label ":" s=stmt { Labelled_statement (l, s)}

throw_stmt:
 | T_THROW e=expr sc { (Throw_statement e) }

try_stmt:
 | T_TRY b=block c=catch { (Try_statement (b, Some c, None)) }
 | T_TRY b=block         f=finally { (Try_statement (b, None, Some f)) }
 | T_TRY b=block c=catch f=finally { (Try_statement (b, Some c, Some f)) }

catch:
 | T_CATCH "(" p=formal_parameter ")" b=block { Some p,b }
 | T_CATCH b=block { None,b }

finally:
 | T_FINALLY b=block { b }

debugger_stmt:
 | T_DEBUGGER { Debugger_statement }

(*----------------------------*)
(* auxillary stmts *)
(*----------------------------*)

case_block:
 | "{" case_clause* "}" { $2, None, [] }
 | "{" case_clause* default_clause case_clause* "}" { $2, Some $3, $4 }

case_clause:
 | T_CASE e=expr ":" s= optl(stmt_list) { e,s }

default_clause:
 | T_DEFAULT ":" list=optl(stmt_list) { list }

(*************************************************************************)
(* Exprs                                                          *)
(*************************************************************************)

expr:
 | assignment_expr { $1 }
 | e1=expr "," e2=assignment_expr { ESeq (e1, e2) }

assignment_expr:
 | conditional_expr(d1) { $1 }
 | e1=left_hand_side_expr_(d1) op=assignment_operator e2=assignment_expr
    { EBin (op, e1, e2) }
 | arrow_function { $1 }
 | T_YIELD { EYield None }
 | T_YIELD e=assignment_expr { EYield (Some e) }
 | T_YIELD "*" e=assignment_expr { EYield (Some e) }

left_hand_side_expr: left_hand_side_expr_(d1) { $1 }

(*----------------------------*)
(* Generic part (to factorize rules) *)
(*----------------------------*)

conditional_expr(x):
 | post_in_expr(x) { $1 }
  | c=post_in_expr (x) "?" a=assignment_expr ":" b=assignment_expr {
                         ECond (c, a, b)}

left_hand_side_expr_(x):
 | new_expr(x)  { $1 }
 | call_expr(x) { $1 }

post_in_expr(x):
 | pre_in_expr(x) { $1 }

 | post_in_expr(x) T_LESS_THAN post_in_expr(d1)          { EBin(Lt, $1, $3) }
 | post_in_expr(x) T_GREATER_THAN post_in_expr(d1)       { EBin(Gt, $1, $3) }
 | post_in_expr(x) T_LESS_THAN_EQUAL post_in_expr(d1)    { EBin(Le, $1, $3) }
 | post_in_expr(x) T_GREATER_THAN_EQUAL post_in_expr(d1) { EBin(Ge, $1, $3) }
 | post_in_expr(x) T_INSTANCEOF post_in_expr(d1)
    { EBin (InstanceOf, $1, $3) }

 (* also T_IN! *)
 | post_in_expr(x) T_IN post_in_expr(d1)             { EBin (In, $1, $3) }

 | post_in_expr(x) T_EQUAL post_in_expr(d1)          { EBin(EqEq, $1, $3) }
 | post_in_expr(x) T_NOT_EQUAL post_in_expr(d1)      { EBin(NotEq, $1, $3) }
 | post_in_expr(x) T_STRICT_EQUAL post_in_expr(d1)   { EBin(EqEqEq, $1, $3) }
 | post_in_expr(x) T_STRICT_NOT_EQUAL post_in_expr(d1)   { EBin(NotEqEq, $1, $3) }
 | post_in_expr(x) T_BIT_AND post_in_expr(d1)        { EBin(Band, $1, $3) }
 | post_in_expr(x) T_BIT_XOR post_in_expr(d1)        { EBin(Bxor, $1, $3) }
 | post_in_expr(x) T_BIT_OR post_in_expr(d1)         { EBin(Bor, $1, $3) }
 | post_in_expr(x) T_AND post_in_expr(d1)            { EBin(And, $1, $3) }
 | post_in_expr(x) T_OR post_in_expr(d1)             { EBin(Or, $1, $3) }
 | post_in_expr(x) T_PLING_PLING post_in_expr(d1)    { EBin(Coalesce, $1, $3) }

(* called unary_expr and update_expr in ECMA *)
pre_in_expr(x):
 | left_hand_side_expr_(x)                     { $1 }

 | pre_in_expr(x) T_INCR_NB (* %prec p_POSTFIX*)
    { EUn (IncrA, $1) }
 | pre_in_expr(x) T_DECR_NB (* %prec p_POSTFIX*)
    { EUn (DecrA, $1) }
 | T_INCR pre_in_expr(d1)
  { EUn (IncrB, $2) }
 | T_DECR pre_in_expr(d1)
  { EUn (DecrB, $2) }
 | T_INCR_NB pre_in_expr(d1)
  { EUn (IncrB, $2) }
 | T_DECR_NB pre_in_expr(d1)
  { EUn (DecrB, $2) }

 | T_DELETE pre_in_expr(d1)                    { EUn (Delete, $2) }
 | T_VOID pre_in_expr(d1)                      { EUn (Void, $2) }
  | T_TYPEOF pre_in_expr(d1)                   { EUn (Typeof, $2) }
 | T_PLUS pre_in_expr(d1)                      { EUn (Pl, $2) }
 | T_MINUS pre_in_expr(d1)                     { EUn (Neg, $2)}
 | T_BIT_NOT pre_in_expr(d1)                   { EUn (Bnot, $2) }
 | T_NOT pre_in_expr(d1)                       { EUn (Not, $2) }
 (* es7: *)
 | T_AWAIT pre_in_expr(d1)                     { EUn (Await, $2) }

 | pre_in_expr(x) "*" pre_in_expr(d1)       { EBin(Mul, $1, $3) }
 | pre_in_expr(x) T_DIV pre_in_expr(d1)     { EBin(Div, $1, $3) }
 | pre_in_expr(x) T_MOD pre_in_expr(d1)     { EBin(Mod, $1, $3) }
 | pre_in_expr(x) T_PLUS pre_in_expr(d1)    { EBin(Plus, $1, $3) }
 | pre_in_expr(x) T_MINUS pre_in_expr(d1)   { EBin(Minus, $1, $3) }
 | pre_in_expr(x) T_LSHIFT pre_in_expr(d1)  { EBin(Lsl, $1, $3) }
 | pre_in_expr(x) T_RSHIFT pre_in_expr(d1)  { EBin(Asr, $1, $3) }
 | pre_in_expr(x) T_RSHIFT3 pre_in_expr(d1) { EBin(Lsr, $1, $3) }

 (* es7: *)
 | pre_in_expr(x) T_EXP pre_in_expr(d1) { EBin(Exp, $1, $3) }

call_expr(x):
 | T_IMPORT a=arguments
     { (ECall(EVar (var (p $symbolstartpos) (Stdlib.Utf8_string.of_string_exn "import")), ANormal, a, p $symbolstartpos)) }
 | e=member_expr(x) a=arguments
     { (ECall(e, ANormal, a, p $symbolstartpos)) }
 | e=member_expr(x) T_PLING_PERIOD a=arguments
     { (ECall(e, ANullish, a, p $symbolstartpos)) }
 | e=call_expr(x) a=arguments
     { (ECall(e, ANormal, a, p $symbolstartpos)) }
 | e=call_expr(x) T_PLING_PERIOD a=arguments
     { (ECall(e, ANullish, a, p $symbolstartpos)) }
 | e=call_expr(x) "[" e2=expr "]"
     { (EAccess (e, ANormal,  e2)) }
 | e=call_expr(x) T_PLING_PERIOD "[" e2=expr "]"
     { (EAccess (e, ANullish, e2)) }
 | e=call_expr(x) a=access i=method_name
    { EDot (e,a,i) }

new_expr(x):
 | e=member_expr(x)    { e }
 | T_NEW e=new_expr(d1) { (ENew (e,None)) }

access:
  | "." { ANormal }
  | T_PLING_PERIOD { ANullish }

member_expr(x):
 | e=primary_expr(x)
     { e }
 | e1=member_expr(x) "[" e2=expr "]"
     { (EAccess (e1,ANormal, e2)) }
 | e1=member_expr(x) T_PLING_PERIOD "[" e2=expr "]"
     { (EAccess (e1,ANullish, e2)) }
 | e1=member_expr(x) ak=access i=field_name
     { (EDot(e1,ak,i)) }
 | T_NEW e1=member_expr(d1) a=arguments
     { (ENew(e1, Some a)) }

primary_expr(x):
 | e=primary_expr_no_braces
 | e=x { e }

d1: primary_with_stmt { $1 }

primary_with_stmt:
 | object_literal            { $1 }
 | function_expr       { $1 }
 (* es6: *)
 | generator_expr      { $1 }
 (* es7: *)
 | async_function_expr { $1 }


primary_expr_no_braces:
 | T_THIS                { EVar (var (p $symbolstartpos) (Stdlib.Utf8_string.of_string_exn "this")) }
 | i=ident               { EVar i }
 | n=null_literal        { n }
 | b=boolean_literal     { b }
 | n=numeric_literal     { ENum (Num.of_string_unsafe n) }
 | n=big_numeric_literal { ENum (Num.of_string_unsafe n) }
 | s=string_literal      { s }
 | r=regex_literal       { r }
 | a=array_literal       { a }
 | "(" e=expr ")"        { e }

(*----------------------------*)
(* scalar *)
(*----------------------------*)
boolean_literal:
 | T_TRUE  { (EBool true) }
 | T_FALSE { (EBool false) }

null_literal:
 | T_NULL { (EVar (var (p $symbolstartpos) (Stdlib.Utf8_string.of_string_exn "null"))) }

numeric_literal:
 | T_NUMBER { let _,f = $1 in (f) }

big_numeric_literal:
 | T_BIGINT { let _,f = $1 in (f) }

regex_literal:
 | r=T_REGEXP {
   let (Utf8 s, f) = r in
   (ERegexp (s, if String.equal f "" then None else Some f)) }

string_literal: s=T_STRING { (EStr (fst s)) }

(*----------------------------*)
(* assign *)
(*----------------------------*)

assignment_operator:
 | T_ASSIGN         { Eq }
 | T_MULT_ASSIGN    { StarEq }
 | T_EXP_ASSIGN     { ExpEq }
 | T_DIV_ASSIGN     { SlashEq }
 | T_MOD_ASSIGN     { ModEq }
 | T_PLUS_ASSIGN    { PlusEq }
 | T_MINUS_ASSIGN   { MinusEq }
 | T_LSHIFT_ASSIGN  { LslEq }
 | T_RSHIFT_ASSIGN  { AsrEq }
 | T_RSHIFT3_ASSIGN { LsrEq }
 | T_BIT_AND_ASSIGN { BandEq }
 | T_BIT_XOR_ASSIGN { BxorEq }
 | T_BIT_OR_ASSIGN  { BorEq }
 | T_AND_ASSIGN     { AndEq }
 | T_OR_ASSIGN      { OrEq }
 | T_NULLISH_ASSIGN { CoalesceEq }

(*----------------------------*)
(* array                    *)
(*----------------------------*)

array_literal:
 | "[" e=optl(elision) "]" { (EArr e) }
 | "[" l=element_list_rev last=optl(elision) "]"
     { (EArr (List.rev_append l (List.rev last))) }

element_list_rev:
 | empty=optl(elision) e=element { e::empty }
 | l=element_list_rev "," e=element { e :: l }
 | l=element_list_rev "," empty=elision e=element { e :: (List.rev_append empty l) }

element:
 | assignment_expr { Element $1 }
 (* es6: spread operator: *)
 | "..." assignment_expr { ElementSpread $2 }

(*----------------------------*)
(* object *)
(*----------------------------*)

object_literal:
 | "{" "}"                                      { EObj [] }
 | "{" listc(property_name_and_value) ","? "}"  { EObj $2 }

property_name_and_value:
 | property_name ":" assignment_expr    { Property ($1, $3) }
 (* es6: *)
 | id=id                                   { Property (PNI id, EVar (var (p $symbolstartpos) id)) }
 (* es6: spread operator: *)
 | "..." assignment_expr                { PropertySpread($2) }
 | T_GET name=id args=call_signature "{" b=function_body "}" { PropertyGet(name,args,b) }
 | T_SET name=id args=call_signature "{" b=function_body "}" { PropertySet(name,args,b) }
 | name=id args=call_signature "{" b=function_body "}" {
      PropertyMethod(name, (None, {async = false; generator = false}, args, b, p $symbolstartpos)) }
 | T_ASYNC name=id args=call_signature "{" b=function_body "}" {
      PropertyMethod(name, (None, {async = true; generator = false}, args, b, p $symbolstartpos)) }
 | "*" name=id args=call_signature "{" b=function_body "}" {
      PropertyMethod(name, (None, {async = false; generator = true}, args, b, p $symbolstartpos)) }
 | T_ASYNC "*" name=id args=call_signature "{" b=function_body "}" {
      PropertyMethod(name, (None, {async = true; generator = true}, args, b, p $symbolstartpos)) }
 | "[" p=assignment_expr "]" ":" e=assignment_expr { PropertyComputed (p,e) }
(*----------------------------*)
(* function call *)
(*----------------------------*)

arguments: "(" argument_list_opt ")" { $2 }

argument_list_opt:
 | (*empty*)   { [] }
 (* argument_list must be written in a left-recursive way(see conflicts.txt) *)
 | listc(argument) ","?  { $1  }

(* assignment_expr because expr supports sequence of exprs with ',' *)
argument:
 | assignment_expr       { Arg $1 }
 (* es6: spread operator, allowed not only in last position *)
 | "..." assignment_expr { ArgSpread $2 }

(*----------------------------*)
(* interpolated strings *)
(*----------------------------*)

(* TODO *)

(*----------------------------*)
(* arrow (short lambda) *)
(*----------------------------*)

(* TODO conflict with as then in indent_keyword_bis *)
arrow_function:
  | i=ident T_ARROW b=arrow_body {  EArrow({async = false; generator = false}, [param' i],b, p $symbolstartpos) }
  | "(" ")" T_ARROW b=arrow_body
    { EArrow ({async = false; generator = false}, [],b, p $symbolstartpos) }
  | T_LPAREN_ARROW a=formal_parameter_list_opt ")" T_ARROW b=arrow_body
    { EArrow ({async = false; generator = false}, a,b, p $symbolstartpos) }


(* was called consise body in spec *)
arrow_body:
 | b=function_body { b }
 (* see conflicts.txt for why the %prec *)
 | e=assignment_expr_no_stmt (* %prec LOW_PRIORITY_RULE *) { [(Return_statement (Some e), N)] }
 (* ugly *)
 | e=function_expr { [(Expression_statement e, N)] }

(*----------------------------*)
(* no in                    *)
(*----------------------------*)

expr_no_in:
 | assignment_expr_no_in { $1 }
 | e1=expr_no_in "," e2=assignment_expr_no_in { ESeq (e1, e2) }

assignment_expr_no_in:
 | conditional_expr_no_in { $1 }
 | e1=left_hand_side_expr_(d1) op=assignment_operator e2=assignment_expr_no_in
     { EBin(op,e1,e2) }

conditional_expr_no_in:
 | post_in_expr_no_in { $1 }
 | c=post_in_expr_no_in "?" a=assignment_expr_no_in ":" b=assignment_expr_no_in
   { ECond (c, a, b) }

post_in_expr_no_in:
 | pre_in_expr(d1) { $1 }
 | post_in_expr_no_in T_LESS_THAN post_in_expr(d1)        { EBin (Lt, $1, $3) }
 | post_in_expr_no_in T_GREATER_THAN post_in_expr(d1)     { EBin (Gt, $1, $3) }
 | post_in_expr_no_in T_LESS_THAN_EQUAL post_in_expr(d1)  { EBin (Le, $1, $3) }
 | post_in_expr_no_in T_GREATER_THAN_EQUAL post_in_expr(d1) { EBin (Ge, $1, $3) }
 | post_in_expr_no_in T_INSTANCEOF post_in_expr(d1) { EBin(InstanceOf, $1, $3) }

 (* no T_IN case *)

 | post_in_expr_no_in T_EQUAL post_in_expr(d1)         { EBin (EqEq, $1, $3) }
 | post_in_expr_no_in T_NOT_EQUAL post_in_expr(d1)     { EBin (NotEq, $1, $3) }
 | post_in_expr_no_in T_STRICT_EQUAL post_in_expr(d1)  { EBin (EqEqEq, $1, $3)}
 | post_in_expr_no_in T_STRICT_NOT_EQUAL post_in_expr(d1) { EBin (NotEqEq, $1, $3) }
 | post_in_expr_no_in T_BIT_AND post_in_expr(d1)       { EBin (Band, $1, $3)}
 | post_in_expr_no_in T_BIT_XOR post_in_expr(d1)       { EBin (Bxor, $1, $3)}
 | post_in_expr_no_in T_BIT_OR post_in_expr(d1)        { EBin (Bor, $1, $3) }
 | post_in_expr_no_in T_AND post_in_expr(d1)           { EBin (And, $1, $3) }
 | post_in_expr_no_in T_OR post_in_expr(d1)            { EBin (Or, $1, $3) }
 | post_in_expr_no_in T_PLING_PLING post_in_expr(d1)   { EBin (Coalesce, $1, $3) }

(*----------------------------*)
(* (no stmt, and no object literal like { v: 1 }) *)
(*----------------------------*)
expr_no_stmt:
 | assignment_expr_no_stmt { $1 }
 | expr_no_stmt "," assignment_expr { ESeq ($1, $3) }

(* coupling: with assignment_expr *)
assignment_expr_no_stmt:
 | conditional_expr(primary_no_stmt) { $1 }
 | e1=left_hand_side_expr_(primary_no_stmt) op=assignment_operator e2=assignment_expr
   { EBin (op,e1,e2) }
 (* es6: *)
 | arrow_function { $1 }
 (* es6: *)
 | T_YIELD { EYield None }
 | T_YIELD e=assignment_expr { EYield (Some e) }
 | T_YIELD "*" e=assignment_expr { EYield (Some e) }

(* no object_literal here *)
primary_no_stmt: T_ERROR TComment { assert false }

(*************************************************************************)
(* Entities, names *)
(*************************************************************************)
(* used for entities, parameters, labels, etc. *)
id:
 | T_IDENTIFIER { fst $1 }
  | ident_semi_keyword { utf8_s (Js_token.to_string $1) }

ident:
  | id { var (p $symbolstartpos) $1 }

(* add here keywords which are not considered reserved by ECMA *)
ident_semi_keyword:
 (* TODO: would like to add T_IMPORT here, but cause conflicts *)
 (* can have AS and ASYNC here but need to restrict arrow_function then *)
 | T_FROM
 | T_GET { T_GET }
 | T_META { T_META }
 | T_OF { T_OF }
 | T_SET { T_SET }
 | T_TARGET {T_TARGET }

  (* future reserved words in strict mode code. *)
  | T_IMPLEMENTS { T_IMPLEMENTS }
  | T_INTERFACE { T_INTERFACE }
  | T_PACKAGE { T_PACKAGE }
  | T_PRIVATE { T_PRIVATE }
  | T_PROTECTED {T_PROTECTED }
  | T_PUBLIC { T_PUBLIC }

(* alt: use the _last_non_whitespace_like_token trick and look if
 * previous token was a period to return a T_ID
 *)
ident_keyword:
 | ident_keyword_bis { utf8_s (Js_token.to_string $1) }

ident_keyword_bis:
  | T_AWAIT { T_AWAIT }
  | T_BREAK { T_BREAK }
  | T_CASE { T_CASE }
  | T_CATCH { T_CATCH }
  | T_CLASS { T_CLASS }
  | T_CONST { T_CONST }
  | T_CONTINUE { T_CONTINUE }
  | T_DEBUGGER { T_DEBUGGER }
  | T_DEFAULT { T_DEFAULT }
  | T_DELETE { T_DELETE }
  | T_DO { T_DO }
  | T_ELSE { T_ELSE }
  | T_ENUM { T_ENUM }
  | T_EXPORT { T_EXPORT }
  | T_EXTENDS { T_EXTENDS }
  | T_FALSE { T_FALSE }
  | T_FINALLY { T_FINALLY }
  | T_FOR { T_FOR }
  | T_FUNCTION { T_FUNCTION }
  | T_IF { T_IF }
  | T_IMPORT { T_IMPORT }
  | T_IN { T_IN }
  | T_INSTANCEOF { T_INSTANCEOF }
  | T_NEW { T_NEW }
  | T_NULL { T_NULL }
  | T_RETURN { T_RETURN }
  | T_SUPER { T_SUPER }
  | T_SWITCH { T_SWITCH }
  | T_THIS { T_THIS }
  | T_THROW { T_THROW }
  | T_TRUE { T_TRUE }
  | T_TRY { T_TRY }
  | T_TYPEOF { T_TYPEOF }
  | T_VAR { T_VAR }
  | T_VOID { T_VOID }
  | T_WHILE { T_WHILE }
  | T_WITH { T_WITH }
  | T_YIELD { T_YIELD }
  (* reserved words in strict mode code. *)
  | T_LET { T_LET }
  | T_STATIC { T_STATIC }

field_name:
 | id            { $1 }
 | ident_keyword { $1 }

method_name:
 | id            { $1 }
 | ident_keyword { $1 }

property_name:
 | i=id { PNI i }
 | i=ident_keyword { PNI i }
 | s=T_STRING         {
    let s, _len = s in PNS s }
 | n=numeric_literal  { PNN (Num.of_string_unsafe (n)) }
 | n=big_numeric_literal  { PNN (Num.of_string_unsafe (n)) }

(*************************************************************************)
(* Misc *)
(*************************************************************************)
sc:
 | ";"                 { $1 }
 | T_VIRTUAL_SEMICOLON { $1 }

elision:
 | ","         { [ElementHole] }
 | elision "," { $1 @ [ElementHole] }
