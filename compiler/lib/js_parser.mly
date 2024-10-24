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

let vartok pos tok =
  EVar (var (p pos) (Stdlib.Utf8_string.of_string_exn (Js_token.to_string tok)))

let utf8_s = Stdlib.Utf8_string.of_string_exn

let name_of_ident = function
  | S { name; _} -> name
  | V _ -> assert false

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
%token<string> T_ENCAPSED_STRING
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
T_AS
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
T_DOLLARCURLY
T_BACKQUOTE

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
%token T_VIRTUAL_SEMICOLON_DO_WHILE
%token T_VIRTUAL_SEMICOLON_EXPORT_DEFAULT
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

%start <(Lexing.position * (Javascript.statement * Javascript.location)) list > program
%start <Javascript.expression> standalone_expression

%%

(*************************************************************************)
(* Macros *)
(*************************************************************************)

listc_rev(X):
 | X              { [$1] }
 | listc_rev(X) "," X { $3 :: $1 }

%inline listc(X):
 | listc_rev(X) { List.rev $1 }

listc_with_empty_trail_rev(X):
 | ","                { [ None ] }
 | X ","              { [ Some $1 ] }
 | listc_with_empty_trail_rev(X) X "," { Some $2 :: $1 }
 | listc_with_empty_trail_rev(X) "," { None :: $1 }

listc_with_empty(X):
  | listc_with_empty_trail_rev(X) x=X? {
                                       match x with
                                       | None -> List.rev $1
                                       | Some _ -> List.rev (x :: $1)
                                     }
  | x=X                              { [ Some x ] }
  | (* empty *)                       { [] }

listc_with_empty2(X,Y):
  | listc_with_empty_trail_rev(X) x=X { List.rev (Some x :: $1), None }
  | listc_with_empty_trail_rev(X)     { List.rev $1, None }
  | listc_with_empty_trail_rev(X) y=Y { List.rev $1, Some y }
  | X                                 { [Some $1], None }
  | Y                                 { [], Some $1 }
  | (* empty *)                       { [], None }

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

module_item:
  | item { $symbolstartpos, $1 }
  | import_decl { $symbolstartpos, $1 }
  | export_decl { $symbolstartpos, $1 }


(*************************************************************************)
(* statement                                                           *)
(*************************************************************************)

item:
 | stmt { $1 }
 | decl { $1 }

decl:
 | function_decl
   { let i,f = $1 in Function_declaration (i,f), p $symbolstartpos }
 | generator_decl
   { let i,f = $1 in Function_declaration (i,f), p $symbolstartpos }
 | async_generator_decl
   { let i,f = $1 in Function_declaration (i,f), p $symbolstartpos }
 | async_decl
   { let i,f = $1 in Function_declaration (i,f), p $symbolstartpos }
 | lexical_decl    { $1, p $symbolstartpos }
 | class_decl
   { let i,f = $1 in Class_declaration (i,f), p $symbolstartpos }

(*************************************************************************)
(* Namespace *)
(*************************************************************************)
(*----------------------------*)
(* import *)
(*----------------------------*)

import_decl:
 | T_IMPORT kind=import_clause from=from_clause sc
    { let pos = $symbolstartpos in
      Import ({ from; kind }, pi pos), p pos }
 | T_IMPORT from=module_specifier sc
    { let pos = $symbolstartpos in
      Import ({ from; kind = SideEffect }, pi pos), p pos }

import_clause:
 | import_default                            { Default $1 }
 | import_default "," "*" T_AS id=binding_id { Namespace (Some $1, id) }
 | "*" T_AS id=binding_id                    { Namespace (None, id) }
 | import_default "," x=named_imports { Named (Some $1, x) }
 | x=named_imports                    { Named (None, x) }

import_default: binding_id { $1 }

named_imports:
 | "{" "}"                             { [] }
 | "{" listc(import_specifier) "}"     { $2 }
 | "{" listc(import_specifier) "," "}" { $2 }

(* also valid for export *)
from_clause: T_FROM module_specifier {$2 }

import_specifier:
 | binding_id                 { (name_of_ident $1, $1) }
 | string_or_ident T_AS binding_id         {
   let (_,s,_) = $1 in
   (s, $3) }

%inline string_or_ident:
 | T_STRING { `String, fst $1, $symbolstartpos }
 | id { `Ident, $1, $symbolstartpos }
 | ident_keyword { `Ident, $1, $symbolstartpos }

module_specifier:
  | T_STRING { (fst $1) }

(*----------------------------*)
(* export *)
(*----------------------------*)

export_fun_class:
 | function_expr       { $1 }
 | class_expr          { $1 }
 (* es6: *)
 | generator_expr      { $1 }
 (* es7: *)
 | async_function_expr { $1 }
 | async_generator_expr{ $1 }


export_decl:
  | T_EXPORT names=export_clause sc {
    let exception Invalid of Lexing.position in
    let k =
      try
        let names =
          List.map (fun ((k, id,pos), (_,s,_)) ->
                     match k with
                     | `Ident -> (var (p pos) id, s)
                     | `String -> raise (Invalid pos))
          names
        in
        (ExportNames names)
      with Invalid pos ->
         CoverExportFrom (early_error (pi pos))
    in
    let pos = $symbolstartpos in
    Export (k, pi pos), p pos }
 | T_EXPORT v=variable_stmt
    {
      let pos = $symbolstartpos in
      let k = match v with
        | Variable_statement (k,l) -> ExportVar (k, l)
        | _ -> assert false
      in
      Export (k, pi pos), p pos }
 | T_EXPORT d=decl
    { let k = match d with
        | Variable_statement (k,l),_ -> ExportVar (k,l)
        | Function_declaration (id, decl),_ -> ExportFun (id,decl)
        | Class_declaration (id, decl),_ -> ExportClass (id,decl)
        | _ -> assert false
      in
      let pos = $symbolstartpos in
      Export (k,pi pos), p pos }
 | T_EXPORT T_DEFAULT e=assignment_expr_no_stmt sc
    {
      let k = ExportDefaultExpression e in
      let pos = $symbolstartpos in
      Export (k,pi pos), p pos }
 | T_EXPORT T_DEFAULT e=object_literal sc
    {
      let k = ExportDefaultExpression e in
      let pos = $symbolstartpos in
      Export (k,pi pos), p pos }
 | T_EXPORT T_DEFAULT e=export_fun_class endrule(sc | T_VIRTUAL_SEMICOLON_EXPORT_DEFAULT { () } )
    {
      let k = match e with
      | EFun (id, decl) ->
         ExportDefaultFun (id,decl)
      | EClass (id, decl) ->
         ExportDefaultClass (id, decl)
      | _ -> assert false
      in
      let pos = $symbolstartpos in
      Export (k,pi pos), p pos }
| T_EXPORT "*" T_FROM from=module_specifier sc {
    let kind = Export_all None in
    let pos = $symbolstartpos in
    Export (ExportFrom ({from; kind}),pi pos), p pos
  }
 | T_EXPORT "*" T_AS id=string_or_ident T_FROM from=module_specifier sc {
    let (_,id,_) = id in
    let kind = Export_all (Some id) in
    let pos = $symbolstartpos in
    Export (ExportFrom ({from; kind}), pi pos), p pos
  }
| T_EXPORT names=export_clause T_FROM from=module_specifier sc {
    let names = List.map (fun ((_,a,_), (_,b,_)) -> a, b) names in
    let kind = Export_names names in
    let pos = $symbolstartpos in
    Export (ExportFrom ({from; kind}), pi pos), p pos
  }

export_specifier:
 | string_or_ident                       { ($1, $1) }
 | string_or_ident T_AS string_or_ident  { ($1, $3) }

export_clause:
 | "{" "}"                              { [] }
 | "{" listc(export_specifier) "}"      { $2 }
 | "{" listc(export_specifier) ","  "}" { $2 }


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
 | i=ident e=initializer_?            { DeclIdent (i,e) }
 | p=binding_pattern e=initializer_   { DeclPattern (p, e) }

initializer_:
 | "=" e=assignment_expr { e, p $symbolstartpos }

for_variable_decl:
 | T_VAR l=listc(variable_decl_no_in)   { Var, l }
 (* es6: *)
 | T_CONST l=listc(variable_decl_no_in) { Const, l }
 | T_LET l=listc(variable_decl_no_in)   { Let, l }

variable_decl_no_in:
 | i=ident e=initializer_no_in              { DeclIdent (i,Some e) }
 | i=ident                                  { DeclIdent (i, None) }
 | p=binding_pattern e=initializer_no_in { DeclPattern (p, e) }

(* 'for ... in' and 'for ... of' declare only one variable *)
for_single_variable_decl:
 | T_VAR b=for_binding   { Var, b }
 (* es6: *)
 | T_CONST b=for_binding { Const, b }
 | T_LET  b=for_binding  { Let, b }

for_binding:
 | binding               { $1 }


(*----------------------------*)
(* pattern *)
(*----------------------------*)

binding_pattern:
 | object_binding_pattern { $1 }
 | array_binding_pattern  { $1 }

binding:
 | binding_pattern { BindingPattern $1 }
 | ident           { BindingIdent $1 }

object_binding_pattern:
 | "{" "}"                                { ObjectBinding (list []) }
 | "{" r=binding_property_rest "}"         { ObjectBinding {list = []; rest = Some r } }
 | "{" l=listc(binding_property) ","? "}" { ObjectBinding (list l) }
 | "{" l=listc(binding_property) "," r=binding_property_rest "}"
    { ObjectBinding {list=l;rest= Some r} }

binding_property:
  | i=ident e=initializer_? { Prop_ident (Prop_and_ident i, e) }
  | pn=property_name ":" e=binding_element { Prop_binding (pn, e) }

binding_property_rest:
 (* can appear only at the end of a binding_property_list in ECMA *)
 | "..." id=ident      { id }

(* in theory used also for formal parameter as is *)
binding_element:
 | b=binding e=initializer_? { b, e }

(* array destructuring *)

array_binding_pattern:
  | "[" l=listc_with_empty2(binding_element, binding_element_rest) "]" {
        ArrayBinding {list = fst l; rest = snd l }
  }

binding_element_rest:
 (* can appear only at the end of a array_binding_pattern in ECMA *)
 | "..." binding            { $2 }

(*************************************************************************)
(* Function declarations (and exprs) *)
(*************************************************************************)

function_decl:
 | T_FUNCTION name=ident args=call_signature "{" b=function_body "}"
    { (name, ({async = false; generator = false}, args, b, p $startpos($6))) }

function_expr:
 | T_FUNCTION name=ident? args=call_signature "{" b=function_body "}"
   { EFun (name, ({async = false; generator = false}, args, b, p $symbolstartpos)) }

call_signature: "(" args=formal_parameter_list_opt ")"
  { args }

function_body: optl(stmt_list) { $1 }

(*----------------------------*)
(* parameters *)
(*----------------------------*)

formal_parameter_list_opt:
 | (*empty*)                                     { list [] }
 | formal_parameter_list_rev ","?                    { list (List.rev $1) }
 | r=function_rest_param                           { { list = []; rest = Some r } }
 | formal_parameter_list_rev "," r=function_rest_param { { list = List.rev $1; rest = Some r } }

function_rest_param:
 | "..." binding { $2 }

(* must be written in a left-recursive way (see conflicts.txt) *)
formal_parameter_list_rev:
 | formal_parameter_list_rev "," formal_parameter { $3::$1 }
 | formal_parameter                           { [$1] }

(* The ECMA and Typescript grammars imposes more restrictions
 * (some require_parameter, optional_parameter, rest_parameter)
 * but I've simplified.
 * We could also factorize with binding_element as done by ECMA.
 *)
formal_parameter:
  | binding initializer_? { $1, $2 }

(*************************************************************************)
(* generators                                                *)
(*************************************************************************)

generator_decl:
 | T_FUNCTION "*" name=ident args=call_signature "{" b=function_body "}"
   { (name, ({async = false; generator = true}, args, b, p $symbolstartpos)) }

generator_expr:
 | T_FUNCTION "*" name=ident? args=call_signature "{" b=function_body "}"
   { EFun (name, ({async = false; generator = true}, args, b, p $symbolstartpos)) }

(*************************************************************************)
(* asynchronous functions                                                *)
(*************************************************************************)

async_decl:
 | T_ASYNC T_FUNCTION  name=ident args=call_signature "{" b=function_body "}"
   { (name, ({async = true; generator = false}, args, b, p $symbolstartpos)) }

async_function_expr:
 | T_ASYNC T_FUNCTION name=ident? args=call_signature "{" b=function_body "}"
   { EFun (name, ({async = true; generator = false}, args, b, p $symbolstartpos)) }

(*************************************************************************)
(* async generators                                                *)
(*************************************************************************)

async_generator_decl:
 | T_ASYNC T_FUNCTION "*" name=ident args=call_signature "{" b=function_body "}"
   { (name, ({async = true; generator = true}, args, b, p $symbolstartpos)) }

async_generator_expr:
 | T_ASYNC T_FUNCTION "*" name=ident? args=call_signature "{" b=function_body "}"
   { EFun (name, ({async = true; generator = true}, args, b, p $symbolstartpos)) }

(*************************************************************************)
(* Class declaration *)
(*************************************************************************)

class_decl: T_CLASS id=binding_id extends=extends_clause? body=class_body
   { id, {extends; body}  }

class_body: "{" class_element* "}" { List.flatten $2 }

extends_clause: T_EXTENDS left_hand_side_expr { $2 }

binding_id: ident { $1 }

class_expr: T_CLASS i=binding_id? extends=extends_clause? body=class_body
   { EClass (i, {extends; body}) }

(*----------------------------*)
(* Class elements *)
(*----------------------------*)

(* can't factorize with static_opt, or access_modifier_opt; ambiguities  *)
class_element:
 |          m=method_definition(class_property_name)
    { let n,m = m in [ CEMethod (false, n, m) ] }
 | T_STATIC m=method_definition(class_property_name)
    { let n,m = m in [ CEMethod (true, n, m) ] }

 |          n=class_property_name i=initializer_? sc
    { [ CEField (false, n, i) ] }
 | T_STATIC n=class_property_name i=initializer_? sc
    { [ CEField (true, n, i) ] }
 | T_STATIC b=block { [CEStaticBLock b] }
 | sc    { [] }

class_property_name:
  | property_name { PropName $1 }
  | T_POUND id { PrivName $2 }

method_definition(name):
 | T_GET name=name args=call_signature "{" b=function_body "}" { name, MethodGet(({async = false; generator = false}, args, b, p $symbolstartpos)) }
 | T_SET name=name args=call_signature "{" b=function_body "}" { name, MethodSet(({async = false; generator = false}, args, b, p $symbolstartpos)) }
 | name=name args=call_signature "{" b=function_body "}" {
      name, Method(({async = false; generator = false}, args, b, p $symbolstartpos)) }
 | T_ASYNC name=name args=call_signature "{" b=function_body "}" {
      name, Method(({async = true; generator = false}, args, b, p $symbolstartpos)) }
 | "*" name=name args=call_signature "{" b=function_body "}" {
      name, Method(({async = false; generator = true}, args, b, p $symbolstartpos)) }
 | T_ASYNC "*" name=name args=call_signature "{" b=function_body "}" {
      name, Method(({async = true; generator = true}, args, b, p $symbolstartpos)) }

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
 | with_stmt       { $1 }
 | debugger_stmt   { $1 }

label:
  | id { Label.of_string $1 }

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
 | T_DO body=stmt T_WHILE "(" condition=expr ")" endrule(sc | T_VIRTUAL_SEMICOLON_DO_WHILE { () } )
    { Do_while_statement (body, condition) }
 | T_WHILE "(" condition=expr ")" body=stmt
     { While_statement (condition, body) }

 | T_FOR "(" i=expr_no_in? ";" c=expr? ";" incr=expr? ")" st=stmt
   { For_statement (Left i, c, incr, st) }
 | T_FOR "(" l=for_variable_decl ";" c=expr? ";" incr=expr? ")" st=stmt
   { For_statement (Right l, c, incr, st) }

 | T_FOR "(" left=left_hand_side_expr T_IN right=expr ")" body=stmt
   { let left = assignment_target_of_expr None left in
     ForIn_statement (Left left, right, body) }
 | T_FOR "(" left=for_single_variable_decl T_IN right=expr ")" body=stmt
   { ForIn_statement (Right left, right, body) }

 | T_FOR "(" left=left_hand_side_expr T_OF right=assignment_expr ")" body=stmt
    { let left = assignment_target_of_expr None left in
      ForOf_statement (Left left, right, body) }
 | T_FOR "(" left=for_single_variable_decl T_OF right=assignment_expr ")" body=stmt
   { ForOf_statement (Right left, right, body) }
 | T_FOR T_AWAIT "(" left=left_hand_side_expr T_OF right=assignment_expr ")" body=stmt
    { let left = assignment_target_of_expr None left in
      ForAwaitOf_statement (Left left, right, body) }
 | T_FOR T_AWAIT "(" left=for_single_variable_decl T_OF right=assignment_expr ")" body=stmt
   { ForAwaitOf_statement (Right left, right, body) }

initializer_no_in:
 | "=" e=assignment_expr_no_in { e, p $symbolstartpos }

continue_stmt:
 | T_CONTINUE l=label? sc { (Continue_statement (l)) }

break_stmt:
 | T_BREAK l=label? sc { (Break_statement (l)) }

return_stmt:
 | T_RETURN e=expr? sc { (Return_statement (e, p $endpos(e))) }

switch_stmt:
 | T_SWITCH "(" subject=expr ")" cb=case_block
   { let c1, d, c2 = cb in
     Switch_statement (subject, c1, d, c2)
   }

labelled_stmt:
 | l=label ":" s=stmt { Labelled_statement (l, s)}

throw_stmt:
 | T_THROW e=expr sc { (Throw_statement e) }

with_stmt:
 | T_WITH "(" e=expr ")" s=stmt { (With_statement (e,s)) }

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
 | T_DEBUGGER sc { Debugger_statement }

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
    {
      let e1 = assignment_target_of_expr (Some op) e1 in
      EBin (op, e1, e2)
    }
 | arrow_function { $1 }
 | async_arrow_function { $1 }
 | T_YIELD { EYield { delegate= false; expr = None } }
 | T_YIELD e=assignment_expr { EYield {delegate=false; expr = (Some e) } }
 | T_YIELD "*" e=assignment_expr { EYield {delegate=true; expr = (Some e) } }

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
     { (ECall(vartok $startpos($1) T_IMPORT, ANormal, a, p $symbolstartpos)) }
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
 | e=call_expr(x) t=template_literal
    { ECallTemplate(e, t,p $symbolstartpos) }
 | T_SUPER a=arguments { ECall(vartok $startpos($1) T_SUPER,ANormal, a, p $symbolstartpos) }
 | e=call_expr(x) a=access i=method_name
    { EDot (e,a,i) }
 | e=call_expr(x) a=access T_POUND i=method_name
    { EDotPrivate (e,a,i) }

new_expr(x):
 | e=member_expr(x)    { e }
 | T_NEW e=new_expr(d1) { (ENew (e,None, p $symbolstartpos)) }

access:
  | "." { ANormal }
  | T_PLING_PERIOD { ANullish }

member_expr(x):
 | e=primary_expr(x)
    { e }
 | T_IMPORT "." T_META
    { EDot (vartok $startpos($1) T_IMPORT,ANormal,(Stdlib.Utf8_string.of_string_exn "meta")) }
 | e1=member_expr(x) "[" e2=expr "]"
     { (EAccess (e1,ANormal, e2)) }
 | e1=member_expr(x) T_PLING_PERIOD "[" e2=expr "]"
     { (EAccess (e1,ANullish, e2)) }
 | e1=member_expr(x) ak=access i=field_name
     { (EDot(e1,ak,i)) }
 | T_NEW e1=member_expr(d1) a=arguments
     { (ENew(e1, Some a, p $symbolstartpos)) }
 | e=member_expr(x) t=template_literal
     { ECallTemplate(e, t, p $symbolstartpos) }
 | T_SUPER "[" e=expr "]"
      { (EAccess (vartok $startpos($1) T_SUPER,ANormal, e)) }
 | T_SUPER ak=access i=field_name
     { (EDot(vartok $startpos($1) T_SUPER,ak,i)) }
  | T_NEW "." T_TARGET
     { (EDot(vartok $startpos($1) T_NEW,ANormal,Stdlib.Utf8_string.of_string_exn "target")) }
  | e1=member_expr(x) a=access T_POUND i=field_name
    { (EDotPrivate(e1,a,i)) }
primary_expr(x):
 | e=primary_expr_no_braces
 | e=x { e }

d1: primary_with_stmt { $1 }

primary_with_stmt:
 | object_literal      { $1 }
 | function_expr       { $1 }
 | class_expr          { $1 }
 (* es6: *)
 | generator_expr      { $1 }
 (* es7: *)
 | async_function_expr { $1 }
 | async_generator_expr{ $1 }


primary_expr_no_braces:
 | T_THIS                { EVar (var (p $symbolstartpos) (Stdlib.Utf8_string.of_string_exn "this")) }
 | i=ident               { EVar i }
 | T_POUND id            { EPrivName $2 }
 | n=null_literal        { n }
 | b=boolean_literal     { b }
 | n=numeric_literal     { ENum (Num.of_string_unsafe n) }
 | n=big_numeric_literal { ENum (Num.of_string_unsafe n) }
 | s=string_literal      { s }
 | t=template_literal    { ETemplate t }
 | r=regex_literal       { r }
 | a=array_literal       { a }
 | e=coverParenthesizedExpressionAndArrowParameterList { e }

coverParenthesizedExpressionAndArrowParameterList:
 | "(" e=expr ","? ")" { e }
 | "(" ")" { CoverParenthesizedExpressionAndArrowParameterList (early_error (pi $startpos($2))) }
 | "(" "..." binding ")" { CoverParenthesizedExpressionAndArrowParameterList (early_error (pi $startpos($2)) ) }
 | "(" expr "," "..." binding ")" { CoverParenthesizedExpressionAndArrowParameterList (early_error (pi $startpos($4)) ) }

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
  | "[" l=listc_with_empty (element) "]"
    { (EArr (List.map (function None -> ElementHole | Some x -> x) l)) }

element:
 | assignment_expr       { Element $1 }
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
 | id=id          { Property (PNI id, EVar (ident_unsafe id)) }
 | ident initializer_  { CoverInitializedName (early_error (pi $startpos($2)), $1, $2)  }
 (* es6: spread operator: *)
 | "..." assignment_expr                { PropertySpread($2) }
  | method_definition(property_name)
    { let n, m = $1 in PropertyMethod(n,m) }
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

(* templated string (a.k.a interpolated strings) *)
template_literal: T_BACKQUOTE encaps* T_BACKQUOTE  { $2 }

encaps:
 | T_ENCAPSED_STRING        { TStr (Stdlib.Utf8_string.of_string_exn $1) }
 | T_DOLLARCURLY expr "}"   { TExp $2 }

(*----------------------------*)
(* arrow (short lambda) *)
(*----------------------------*)

(* TODO conflict with as then in indent_keyword_bis *)
arrow_function:
  | i=ident T_ARROW b=arrow_body
    { let b,consise = b in
      EArrow (({async = false; generator = false}, list [param' i],b, p $symbolstartpos), consise, AUnknown) }
  | T_LPAREN_ARROW a=formal_parameter_list_opt ")" T_ARROW b=arrow_body
    { let b,consise = b in
      EArrow (({async = false; generator = false}, a,b, p $symbolstartpos), consise, AUnknown) }

async_arrow_function:
  | T_ASYNC i=ident T_ARROW b=arrow_body {
      let b,consise = b in
      EArrow(({async = true; generator = false}, list [param' i],b, p $symbolstartpos), consise, AUnknown) }
  | T_ASYNC T_LPAREN_ARROW a=formal_parameter_list_opt ")" T_ARROW b=arrow_body
    { let b,consise = b in
      EArrow (({async = true; generator = false}, a,b, p $symbolstartpos), consise, AUnknown) }


(* was called consise body in spec *)
arrow_body:
 | "{" b=function_body "}" { b, false }
 | e=assignment_expr_for_consise_body { [(Return_statement (Some e, p $endpos), p $symbolstartpos)], true }

(*----------------------------*)
(* no in                    *)
(*----------------------------*)

expr_no_in:
 | assignment_expr_no_in { $1 }
 | e1=expr_no_in "," e2=assignment_expr_no_in { ESeq (e1, e2) }

assignment_expr_no_in:
 | conditional_expr_no_in { $1 }
 | e1=left_hand_side_expr_(d1) op=assignment_operator e2=assignment_expr_no_in
    {
      let e1 = assignment_target_of_expr (Some op) e1 in
      EBin (op, e1, e2)
    }

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
    {
      let e1 = assignment_target_of_expr (Some op) e1 in
      EBin (op, e1, e2)
    }
 (* es6: *)
 | arrow_function { $1 }
 | async_arrow_function { $1 }
 (* es6: *)
 | T_YIELD { EYield {delegate = false; expr = None} }
 | T_YIELD e=assignment_expr { EYield {delegate = false; expr = Some e } }
 | T_YIELD "*" e=assignment_expr { EYield { delegate = true; expr = (Some e) } }


primary_for_consise_body:
 | function_expr       { $1 }
 | class_expr          { $1 }
 (* es6: *)
 | generator_expr      { $1 }
 (* es7: *)
 | async_function_expr { $1 }
 | async_generator_expr{ $1 }

assignment_expr_for_consise_body:
 | conditional_expr(primary_for_consise_body) { $1 }
 | e1=left_hand_side_expr_(primary_for_consise_body) op=assignment_operator e2=assignment_expr
    {
      let e1 = assignment_target_of_expr (Some op) e1 in
      EBin (op, e1, e2)
    }
 (* es6: *)
 | arrow_function { $1 }
 | async_arrow_function { $1 }
 (* es6: *)
 | T_YIELD { EYield { delegate = false; expr = None } }
 | T_YIELD e=assignment_expr { EYield {delegate = false; expr = (Some e) } }
 | T_YIELD "*" e=assignment_expr { EYield {delegate = true; expr = (Some e) } }

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
 | T_AS { T_AS }
 | T_ASYNC { T_ASYNC }
 | T_FROM { T_FROM }
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
 | "[" p=assignment_expr "]" { PComputed p }
(*************************************************************************)
(* Misc *)
(*************************************************************************)
sc:
 | ";"                 { $1 }
 | T_VIRTUAL_SEMICOLON { $1 }
