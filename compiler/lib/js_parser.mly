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
 *
 * The grammar rules are organized to follow the structure of the
 * ECMAScript specification (ECMA-262):
 *  - Section 12: Lexical Grammar (identifiers, literals)
 *  - Section 13: Expressions
 *  - Section 14: Statements and Declarations
 *  - Section 15: Functions and Classes
 *  - Section 16: Scripts and Modules
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
(* coupling: if you add an element here, expand also identifierKeyword
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
(* Section 12: ECMAScript Language: Lexical Grammar                     *)
(*************************************************************************)

(*----------------------------*)
(* 12.6 Names and Keywords *)
(*----------------------------*)

(* IdentifierName - used for entities, parameters, labels, etc. *)
identifierName:
 | T_IDENTIFIER { fst $1 }
 | identifierSemiKeyword { utf8_s (Js_token.to_string $1) }

identifier:
 | identifierName { var (p $symbolstartpos) $1 }

(* add here keywords which are not considered reserved by ECMA *)
identifierSemiKeyword:
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
identifierKeyword:
 | identifierKeywordToken { utf8_s (Js_token.to_string $1) }

identifierKeywordToken:
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

fieldName:
 | identifierName    { $1 }
 | identifierKeyword { $1 }

methodName:
 | identifierName    { $1 }
 | identifierKeyword { $1 }

propertyName:
 | i=identifierName { PNI i }
 | i=identifierKeyword { PNI i }
 | s=T_STRING         {
    let s, _len = s in PNS s }
 | n=numericLiteral  { PNN (Num.of_string_unsafe (n)) }
 | n=bigIntLiteral  { PNN (Num.of_string_unsafe (n)) }
 | "[" p=assignmentExpression "]" { PComputed p }

labelIdentifier:
  | identifierName { Label.of_string $1 }

(*----------------------------*)
(* 12.8.1 Null Literals *)
(*----------------------------*)

nullLiteral:
 | T_NULL { (EVar (var (p $symbolstartpos) (Stdlib.Utf8_string.of_string_exn "null"))) }

(*----------------------------*)
(* 12.8.2 Boolean Literals *)
(*----------------------------*)

booleanLiteral:
 | T_TRUE  { (EBool true) }
 | T_FALSE { (EBool false) }

(*----------------------------*)
(* 12.8.3 Numeric Literals *)
(*----------------------------*)

numericLiteral:
 | T_NUMBER { let _,f = $1 in (f) }

bigIntLiteral:
 | T_BIGINT { let _,f = $1 in (f) }

(*----------------------------*)
(* 12.8.4 String Literals *)
(*----------------------------*)

stringLiteral: s=T_STRING { (EStr (fst s)) }

(*----------------------------*)
(* 12.8.5 Regular Expression Literals *)
(*----------------------------*)

regularExpressionLiteral:
 | r=T_REGEXP {
   let (Utf8 s, f) = r in
   (ERegexp (s, if String.equal f "" then None else Some f)) }

(*----------------------------*)
(* 12.8.6 Template Literal Lexical Components *)
(*----------------------------*)

templateLiteral: T_BACKQUOTE templateSpan* T_BACKQUOTE  { $2 }

templateSpan:
 | T_ENCAPSED_STRING        { TStr (Stdlib.Utf8_string.of_string_exn $1) }
 | T_DOLLARCURLY expression "}"   { TExp $2 }

(*************************************************************************)
(* Section 13: ECMAScript Language: Expressions                         *)
(*************************************************************************)

(*----------------------------*)
(* 13.1 Identifiers *)
(*----------------------------*)

bindingIdentifier: identifier { $1 }

(*----------------------------*)
(* 13.2 Primary Expression *)
(*----------------------------*)

primaryExpression(x):
 | e=primaryExpressionNoBraces
 | e=x { e }

d1: primaryExpressionWithStmt { $1 }

primaryExpressionWithStmt:
 | objectLiteral           { $1 }
 | functionExpression      { $1 }
 | classExpression         { $1 }
 (* es6: *)
 | generatorExpression     { $1 }
 (* es7: *)
 | asyncFunctionExpression { $1 }
 | asyncGeneratorExpression{ $1 }

primaryExpressionNoBraces:
 | T_THIS                { EVar (var (p $symbolstartpos) (Stdlib.Utf8_string.of_string_exn "this")) }
 | i=identifier          { EVar i }
 | T_POUND identifierName { EPrivName $2 }
 | n=nullLiteral         { n }
 | b=booleanLiteral      { b }
 | n=numericLiteral      { ENum (Num.of_string_unsafe n) }
 | n=bigIntLiteral       { ENum (Num.of_string_unsafe n) }
 | s=stringLiteral       { s }
 | t=templateLiteral     { ETemplate t }
 | r=regularExpressionLiteral { r }
 | a=arrayLiteral        { a }
 | e=coverParenthesizedExpressionAndArrowParameterList { e }

coverParenthesizedExpressionAndArrowParameterList:
 | "(" e=expression ","? ")" { e }
 | "(" ")" { CoverParenthesizedExpressionAndArrowParameterList (early_error (pi $startpos($2))) }
 | "(" "..." bindingElement ")" { CoverParenthesizedExpressionAndArrowParameterList (early_error (pi $startpos($2)) ) }
 | "(" expression "," "..." bindingElement ")" { CoverParenthesizedExpressionAndArrowParameterList (early_error (pi $startpos($4)) ) }

(* no objectLiteral here *)
primaryExpressionNoStmt: T_ERROR TComment { assert false }

(*----------------------------*)
(* 13.2.4 Array Initializer *)
(*----------------------------*)

arrayLiteral:
  | "[" l=listc_with_empty (arrayElement) "]"
    { (EArr (List.map (function None -> ElementHole | Some x -> x) l)) }

arrayElement:
 | assignmentExpression       { Element $1 }
 (* es6: SpreadElement *)
 | "..." assignmentExpression { ElementSpread $2 }

(*----------------------------*)
(* 13.2.5 Object Initializer *)
(*----------------------------*)

objectLiteral:
 | "{" "}"                                      { EObj [] }
 | "{" listc(propertyDefinition) ","? "}"       { EObj $2 }

propertyDefinition:
 | propertyName ":" assignmentExpression    { Property ($1, $3) }
 (* es6: shorthand property *)
 | i=identifierName          { Property (PNI i, EVar (ident_unsafe i)) }
 | identifier initializer_  { CoverInitializedName (early_error (pi $startpos($2)), $1, $2)  }
 (* es6: spread property *)
 | "..." assignmentExpression                { PropertySpread($2) }
  | methodDefinition(propertyName)
    { let n, m = $1 in PropertyMethod(n,m) }

(*----------------------------*)
(* 13.3 Left-Hand-Side Expressions *)
(*----------------------------*)

leftHandSideExpression: leftHandSideExpression_(d1) { $1 }

leftHandSideExpression_(x):
 | newExpression(x)  { $1 }
 | callExpression(x) { $1 }

(*----------------------------*)
(* 13.3.1 Static Semantics (Property Accessors) *)
(*----------------------------*)

optionalChainingPunctuator:
  | "." { ANormal }
  | T_PLING_PERIOD { ANullish }

(*----------------------------*)
(* 13.3.2 Property Accessors *)
(*----------------------------*)

memberExpression(x):
 | e=primaryExpression(x)
    { e }
 | T_IMPORT "." T_META
    { EDot (vartok $startpos($1) T_IMPORT,ANormal,(Stdlib.Utf8_string.of_string_exn "meta")) }
 | e1=memberExpression(x) "[" e2=expression "]"
     { (EAccess (e1,ANormal, e2)) }
 | e1=memberExpression(x) T_PLING_PERIOD "[" e2=expression "]"
     { (EAccess (e1,ANullish, e2)) }
 | e1=memberExpression(x) ak=optionalChainingPunctuator i=fieldName
     { (EDot(e1,ak,i)) }
 | T_NEW e1=memberExpression(d1) a=arguments
     { (ENew(e1, Some a, p $symbolstartpos)) }
 | e=memberExpression(x) t=templateLiteral
     { ECallTemplate(e, t, p $symbolstartpos) }
 | T_SUPER "[" e=expression "]"
      { (EAccess (vartok $startpos($1) T_SUPER,ANormal, e)) }
 | T_SUPER ak=optionalChainingPunctuator i=fieldName
     { (EDot(vartok $startpos($1) T_SUPER,ak,i)) }
  | T_NEW "." T_TARGET
     { (EDot(vartok $startpos($1) T_NEW,ANormal,Stdlib.Utf8_string.of_string_exn "target")) }
  | e1=memberExpression(x) a=optionalChainingPunctuator T_POUND i=fieldName
    { (EDotPrivate(e1,a,i)) }

(*----------------------------*)
(* 13.3.3 The new Operator *)
(*----------------------------*)

newExpression(x):
 | e=memberExpression(x)    { e }
 | T_NEW e=newExpression(d1) { (ENew (e,None, p $symbolstartpos)) }

(*----------------------------*)
(* 13.3.4 Function Calls *)
(*----------------------------*)

callExpression(x):
 | T_IMPORT a=arguments
     { (ECall(vartok $startpos($1) T_IMPORT, ANormal, a, p $symbolstartpos)) }
 | e=memberExpression(x) a=arguments
     { (ECall(e, ANormal, a, p $symbolstartpos)) }
 | e=memberExpression(x) T_PLING_PERIOD a=arguments
     { (ECall(e, ANullish, a, p $symbolstartpos)) }
 | e=callExpression(x) a=arguments
     { (ECall(e, ANormal, a, p $symbolstartpos)) }
 | e=callExpression(x) T_PLING_PERIOD a=arguments
     { (ECall(e, ANullish, a, p $symbolstartpos)) }
 | e=callExpression(x) "[" e2=expression "]"
     { (EAccess (e, ANormal,  e2)) }
 | e=callExpression(x) T_PLING_PERIOD "[" e2=expression "]"
    { (EAccess (e, ANullish, e2)) }
 | e=callExpression(x) t=templateLiteral
    { ECallTemplate(e, t,p $symbolstartpos) }
 | T_SUPER a=arguments { ECall(vartok $startpos($1) T_SUPER,ANormal, a, p $symbolstartpos) }
 | e=callExpression(x) a=optionalChainingPunctuator i=methodName
    { EDot (e,a,i) }
 | e=callExpression(x) a=optionalChainingPunctuator T_POUND i=methodName
    { EDotPrivate (e,a,i) }

(*----------------------------*)
(* 13.3.5 Argument Lists *)
(*----------------------------*)

arguments: "(" argumentList ")" { $2 }

argumentList:
 | (*empty*)   { [] }
 (* argumentList must be written in a left-recursive way(see conflicts.txt) *)
 | listc(argumentListElement) ","?  { $1  }

(* assignmentExpression because expression supports sequence of exprs with ',' *)
argumentListElement:
 | assignmentExpression       { Arg $1 }
 (* es6: spread element, allowed not only in last position *)
 | "..." assignmentExpression { ArgSpread $2 }

(*----------------------------*)
(* 13.4 Update Expressions *)
(*----------------------------*)

(* called UnaryExpression and UpdateExpression in ECMA *)
unaryExpression(x):
 | leftHandSideExpression_(x)                     { $1 }

 | unaryExpression(x) T_INCR_NB (* %prec p_POSTFIX*)
    { EUn (IncrA, $1) }
 | unaryExpression(x) T_DECR_NB (* %prec p_POSTFIX*)
    { EUn (DecrA, $1) }
 | T_INCR unaryExpression(d1)
  { EUn (IncrB, $2) }
 | T_DECR unaryExpression(d1)
  { EUn (DecrB, $2) }
 | T_INCR_NB unaryExpression(d1)
  { EUn (IncrB, $2) }
 | T_DECR_NB unaryExpression(d1)
  { EUn (DecrB, $2) }

(*----------------------------*)
(* 13.5 Unary Operators *)
(*----------------------------*)

 | T_DELETE unaryExpression(d1)                    { EUn (Delete, $2) }
 | T_VOID unaryExpression(d1)                      { EUn (Void, $2) }
  | T_TYPEOF unaryExpression(d1)                   { EUn (Typeof, $2) }
 | T_PLUS unaryExpression(d1)                      { EUn (Pl, $2) }
 | T_MINUS unaryExpression(d1)                     { EUn (Neg, $2)}
 | T_BIT_NOT unaryExpression(d1)                   { EUn (Bnot, $2) }
 | T_NOT unaryExpression(d1)                       { EUn (Not, $2) }
 (* es7: *)
 | T_AWAIT unaryExpression(d1)                     { EUn (Await, $2) }

(*----------------------------*)
(* 13.6 Exponentiation Operator *)
(*----------------------------*)

 (* es7: *)
 | unaryExpression(x) T_EXP unaryExpression(d1) { EBin(Exp, $1, $3) }

(*----------------------------*)
(* 13.7 Multiplicative Operators *)
(*----------------------------*)

 | unaryExpression(x) "*" unaryExpression(d1)       { EBin(Mul, $1, $3) }
 | unaryExpression(x) T_DIV unaryExpression(d1)     { EBin(Div, $1, $3) }
 | unaryExpression(x) T_MOD unaryExpression(d1)     { EBin(Mod, $1, $3) }

(*----------------------------*)
(* 13.8 Additive Operators *)
(*----------------------------*)

 | unaryExpression(x) T_PLUS unaryExpression(d1)    { EBin(Plus, $1, $3) }
 | unaryExpression(x) T_MINUS unaryExpression(d1)   { EBin(Minus, $1, $3) }

(*----------------------------*)
(* 13.9 Bitwise Shift Operators *)
(*----------------------------*)

 | unaryExpression(x) T_LSHIFT unaryExpression(d1)  { EBin(Lsl, $1, $3) }
 | unaryExpression(x) T_RSHIFT unaryExpression(d1)  { EBin(Asr, $1, $3) }
 | unaryExpression(x) T_RSHIFT3 unaryExpression(d1) { EBin(Lsr, $1, $3) }

(*----------------------------*)
(* 13.10 Relational Operators *)
(*----------------------------*)

relationalExpression(x):
 | unaryExpression(x) { $1 }

 | relationalExpression(x) T_LESS_THAN relationalExpression(d1)          { EBin(Lt, $1, $3) }
 | relationalExpression(x) T_GREATER_THAN relationalExpression(d1)       { EBin(Gt, $1, $3) }
 | relationalExpression(x) T_LESS_THAN_EQUAL relationalExpression(d1)    { EBin(Le, $1, $3) }
 | relationalExpression(x) T_GREATER_THAN_EQUAL relationalExpression(d1) { EBin(Ge, $1, $3) }
 | relationalExpression(x) T_INSTANCEOF relationalExpression(d1)
    { EBin (InstanceOf, $1, $3) }

 (* also T_IN! *)
 | relationalExpression(x) T_IN relationalExpression(d1)             { EBin (In, $1, $3) }

(*----------------------------*)
(* 13.11 Equality Operators *)
(*----------------------------*)

 | relationalExpression(x) T_EQUAL relationalExpression(d1)          { EBin(EqEq, $1, $3) }
 | relationalExpression(x) T_NOT_EQUAL relationalExpression(d1)      { EBin(NotEq, $1, $3) }
 | relationalExpression(x) T_STRICT_EQUAL relationalExpression(d1)   { EBin(EqEqEq, $1, $3) }
 | relationalExpression(x) T_STRICT_NOT_EQUAL relationalExpression(d1)   { EBin(NotEqEq, $1, $3) }

(*----------------------------*)
(* 13.12 Binary Bitwise Operators *)
(*----------------------------*)

 | relationalExpression(x) T_BIT_AND relationalExpression(d1)        { EBin(Band, $1, $3) }
 | relationalExpression(x) T_BIT_XOR relationalExpression(d1)        { EBin(Bxor, $1, $3) }
 | relationalExpression(x) T_BIT_OR relationalExpression(d1)         { EBin(Bor, $1, $3) }

(*----------------------------*)
(* 13.13 Binary Logical Operators *)
(*----------------------------*)

 | relationalExpression(x) T_AND relationalExpression(d1)            { EBin(And, $1, $3) }
 | relationalExpression(x) T_OR relationalExpression(d1)             { EBin(Or, $1, $3) }
 | relationalExpression(x) T_PLING_PLING relationalExpression(d1)    { EBin(Coalesce, $1, $3) }

(*----------------------------*)
(* 13.14 Conditional Operator ( ? : ) *)
(*----------------------------*)

conditionalExpression(x):
 | relationalExpression(x) { $1 }
  | c=relationalExpression (x) "?" a=assignmentExpression ":" b=assignmentExpression {
                         ECond (c, a, b)}

(*----------------------------*)
(* 13.15 Assignment Operators *)
(*----------------------------*)

assignmentExpression:
 | conditionalExpression(d1) { $1 }
 | e1=leftHandSideExpression_(d1) op=assignmentOperator e2=assignmentExpression
    {
      let e1 = assignment_target_of_expr (Some op) e1 in
      EBin (op, e1, e2)
    }
 | arrowFunction { $1 }
 | asyncArrowFunction { $1 }
 | T_YIELD { EYield { delegate= false; expr = None } }
 | T_YIELD e=assignmentExpression { EYield {delegate=false; expr = (Some e) } }
 | T_YIELD "*" e=assignmentExpression { EYield {delegate=true; expr = (Some e) } }

assignmentOperator:
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
(* 13.16 Comma Operator ( , ) *)
(*----------------------------*)

expression:
 | assignmentExpression { $1 }
 | e1=expression "," e2=assignmentExpression { ESeq (e1, e2) }

(*----------------------------*)
(* Expression variants (no 'in' allowed) *)
(*----------------------------*)

expressionNoIn:
 | assignmentExpressionNoIn { $1 }
 | e1=expressionNoIn "," e2=assignmentExpressionNoIn { ESeq (e1, e2) }

assignmentExpressionNoIn:
 | conditionalExpressionNoIn { $1 }
 | e1=leftHandSideExpression_(d1) op=assignmentOperator e2=assignmentExpressionNoIn
    {
      let e1 = assignment_target_of_expr (Some op) e1 in
      EBin (op, e1, e2)
    }

conditionalExpressionNoIn:
 | relationalExpressionNoIn { $1 }
 | c=relationalExpressionNoIn "?" a=assignmentExpressionNoIn ":" b=assignmentExpressionNoIn
   { ECond (c, a, b) }

relationalExpressionNoIn:
 | unaryExpression(d1) { $1 }
 | relationalExpressionNoIn T_LESS_THAN relationalExpression(d1)        { EBin (Lt, $1, $3) }
 | relationalExpressionNoIn T_GREATER_THAN relationalExpression(d1)     { EBin (Gt, $1, $3) }
 | relationalExpressionNoIn T_LESS_THAN_EQUAL relationalExpression(d1)  { EBin (Le, $1, $3) }
 | relationalExpressionNoIn T_GREATER_THAN_EQUAL relationalExpression(d1) { EBin (Ge, $1, $3) }
 | relationalExpressionNoIn T_INSTANCEOF relationalExpression(d1) { EBin(InstanceOf, $1, $3) }

 (* no T_IN case *)

 | relationalExpressionNoIn T_EQUAL relationalExpression(d1)         { EBin (EqEq, $1, $3) }
 | relationalExpressionNoIn T_NOT_EQUAL relationalExpression(d1)     { EBin (NotEq, $1, $3) }
 | relationalExpressionNoIn T_STRICT_EQUAL relationalExpression(d1)  { EBin (EqEqEq, $1, $3)}
 | relationalExpressionNoIn T_STRICT_NOT_EQUAL relationalExpression(d1) { EBin (NotEqEq, $1, $3) }
 | relationalExpressionNoIn T_BIT_AND relationalExpression(d1)       { EBin (Band, $1, $3)}
 | relationalExpressionNoIn T_BIT_XOR relationalExpression(d1)       { EBin (Bxor, $1, $3)}
 | relationalExpressionNoIn T_BIT_OR relationalExpression(d1)        { EBin (Bor, $1, $3) }
 | relationalExpressionNoIn T_AND relationalExpression(d1)           { EBin (And, $1, $3) }
 | relationalExpressionNoIn T_OR relationalExpression(d1)            { EBin (Or, $1, $3) }
 | relationalExpressionNoIn T_PLING_PLING relationalExpression(d1)   { EBin (Coalesce, $1, $3) }

(*----------------------------*)
(* Expression variants (no statement-like constructs) *)
(*----------------------------*)

expressionNoStmt:
 | assignmentExpressionNoStmt { $1 }
 | expressionNoStmt "," assignmentExpression { ESeq ($1, $3) }

(* coupling: with assignmentExpression *)
assignmentExpressionNoStmt:
 | conditionalExpression(primaryExpressionNoStmt) { $1 }
 | e1=leftHandSideExpression_(primaryExpressionNoStmt) op=assignmentOperator e2=assignmentExpression
    {
      let e1 = assignment_target_of_expr (Some op) e1 in
      EBin (op, e1, e2)
    }
 (* es6: *)
 | arrowFunction { $1 }
 | asyncArrowFunction { $1 }
 (* es6: *)
 | T_YIELD { EYield {delegate = false; expr = None} }
 | T_YIELD e=assignmentExpression { EYield {delegate = false; expr = Some e } }
 | T_YIELD "*" e=assignmentExpression { EYield { delegate = true; expr = (Some e) } }

(*----------------------------*)
(* Expression variants (for concise arrow body) *)
(*----------------------------*)

primaryExpressionForConciseBody:
 | functionExpression       { $1 }
 | classExpression          { $1 }
 (* es6: *)
 | generatorExpression      { $1 }
 (* es7: *)
 | asyncFunctionExpression { $1 }
 | asyncGeneratorExpression{ $1 }

assignmentExpressionForConciseBody:
 | conditionalExpression(primaryExpressionForConciseBody) { $1 }
 | e1=leftHandSideExpression_(primaryExpressionForConciseBody) op=assignmentOperator e2=assignmentExpression
    {
      let e1 = assignment_target_of_expr (Some op) e1 in
      EBin (op, e1, e2)
    }
 (* es6: *)
 | arrowFunction { $1 }
 | asyncArrowFunction { $1 }
 (* es6: *)
 | T_YIELD { EYield { delegate = false; expr = None } }
 | T_YIELD e=assignmentExpression { EYield {delegate = false; expr = (Some e) } }
 | T_YIELD "*" e=assignmentExpression { EYield {delegate = true; expr = (Some e) } }

(*************************************************************************)
(* Section 14: ECMAScript Language: Statements and Declarations         *)
(*************************************************************************)

(*----------------------------*)
(* 14.1 Statement Semantics *)
(*----------------------------*)

%inline
statement: s=statementBody { s, p $symbolstartpos }

statementBody:
 | block            { Block $1 }
 | variableStatement     { $1 }
 | emptyStatement        { $1 }
 | expressionStatement   { $1 }
 | ifStatement           { $1 }
 | iterationStatement    { $1 }
 | continueStatement     { $1 }
 | breakStatement        { $1 }
 | returnStatement       { $1 }
 | labelledStatement     { $1 }
 | switchStatement       { $1 }
 | throwStatement        { $1 }
 | tryStatement          { $1 }
 | withStatement         { $1 }
 | debuggerStatement     { $1 }

statementListItem:
 | statement { $1 }
 | declaration { $1 }

declaration:
 | functionDeclaration
   { let i,f = $1 in Function_declaration (i,f), p $symbolstartpos }
 | generatorDeclaration
   { let i,f = $1 in Function_declaration (i,f), p $symbolstartpos }
 | asyncGeneratorDeclaration
   { let i,f = $1 in Function_declaration (i,f), p $symbolstartpos }
 | asyncFunctionDeclaration
   { let i,f = $1 in Function_declaration (i,f), p $symbolstartpos }
 | lexicalDeclaration    { $1, p $symbolstartpos }
 | classDeclaration
   { let i,f = $1 in Class_declaration (i,f), p $symbolstartpos }

(*----------------------------*)
(* 14.2 Block *)
(*----------------------------*)

block: "{" l=optl(statementList) "}" { l }

statementList: statementListItem+ { $1 }

(*----------------------------*)
(* 14.3 Declarations and the Variable Statement *)
(*----------------------------*)

(* 14.3.1 Let and Const Declarations *)
lexicalDeclaration:
 (* es6: *)
 | T_CONST l=listc(lexicalBinding) sc { Variable_statement (Const, l)}
 | T_LET l=listc(lexicalBinding) sc { Variable_statement (Let, l)}

(* 14.3.2 Variable Statement *)
variableStatement:
 | T_VAR l=listc(variableDeclaration) sc { Variable_statement (Var, l) }

variableDeclaration:
 | i=identifier e=initializer_?            { DeclIdent (i,e) }
 | p=bindingPattern e=initializer_   { DeclPattern (p, e) }

lexicalBinding:
 | i=identifier e=initializer_?            { DeclIdent (i,e) }
 | p=bindingPattern e=initializer_   { DeclPattern (p, e) }

initializer_:
 | "=" e=assignmentExpression { e, p $symbolstartpos }

initializerNoIn:
 | "=" e=assignmentExpressionNoIn { e, p $symbolstartpos }

forDeclaration:
 | T_VAR l=listc(variableDeclarationNoIn)   { Var, l }
 (* es6: *)
 | T_CONST l=listc(variableDeclarationNoIn) { Const, l }
 | T_LET l=listc(variableDeclarationNoIn)   { Let, l }

variableDeclarationNoIn:
 | i=identifier e=initializerNoIn              { DeclIdent (i,Some e) }
 | i=identifier                                  { DeclIdent (i, None) }
 | p=bindingPattern e=initializerNoIn { DeclPattern (p, e) }

(* 'for ... in' and 'for ... of' declare only one variable *)
forBinding:
 | T_VAR b=forBindingElement   { Var, b }
 (* es6: *)
 | T_CONST b=forBindingElement { Const, b }
 | T_LET  b=forBindingElement  { Let, b }

forBindingElement:
 | singleNameBinding               { $1 }

(*----------------------------*)
(* 14.3.3 Destructuring Binding Patterns *)
(*----------------------------*)

bindingPattern:
 | objectBindingPattern { $1 }
 | arrayBindingPattern  { $1 }

singleNameBinding:
 | bindingPattern { BindingPattern $1 }
 | identifier           { BindingIdent $1 }

objectBindingPattern:
 | "{" "}"                                { ObjectBinding (list []) }
 | "{" r=bindingRestProperty "}"         { ObjectBinding {list = []; rest = Some r } }
 | "{" l=listc(bindingProperty) ","? "}" { ObjectBinding (list l) }
 | "{" l=listc(bindingProperty) "," r=bindingRestProperty "}"
    { ObjectBinding {list=l;rest= Some r} }

bindingProperty:
  | i=identifier e=initializer_? { Prop_ident (Prop_and_ident i, e) }
  | pn=propertyName ":" e=bindingElement { Prop_binding (pn, e) }

bindingRestProperty:
 (* can appear only at the end of a bindingPropertyList in ECMA *)
 | "..." id=identifier      { id }

(* in theory used also for formal parameter as is *)
bindingElement:
 | b=singleNameBinding e=initializer_? { b, e }

(* array destructuring *)

arrayBindingPattern:
  | "[" l=listc_with_empty2(bindingElement, bindingRestElement) "]" {
        ArrayBinding {list = fst l; rest = snd l }
  }

bindingRestElement:
 (* can appear only at the end of a arrayBindingPattern in ECMA *)
 | "..." singleNameBinding            { $2 }

(*----------------------------*)
(* 14.4 Empty Statement *)
(*----------------------------*)

emptyStatement:
 | T_SEMICOLON { Empty_statement }

(*----------------------------*)
(* 14.5 Expression Statement *)
(*----------------------------*)

expressionStatement:
 | expressionNoStmt sc { Expression_statement $1 }

(*----------------------------*)
(* 14.6 The if Statement *)
(*----------------------------*)

ifStatement:
 | T_IF "(" c=expression ")" t=statement T_ELSE e=statement
     { If_statement (c, t, Some e) }
 | T_IF "(" c=expression ")" t=statement %prec p_IF
     { If_statement (c, t, None) }

(*----------------------------*)
(* 14.7 Iteration Statements *)
(*----------------------------*)

iterationStatement:
 (* 14.7.2 The do-while Statement *)
 | T_DO body=statement T_WHILE "(" condition=expression ")" endrule(sc | T_VIRTUAL_SEMICOLON_DO_WHILE { () } )
    { Do_while_statement (body, condition) }

 (* 14.7.3 The while Statement *)
 | T_WHILE "(" condition=expression ")" body=statement
     { While_statement (condition, body) }

 (* 14.7.4 The for Statement *)
 | T_FOR "(" i=expressionNoIn? ";" c=expression? ";" incr=expression? ")" st=statement
   { For_statement (Left i, c, incr, st) }
 | T_FOR "(" l=forDeclaration ";" c=expression? ";" incr=expression? ")" st=statement
   { For_statement (Right l, c, incr, st) }

 (* 14.7.5 The for-in and for-of Statements *)
 | T_FOR "(" left=leftHandSideExpression T_IN right=expression ")" body=statement
   { let left = assignment_target_of_expr None left in
     ForIn_statement (Left left, right, body) }
 | T_FOR "(" left=forBinding T_IN right=expression ")" body=statement
   { ForIn_statement (Right left, right, body) }

 | T_FOR "(" left=leftHandSideExpression T_OF right=assignmentExpression ")" body=statement
    { let left = assignment_target_of_expr None left in
      ForOf_statement (Left left, right, body) }
 | T_FOR "(" left=forBinding T_OF right=assignmentExpression ")" body=statement
   { ForOf_statement (Right left, right, body) }
 | T_FOR T_AWAIT "(" left=leftHandSideExpression T_OF right=assignmentExpression ")" body=statement
    { let left = assignment_target_of_expr None left in
      ForAwaitOf_statement (Left left, right, body) }
 | T_FOR T_AWAIT "(" left=forBinding T_OF right=assignmentExpression ")" body=statement
   { ForAwaitOf_statement (Right left, right, body) }

(*----------------------------*)
(* 14.8 The continue Statement *)
(*----------------------------*)

continueStatement:
 | T_CONTINUE l=labelIdentifier? sc { (Continue_statement (l)) }

(*----------------------------*)
(* 14.9 The break Statement *)
(*----------------------------*)

breakStatement:
 | T_BREAK l=labelIdentifier? sc { (Break_statement (l)) }

(*----------------------------*)
(* 14.10 The return Statement *)
(*----------------------------*)

returnStatement:
 | T_RETURN e=expression? sc { (Return_statement (e, p $endpos(e))) }

(*----------------------------*)
(* 14.11 The with Statement *)
(*----------------------------*)

withStatement:
 | T_WITH "(" e=expression ")" s=statement { (With_statement (e,s)) }

(*----------------------------*)
(* 14.12 The switch Statement *)
(*----------------------------*)

switchStatement:
 | T_SWITCH "(" subject=expression ")" cb=caseBlock
   { let c1, d, c2 = cb in
     Switch_statement (subject, c1, d, c2)
   }

caseBlock:
 | "{" caseClause* "}" { $2, None, [] }
 | "{" caseClause* defaultClause caseClause* "}" { $2, Some $3, $4 }

caseClause:
 | T_CASE e=expression ":" s= optl(statementList) { e,s }

defaultClause:
 | T_DEFAULT ":" list=optl(statementList) { list }

(*----------------------------*)
(* 14.13 Labelled Statements *)
(*----------------------------*)

labelledStatement:
 | l=labelIdentifier ":" s=statement { Labelled_statement (l, s)}

(*----------------------------*)
(* 14.14 The throw Statement *)
(*----------------------------*)

throwStatement:
 | T_THROW e=expression sc { (Throw_statement e) }

(*----------------------------*)
(* 14.15 The try Statement *)
(*----------------------------*)

tryStatement:
 | T_TRY b=block c=catch { (Try_statement (b, Some c, None)) }
 | T_TRY b=block         f=finally { (Try_statement (b, None, Some f)) }
 | T_TRY b=block c=catch f=finally { (Try_statement (b, Some c, Some f)) }

catch:
 | T_CATCH "(" p=formalParameter ")" b=block { Some p,b }
 | T_CATCH b=block { None,b }

finally:
 | T_FINALLY b=block { b }

(*----------------------------*)
(* 14.16 The debugger Statement *)
(*----------------------------*)

debuggerStatement:
 | T_DEBUGGER sc { Debugger_statement }

(*************************************************************************)
(* Section 15: ECMAScript Language: Functions and Classes               *)
(*************************************************************************)

(*----------------------------*)
(* 15.1 Parameter Lists *)
(*----------------------------*)

formalParameters:
 | (*empty*)                                     { list [] }
 | formalParameterListRev ","?                    { list (List.rev $1) }
 | r=functionRestParameter                           { { list = []; rest = Some r } }
 | formalParameterListRev "," r=functionRestParameter { { list = List.rev $1; rest = Some r } }

functionRestParameter:
 | "..." singleNameBinding { $2 }

(* must be written in a left-recursive way (see conflicts.txt) *)
formalParameterListRev:
 | formalParameterListRev "," formalParameter { $3::$1 }
 | formalParameter                           { [$1] }

(* The ECMA and Typescript grammars imposes more restrictions
 * (some require_parameter, optional_parameter, rest_parameter)
 * but I've simplified.
 * We could also factorize with bindingElement as done by ECMA.
 *)
formalParameter:
  | singleNameBinding initializer_? { $1, $2 }

callSignature: "(" args=formalParameters ")"
  { args }

functionBody: optl(statementList) { $1 }

(*----------------------------*)
(* 15.2 Function Definitions *)
(*----------------------------*)

functionDeclaration:
 | T_FUNCTION name=identifier args=callSignature "{" b=functionBody "}"
    { (name, ({async = false; generator = false}, args, b, p $startpos($6))) }

functionExpression:
 | T_FUNCTION name=identifier? args=callSignature "{" b=functionBody "}"
   { EFun (name, ({async = false; generator = false}, args, b, p $symbolstartpos)) }

(*----------------------------*)
(* 15.3 Arrow Function Definitions *)
(*----------------------------*)

(* TODO conflict with as then in identifierKeyword *)
arrowFunction:
  | i=identifier T_ARROW b=conciseBody
    { let b,consise = b in
      EArrow (({async = false; generator = false}, list [param' i],b, p $symbolstartpos), consise, AUnknown) }
  | T_LPAREN_ARROW a=formalParameters ")" T_ARROW b=conciseBody
    { let b,consise = b in
      EArrow (({async = false; generator = false}, a,b, p $symbolstartpos), consise, AUnknown) }

conciseBody:
 | "{" b=functionBody "}" { b, false }
 | e=assignmentExpressionForConciseBody { [(Return_statement (Some e, p $endpos), p $symbolstartpos)], true }

(*----------------------------*)
(* 15.4 Method Definitions *)
(*----------------------------*)

methodDefinition(name):
 | T_GET name=name args=callSignature "{" b=functionBody "}" { name, MethodGet(({async = false; generator = false}, args, b, p $symbolstartpos)) }
 | T_SET name=name args=callSignature "{" b=functionBody "}" { name, MethodSet(({async = false; generator = false}, args, b, p $symbolstartpos)) }
 | name=name args=callSignature "{" b=functionBody "}" {
      name, Method(({async = false; generator = false}, args, b, p $symbolstartpos)) }
 | T_ASYNC name=name args=callSignature "{" b=functionBody "}" {
      name, Method(({async = true; generator = false}, args, b, p $symbolstartpos)) }
 | "*" name=name args=callSignature "{" b=functionBody "}" {
      name, Method(({async = false; generator = true}, args, b, p $symbolstartpos)) }
 | T_ASYNC "*" name=name args=callSignature "{" b=functionBody "}" {
      name, Method(({async = true; generator = true}, args, b, p $symbolstartpos)) }

(*----------------------------*)
(* 15.5 Generator Function Definitions *)
(*----------------------------*)

generatorDeclaration:
 | T_FUNCTION "*" name=identifier args=callSignature "{" b=functionBody "}"
   { (name, ({async = false; generator = true}, args, b, p $symbolstartpos)) }

generatorExpression:
 | T_FUNCTION "*" name=identifier? args=callSignature "{" b=functionBody "}"
   { EFun (name, ({async = false; generator = true}, args, b, p $symbolstartpos)) }

(*----------------------------*)
(* 15.6 Async Generator Function Definitions *)
(*----------------------------*)

asyncGeneratorDeclaration:
 | T_ASYNC T_FUNCTION "*" name=identifier args=callSignature "{" b=functionBody "}"
   { (name, ({async = true; generator = true}, args, b, p $symbolstartpos)) }

asyncGeneratorExpression:
 | T_ASYNC T_FUNCTION "*" name=identifier? args=callSignature "{" b=functionBody "}"
   { EFun (name, ({async = true; generator = true}, args, b, p $symbolstartpos)) }

(*----------------------------*)
(* 15.7 Class Definitions *)
(*----------------------------*)

classDeclaration: T_CLASS id=bindingIdentifier extends=classHeritage? body=classBody
   { id, {extends; body}  }

classExpression: T_CLASS i=bindingIdentifier? extends=classHeritage? body=classBody
   { EClass (i, {extends; body}) }

classHeritage: T_EXTENDS leftHandSideExpression { $2 }

classBody: "{" classElement* "}" { List.flatten $2 }

(* can't factorize with static_opt, or access_modifier_opt; ambiguities  *)
classElement:
 |          m=methodDefinition(classElementName)
    { let n,m = m in [ CEMethod (false, n, m) ] }
 | T_STATIC m=methodDefinition(classElementName)
    { let n,m = m in [ CEMethod (true, n, m) ] }

 |          n=classElementName i=initializer_? sc
    { [ CEField (false, n, i) ] }
 | T_STATIC n=classElementName i=initializer_? sc
    { [ CEField (true, n, i) ] }
 | T_STATIC b=block { [CEStaticBLock b] }
 | sc    { [] }

classElementName:
  | propertyName { PropName $1 }
  | T_POUND identifierName { PrivName $2 }

(*----------------------------*)
(* 15.8 Async Function Definitions *)
(*----------------------------*)

asyncFunctionDeclaration:
 | T_ASYNC T_FUNCTION  name=identifier args=callSignature "{" b=functionBody "}"
   { (name, ({async = true; generator = false}, args, b, p $symbolstartpos)) }

asyncFunctionExpression:
 | T_ASYNC T_FUNCTION name=identifier? args=callSignature "{" b=functionBody "}"
   { EFun (name, ({async = true; generator = false}, args, b, p $symbolstartpos)) }

asyncArrowFunction:
  | T_ASYNC i=identifier T_ARROW b=conciseBody {
      let b,consise = b in
      EArrow(({async = true; generator = false}, list [param' i],b, p $symbolstartpos), consise, AUnknown) }
  | T_ASYNC T_LPAREN_ARROW a=formalParameters ")" T_ARROW b=conciseBody
    { let b,consise = b in
      EArrow (({async = true; generator = false}, a,b, p $symbolstartpos), consise, AUnknown) }

(*************************************************************************)
(* Section 16: ECMAScript Language: Scripts and Modules                 *)
(*************************************************************************)

(*----------------------------*)
(* 16.1 Scripts *)
(*----------------------------*)

standalone_expression:
 | e=expression T_EOF { e }

program:
 | l=moduleItem* T_EOF { l }

(*----------------------------*)
(* 16.2 Modules *)
(*----------------------------*)

moduleItem:
  | statementListItem { $symbolstartpos, $1 }
  | importDeclaration { $symbolstartpos, $1 }
  | exportDeclaration { $symbolstartpos, $1 }

(*----------------------------*)
(* 16.2.2 Imports *)
(*----------------------------*)

importDeclaration:
 | T_IMPORT kind=importClause from=fromClause sc
    { let pos = $symbolstartpos in
      Import ({ from; kind }, pi pos), p pos }
 | T_IMPORT from=moduleSpecifier sc
    { let pos = $symbolstartpos in
      Import ({ from; kind = SideEffect }, pi pos), p pos }

importClause:
 | importedDefaultBinding                            { Default $1 }
 | importedDefaultBinding "," "*" T_AS id=bindingIdentifier { Namespace (Some $1, id) }
 | "*" T_AS id=bindingIdentifier                    { Namespace (None, id) }
 | importedDefaultBinding "," x=namedImports { Named (Some $1, x) }
 | x=namedImports                    { Named (None, x) }

importedDefaultBinding: bindingIdentifier { $1 }

namedImports:
 | "{" "}"                             { [] }
 | "{" listc(importSpecifier) "}"     { $2 }
 | "{" listc(importSpecifier) "," "}" { $2 }

(* also valid for export *)
fromClause: T_FROM moduleSpecifier {$2 }

importSpecifier:
 | bindingIdentifier                 { (name_of_ident $1, $1) }
 | moduleExportName T_AS bindingIdentifier         {
   let (_,s,_) = $1 in
   (s, $3) }

%inline moduleExportName:
 | T_STRING { `String, fst $1, $symbolstartpos }
 | identifierName { `Ident, $1, $symbolstartpos }
 | identifierKeyword { `Ident, $1, $symbolstartpos }

moduleSpecifier:
  | T_STRING { (fst $1) }

(*----------------------------*)
(* 16.2.3 Exports *)
(*----------------------------*)

exportFunctionOrClass:
 | functionExpression       { $1 }
 | classExpression          { $1 }
 (* es6: *)
 | generatorExpression      { $1 }
 (* es7: *)
 | asyncFunctionExpression { $1 }
 | asyncGeneratorExpression{ $1 }


exportDeclaration:
  | T_EXPORT names=exportClause sc {
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
 | T_EXPORT v=variableStatement
    {
      let pos = $symbolstartpos in
      let k = match v with
        | Variable_statement (k,l) -> ExportVar (k, l)
        | _ -> assert false
      in
      Export (k, pi pos), p pos }
 | T_EXPORT d=declaration
    { let k = match d with
        | Variable_statement (k,l),_ -> ExportVar (k,l)
        | Function_declaration (id, decl),_ -> ExportFun (id,decl)
        | Class_declaration (id, decl),_ -> ExportClass (id,decl)
        | _ -> assert false
      in
      let pos = $symbolstartpos in
      Export (k,pi pos), p pos }
 | T_EXPORT T_DEFAULT e=assignmentExpressionNoStmt sc
    {
      let k = ExportDefaultExpression e in
      let pos = $symbolstartpos in
      Export (k,pi pos), p pos }
 | T_EXPORT T_DEFAULT e=objectLiteral sc
    {
      let k = ExportDefaultExpression e in
      let pos = $symbolstartpos in
      Export (k,pi pos), p pos }
 | T_EXPORT T_DEFAULT e=exportFunctionOrClass endrule(sc | T_VIRTUAL_SEMICOLON_EXPORT_DEFAULT { () } )
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
| T_EXPORT "*" T_FROM from=moduleSpecifier sc {
    let kind = Export_all None in
    let pos = $symbolstartpos in
    Export (ExportFrom ({from; kind}),pi pos), p pos
  }
 | T_EXPORT "*" T_AS id=moduleExportName T_FROM from=moduleSpecifier sc {
    let (_,id,_) = id in
    let kind = Export_all (Some id) in
    let pos = $symbolstartpos in
    Export (ExportFrom ({from; kind}), pi pos), p pos
  }
| T_EXPORT names=exportClause T_FROM from=moduleSpecifier sc {
    let names = List.map (fun ((_,a,_), (_,b,_)) -> a, b) names in
    let kind = Export_names names in
    let pos = $symbolstartpos in
    Export (ExportFrom ({from; kind}), pi pos), p pos
  }

exportSpecifier:
 | moduleExportName                       { ($1, $1) }
 | moduleExportName T_AS moduleExportName  { ($1, $3) }

exportClause:
 | "{" "}"                              { [] }
 | "{" listc(exportSpecifier) "}"      { $2 }
 | "{" listc(exportSpecifier) ","  "}" { $2 }

(*************************************************************************)
(* Misc *)
(*************************************************************************)

sc:
 | ";"                 { $1 }
 | T_VIRTUAL_SEMICOLON { $1 }
