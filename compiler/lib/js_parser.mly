(* Js_of_ocaml compiler *)
(* Copyright (C) 2013-2025 Hugo Heuzard *)

%{

(* Yoann Padioleau
 *
 * Copyright (C) 2010-2014 Facebook
 * Copyright (C) 2019-2022 r2c
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file LICENSE.
 *)

(*************************************************************************)
(* Prelude *)
(*************************************************************************)
(* This file contains a grammar for Javascript (ES6 and more).
 *
 * reference:
 *  - https://en.wikipedia.org/wiki/JavaScript_syntax
 *  - http://www.ecma-international.org/publications/standards/Ecma-262.htm
 *
 * The grammar rules are organized to follow the structure of the
 * ECMAScript specification (ECMA-262):
 *  - Section 12: Lexical Grammar (identifiers, literals)
 *  - Section 13: Expressions
 *  - Section 14: Statements and Declarations
 *  - Section 15: Functions and Classes
 *  - Section 16: Scripts and Modules
 *
 * Operator precedence is encoded directly in the grammar structure
 * (no %left/%right annotations), following the ECMA specification.
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
T_GET T_SET T_USING
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
(* Priorities *)
(*-----------------------------------------*)

(* Special if / else associativity for dangling else *)
%nonassoc p_IF
%nonassoc T_ELSE

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

(* Guard rules for 'in' operator - used to parameterize expression rules *)
(* in_allowed: empty rule, allows the T_IN production *)
(* in_disallowed: matches T_EOF which never appears mid-expression, effectively disabling the production *)
%inline in_allowed: { }
%inline in_disallowed: T_EOF { }



(*************************************************************************)
(* Section 12: ECMAScript Language: Lexical Grammar                     *)
(*************************************************************************)

(*----------------------------*)
(* 12.7 Names and Keywords *)
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
 | T_USING { T_USING }

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

(*----------------------------*)
(* 12.9.1 Null Literals *)
(*----------------------------*)

nullLiteral:
 | T_NULL { (EVar (var (p $symbolstartpos) (Stdlib.Utf8_string.of_string_exn "null"))) }

(*----------------------------*)
(* 12.9.2 Boolean Literals *)
(*----------------------------*)

booleanLiteral:
 | T_TRUE  { (EBool true) }
 | T_FALSE { (EBool false) }

(*----------------------------*)
(* 12.9.3 Numeric Literals *)
(*----------------------------*)

numericLiteral:
 | T_NUMBER { let _,f = $1 in (f) }

bigIntLiteral:
 | T_BIGINT { let _,f = $1 in (f) }

(*----------------------------*)
(* 12.9.4 String Literals *)
(*----------------------------*)

stringLiteral: s=T_STRING { (EStr (fst s)) }

(*----------------------------*)
(* 12.9.5 Regular Expression Literals *)
(*----------------------------*)

regularExpressionLiteral:
 | r=T_REGEXP {
   let (Utf8 s, f) = r in
   (ERegexp (s, if String.equal f "" then None else Some f)) }

(*----------------------------*)
(* 12.9.6 Template Literal Lexical Components *)
(*----------------------------*)

templateLiteral: T_BACKQUOTE templateSpan* T_BACKQUOTE  { $2 }

templateSpan:
 | T_ENCAPSED_STRING        { TStr (Stdlib.Utf8_string.of_string_exn $1) }
 | T_DOLLARCURLY expression(in_allowed) "}"   { TExp $2 }

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

d1:
 | primaryExpression_FunClass { $1 }
 | primaryExpression_Object   { $1 }

primaryExpression_Object:
 | objectLiteral { $1 }

primaryExpression_Empty: T_ERROR TComment { assert false }

primaryExpression_FunClass:
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
 | "(" e=expression(in_allowed) ","? ")" { e }
 | "(" ")" { CoverParenthesizedExpressionAndArrowParameterList (early_error (pi $startpos($2))) }
 | "(" "..." bindingElement ")" { CoverParenthesizedExpressionAndArrowParameterList (early_error (pi $startpos($2)) ) }
 | "(" expression(in_allowed) "," "..." bindingElement ")" { CoverParenthesizedExpressionAndArrowParameterList (early_error (pi $startpos($4)) ) }

(*----------------------------*)
(* 13.2.4 Array Initializer *)
(*----------------------------*)

arrayLiteral:
  | "[" l=listc_with_empty (arrayElement) "]"
    { (EArr (List.map (function None -> ElementHole | Some x -> x) l)) }

arrayElement:
 | assignmentExpression(in_allowed)       { Element $1 }
 (* es6: SpreadElement *)
 | "..." assignmentExpression(in_allowed) { ElementSpread $2 }

(*----------------------------*)
(* 13.2.5 Object Initializer *)
(*----------------------------*)

propertyName:
 | i=identifierName { PNI i }
 | i=identifierKeyword { PNI i }
 | s=T_STRING         {
    let s, _len = s in PNS s }
 | n=numericLiteral  { PNN (Num.of_string_unsafe (n)) }
 | n=bigIntLiteral  { PNN (Num.of_string_unsafe (n)) }
 | "[" p=assignmentExpression(in_allowed) "]" { PComputed p }

objectLiteral:
 | "{" "}"                                      { EObj [] }
 | "{" listc(propertyDefinition) ","? "}"       { EObj $2 }

propertyDefinition:
 | propertyName ":" assignmentExpression(in_allowed)    { Property ($1, $3) }
 (* es6: shorthand property *)
 | i=identifierName          { Property (PNI i, EVar (ident_unsafe i)) }
 | identifier initializer_(in_allowed)  { CoverInitializedName (early_error (pi $startpos($2)), $1, $2)  }
 (* es6: spread property *)
 | "..." assignmentExpression(in_allowed)                { PropertySpread($2) }
  | methodDefinition(propertyName)
    { let n, m = $1 in PropertyMethod(n,m) }

(*----------------------------*)
(* 13.3 Left-Hand-Side Expressions *)
(*----------------------------*)

leftHandSideExpression: leftHandSideExpression_(d1) { $1 }

leftHandSideExpression_(x):
 | newExpression(x)  { $1 }
 | callExpression(x) { $1 }
 | optionalExpression(x) { $1 }

(*----------------------------*)
(* 13.3.2 Property Accessors *)
(*----------------------------*)

fieldName:
 | identifierName    { $1 }
 | identifierKeyword { $1 }

memberExpression(x):
 | e=primaryExpression(x)
    { e }
 | T_IMPORT "." T_META
    { EDot (vartok $startpos($1) T_IMPORT,ANormal,(Stdlib.Utf8_string.of_string_exn "meta")) }
 | e1=memberExpression(x) "[" e2=expression(in_allowed) "]"
     { (EAccess (e1,ANormal, e2)) }
 | e1=memberExpression(x) "." i=fieldName
     { (EDot(e1,ANormal,i)) }
 | T_NEW e1=memberExpression(d1) a=arguments
     { (ENew(e1, Some a, p $symbolstartpos)) }
 | e=memberExpression(x) t=templateLiteral
     { ECallTemplate(e, t, p $symbolstartpos) }
 | T_SUPER "[" e=expression(in_allowed) "]"
      { (EAccess (vartok $startpos($1) T_SUPER,ANormal, e)) }
 | T_SUPER "." i=fieldName
     { (EDot(vartok $startpos($1) T_SUPER,ANormal,i)) }
  | T_NEW "." T_TARGET
     { (EDot(vartok $startpos($1) T_NEW,ANormal,Stdlib.Utf8_string.of_string_exn "target")) }
  | e1=memberExpression(x) "." T_POUND i=fieldName
    { (EDotPrivate(e1,ANormal,i)) }

(*----------------------------*)
(* 13.3.5 The new Operator *)
(*----------------------------*)

newExpression(x):
 | e=memberExpression(x)    { e }
 | T_NEW e=newExpression(d1) { (ENew (e,None, p $symbolstartpos)) }

(*----------------------------*)
(* 13.3.6 Function Calls *)
(*----------------------------*)

callExpression(x):
 | T_IMPORT a=arguments
     { (ECall(vartok $startpos($1) T_IMPORT, ANormal, a, p $symbolstartpos)) }
 | e=memberExpression(x) a=arguments
     { (ECall(e, ANormal, a, p $symbolstartpos)) }
 | e=callExpression(x) a=arguments
     { (ECall(e, ANormal, a, p $symbolstartpos)) }
 | e=callExpression(x) "[" e2=expression(in_allowed) "]"
     { (EAccess (e, ANormal,  e2)) }
 | e=callExpression(x) t=templateLiteral
    { ECallTemplate(e, t,p $symbolstartpos) }
 | T_SUPER a=arguments { ECall(vartok $startpos($1) T_SUPER,ANormal, a, p $symbolstartpos) }
 | e=callExpression(x) "." i=methodName
    { EDot (e,ANormal,i) }
 | e=callExpression(x) "." T_POUND i=methodName
    { EDotPrivate (e,ANormal,i) }

(*----------------------------*)
(* 13.3.8 Argument Lists *)
(*----------------------------*)

arguments:
 | "(" argumentList ")" { $2 }

argumentList:
 | (*empty*)   { [] }
 | listc(argumentListElement) ","?  { $1  }

(* assignmentExpression because expression supports sequence of exprs with ',' *)
argumentListElement:
 | assignmentExpression(in_allowed)       { Arg $1 }
 (* es6: spread element, allowed not only in last position *)
 | "..." assignmentExpression(in_allowed) { ArgSpread $2 }

(*----------------------------*)
(* 13.3.9 Optional Chains *)
(*----------------------------*)

optionalExpression(x):
 | e=memberExpression(x) c=optionalChain
    { c e }
 | e=callExpression(x) c=optionalChain
    { c e }
 | e=optionalExpression(x) c=optionalChain
    { c e }

optionalChain:
 (* ?. Arguments *)
 | T_PLING_PERIOD a=arguments
    { fun e -> ECall(e, ANullish, a, p $symbolstartpos) }
 (* ?. [ Expression ] *)
 | T_PLING_PERIOD "[" e2=expression(in_allowed) "]"
    { fun e -> EAccess(e, ANullish, e2) }
 (* ?. IdentifierName *)
 | T_PLING_PERIOD i=fieldName
    { fun e -> EDot(e, ANullish, i) }
 (* ?. TemplateLiteral, node is unhappy about it *)
(* | T_PLING_PERIOD t=templateLiteral
    { fun e -> ECallTemplate(e, t, p $symbolstartpos) } *)
 (* ?. PrivateIdentifier *)
 | T_PLING_PERIOD T_POUND i=fieldName
    { fun e -> EDotPrivate(e, ANullish, i) }
 (* OptionalChain Arguments *)
 | c=optionalChain a=arguments
    { fun e -> ECall(c e, ANormal, a, p $symbolstartpos) }
 (* OptionalChain [ Expression ] *)
 | c=optionalChain "[" e2=expression(in_allowed) "]"
    { fun e -> EAccess(c e, ANormal, e2) }
 (* OptionalChain . IdentifierName *)
 | c=optionalChain "." i=fieldName
    { fun e -> EDot(c e, ANormal, i) }
 (* OptionalChain TemplateLiteral *)
 | c=optionalChain t=templateLiteral
    { fun e -> ECallTemplate(c e, t, p $symbolstartpos) }
 (* OptionalChain . PrivateIdentifier *)
 | c=optionalChain "." T_POUND i=fieldName
    { fun e -> EDotPrivate(c e, ANormal, i) }

(*----------------------------*)
(* 13.4 Update Expressions *)
(*----------------------------*)

updateExpression(x):
 | leftHandSideExpression_(x)                     { $1 }
 | leftHandSideExpression_(x) T_INCR_NB           { EUn (IncrA, $1) }
 | leftHandSideExpression_(x) T_DECR_NB           { EUn (DecrA, $1) }
 | T_INCR unaryExpression(d1)                     { EUn (IncrB, $2) }
 | T_DECR unaryExpression(d1)                     { EUn (DecrB, $2) }
 | T_INCR_NB unaryExpression(d1)                  { EUn (IncrB, $2) }
 | T_DECR_NB unaryExpression(d1)                  { EUn (DecrB, $2) }

(*----------------------------*)
(* 13.5 Unary Operators *)
(*----------------------------*)

unaryExpression(x):
 | updateExpression(x)                            { $1 }
 | T_DELETE unaryExpression(d1)                   { EUn (Delete, $2) }
 | T_VOID unaryExpression(d1)                     { EUn (Void, $2) }
 | T_TYPEOF unaryExpression(d1)                   { EUn (Typeof, $2) }
 | T_PLUS unaryExpression(d1)                     { EUn (Pl, $2) }
 | T_MINUS unaryExpression(d1)                    { EUn (Neg, $2)}
 | T_BIT_NOT unaryExpression(d1)                  { EUn (Bnot, $2) }
 | T_NOT unaryExpression(d1)                      { EUn (Not, $2) }
 (* es7: *)
 | T_AWAIT unaryExpression(d1)                    { EUn (Await, $2) }

(*----------------------------*)
(* 13.6 Exponentiation Operator *)
(*----------------------------*)

(* Note: ** is right-associative *)
exponentiationExpression(x):
 | unaryExpression(x)                                           { $1 }
 | updateExpression(x) T_EXP exponentiationExpression(d1)       { EBin(Exp, $1, $3) }

(*----------------------------*)
(* 13.7 Multiplicative Operators *)
(*----------------------------*)

multiplicativeExpression(x):
 | exponentiationExpression(x)                                  { $1 }
 | multiplicativeExpression(x) "*" exponentiationExpression(d1) { EBin(Mul, $1, $3) }
 | multiplicativeExpression(x) T_DIV exponentiationExpression(d1) { EBin(Div, $1, $3) }
 | multiplicativeExpression(x) T_MOD exponentiationExpression(d1) { EBin(Mod, $1, $3) }

(*----------------------------*)
(* 13.8 Additive Operators *)
(*----------------------------*)

additiveExpression(x):
 | multiplicativeExpression(x)                                  { $1 }
 | additiveExpression(x) T_PLUS multiplicativeExpression(d1)    { EBin(Plus, $1, $3) }
 | additiveExpression(x) T_MINUS multiplicativeExpression(d1)   { EBin(Minus, $1, $3) }

(*----------------------------*)
(* 13.9 Bitwise Shift Operators *)
(*----------------------------*)

shiftExpression(x):
 | additiveExpression(x)                                        { $1 }
 | shiftExpression(x) T_LSHIFT additiveExpression(d1)           { EBin(Lsl, $1, $3) }
 | shiftExpression(x) T_RSHIFT additiveExpression(d1)           { EBin(Asr, $1, $3) }
 | shiftExpression(x) T_RSHIFT3 additiveExpression(d1)          { EBin(Lsr, $1, $3) }

(*----------------------------*)
(* 13.10 Relational Operators *)
(*----------------------------*)

(* Parameterized by in_ which is either in_allowed or in_disallowed.
   The in_ prefix on the T_IN rule acts as a guard:
   - in_allowed is empty, so the T_IN production is active
   - in_disallowed matches T_EOF (impossible here), so the production is disabled *)
relationalExpression(x, in_):
 | shiftExpression(x)                                           { $1 }
 | relationalExpression(x, in_) T_LESS_THAN shiftExpression(d1)      { EBin(Lt, $1, $3) }
 | relationalExpression(x, in_) T_GREATER_THAN shiftExpression(d1)   { EBin(Gt, $1, $3) }
 | relationalExpression(x, in_) T_LESS_THAN_EQUAL shiftExpression(d1) { EBin(Le, $1, $3) }
 | relationalExpression(x, in_) T_GREATER_THAN_EQUAL shiftExpression(d1) { EBin(Ge, $1, $3) }
 | relationalExpression(x, in_) T_INSTANCEOF shiftExpression(d1)     { EBin (InstanceOf, $1, $3) }
 | in_ e1=relationalExpression(x, in_) T_IN e2=shiftExpression(d1)   { EBin (In, e1, e2) }

(*----------------------------*)
(* 13.11 Equality Operators *)
(*----------------------------*)

equalityExpression(x, in_):
 | relationalExpression(x, in_)                                      { $1 }
 | equalityExpression(x, in_) T_EQUAL relationalExpression(d1, in_)       { EBin(EqEq, $1, $3) }
 | equalityExpression(x, in_) T_NOT_EQUAL relationalExpression(d1, in_)   { EBin(NotEq, $1, $3) }
 | equalityExpression(x, in_) T_STRICT_EQUAL relationalExpression(d1, in_) { EBin(EqEqEq, $1, $3) }
 | equalityExpression(x, in_) T_STRICT_NOT_EQUAL relationalExpression(d1, in_) { EBin(NotEqEq, $1, $3) }

(*----------------------------*)
(* 13.12 Binary Bitwise Operators *)
(*----------------------------*)

bitwiseANDExpression(x, in_):
 | equalityExpression(x, in_)                                        { $1 }
 | bitwiseANDExpression(x, in_) T_BIT_AND equalityExpression(d1, in_)     { EBin(Band, $1, $3) }

bitwiseXORExpression(x, in_):
 | bitwiseANDExpression(x, in_)                                      { $1 }
 | bitwiseXORExpression(x, in_) T_BIT_XOR bitwiseANDExpression(d1, in_)   { EBin(Bxor, $1, $3) }

bitwiseORExpression(x, in_):
 | bitwiseXORExpression(x, in_)                                      { $1 }
 | bitwiseORExpression(x, in_) T_BIT_OR bitwiseXORExpression(d1, in_)     { EBin(Bor, $1, $3) }

(*----------------------------*)
(* 13.13 Binary Logical Operators *)
(*----------------------------*)

logicalANDExpression(x, in_):
 | bitwiseORExpression(x, in_)                                       { $1 }
 | logicalANDExpression(x, in_) T_AND bitwiseORExpression(d1, in_)        { EBin(And, $1, $3) }

logicalORExpression(x, in_):
 | logicalANDExpression(x, in_)                                      { $1 }
 | logicalORExpression(x, in_) T_OR logicalANDExpression(d1, in_)         { EBin(Or, $1, $3) }

(* Coalesce expression - can't mix with || or && without parens *)
coalesceExpression(x, in_):
  | coalesceExpressionHead(x, in_) T_PLING_PLING bitwiseORExpression(d1, in_)  { EBin(Coalesce, $1, $3) }

coalesceExpressionHead(x, in_):
  | coalesceExpression(x, in_) { $1 }
  | bitwiseORExpression(x, in_)  { $1 }

(* ShortCircuitExpression: either logical OR chain or coalesce chain *)
shortCircuitExpression(x, in_):
 | logicalORExpression(x, in_)                                       { $1 }
 | coalesceExpression(x, in_)                                        { $1 }

(*----------------------------*)
(* 13.14 Conditional Operator ( ? : ) *)
(*----------------------------*)

conditionalExpression(x, in_):
 | shortCircuitExpression(x, in_)                                    { $1 }
 | shortCircuitExpression(x, in_) "?" assignmentExpression(in_allowed) ":" assignmentExpression(in_)
   { ECond ($1, $3, $5) }

(*----------------------------*)
(* 13.15 Assignment Operators *)
(*----------------------------*)

assignmentExpression(in_):
 | conditionalExpression(d1, in_) { $1 }
 | e1=leftHandSideExpression_(d1) op=assignmentOperator e2=assignmentExpression(in_)
    {
      let e1 = assignment_target_of_expr (Some op) e1 in
      EBin (op, e1, e2)
    }
 | arrowFunction(in_) { $1 }
 | in_ e=asyncArrowFunction(in_) { e }  (* guarded: avoid conflict with 'for (async of ...)' *)
 | yieldExpression(in_) { $1 }

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

expression(in_):
 | assignmentExpression(in_) { $1 }
 | e1=expression(in_) "," e2=assignmentExpression(in_) { ESeq (e1, e2) }

(*----------------------------*)
(* Expression variants (no statement-like constructs) *)
(*----------------------------*)

expressionNoStmt:
 | assignmentExpression_NoStmt { $1 }
 | expressionNoStmt "," assignmentExpression(in_allowed) { ESeq ($1, $3) }

(* coupling: with assignmentExpression *)
assignmentExpression_NoStmt:
 | conditionalExpression(primaryExpression_Empty, in_allowed) { $1 }
 | e1=leftHandSideExpression_(primaryExpression_Empty) op=assignmentOperator e2=assignmentExpression(in_allowed)
    {
      let e1 = assignment_target_of_expr (Some op) e1 in
      EBin (op, e1, e2)
    }
 (* es6: *)
 | arrowFunction(in_allowed) { $1 }
 | asyncArrowFunction(in_allowed) { $1 }
 (* es6: *)
 | yieldExpression(in_allowed) { $1 }

(*----------------------------*)
(* Expression variants (for concise arrow body) *)
(*----------------------------*)

assignmentExpressionForConciseBody(in_):
 | conditionalExpression(primaryExpression_FunClass, in_) { $1 }
 | e1=leftHandSideExpression_(primaryExpression_FunClass) op=assignmentOperator e2=assignmentExpression(in_)
    {
      let e1 = assignment_target_of_expr (Some op) e1 in
      EBin (op, e1, e2)
    }
 (* es6: *)
 | arrowFunction(in_) { $1 }
 | asyncArrowFunction(in_) { $1 }
 (* es6: *)
 | yieldExpression(in_) { $1 }

(*************************************************************************)
(* Section 14: ECMAScript Language: Statements and Declarations         *)
(*************************************************************************)

(*----------------------------*)
(* 14.1 Statement Semantics *)
(*----------------------------*)

%inline
statement: s=statementBody { s, p $symbolstartpos }

statementBody:
 | block                 { Block $1 }
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
 (* Explicit Resource Management *)
 | T_USING l=listc(usingBinding) sc { Variable_statement (Using, l)}
 | T_AWAIT T_USING l=listc(usingBinding) sc { Variable_statement (AwaitUsing, l)}

(* 14.3.2 Variable Statement *)
variableStatement:
 | T_VAR l=listc(variableDeclaration(in_allowed)) sc { Variable_statement (Var, l) }

variableDeclaration(in_):
 | i=identifier e=initializer_(in_)?            { DeclIdent (i,e) }
 | p=bindingPattern e=initializer_(in_)   { DeclPattern (p, e) }

lexicalBinding:
 | i=identifier e=initializer_(in_allowed)?            { DeclIdent (i,e) }
 | p=bindingPattern e=initializer_(in_allowed)   { DeclPattern (p, e) }

(* using bindings require an initializer *)
usingBinding:
 | i=identifier e=initializer_(in_allowed)       { DeclIdent (i, Some e) }
 | p=bindingPattern e=initializer_(in_allowed)   { DeclPattern (p, e) }

initializer_(in_):
 | "=" e=assignmentExpression(in_) { e, p $symbolstartpos }

forDeclaration:
 | T_VAR l=listc(variableDeclaration(in_disallowed) )  { Var, l }
 (* es6: *)
 | T_CONST l=listc(variableDeclaration(in_disallowed)) { Const, l }
 | T_LET l=listc(variableDeclaration(in_disallowed))   { Let, l }

(* 'for ... in' and 'for ... of' declare only one variable *)
forBinding:
 | T_VAR b=forBindingElement   { Var, b }
 (* es6: *)
 | T_CONST b=forBindingElement { Const, b }
 | T_LET  b=forBindingElement  { Let, b }
 (* Explicit Resource Management *)
 | T_USING b=forBindingElement { Using, b }
 | T_AWAIT T_USING b=forBindingElement { AwaitUsing, b }

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
  | i=identifier e=initializer_(in_allowed)? { Prop_ident (Prop_and_ident i, e) }
  | pn=propertyName ":" e=bindingElement { Prop_binding (pn, e) }

bindingRestProperty:
 (* can appear only at the end of a bindingPropertyList in ECMA *)
 | "..." id=identifier      { id }

(* in theory used also for formal parameter as is *)
bindingElement:
 | b=singleNameBinding e=initializer_(in_allowed)? { b, e }

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
 | T_IF "(" c=expression(in_allowed) ")" t=statement T_ELSE e=statement
     { If_statement (c, t, Some e) }
 | T_IF "(" c=expression(in_allowed) ")" t=statement %prec p_IF
     { If_statement (c, t, None) }

(*----------------------------*)
(* 14.7 Iteration Statements *)
(*----------------------------*)

iterationStatement:
 (* 14.7.2 The do-while Statement *)
 | T_DO body=statement T_WHILE "(" condition=expression(in_allowed) ")" endrule(sc | T_VIRTUAL_SEMICOLON_DO_WHILE { () } )
    { Do_while_statement (body, condition) }

 (* 14.7.3 The while Statement *)
 | T_WHILE "(" condition=expression(in_allowed) ")" body=statement
     { While_statement (condition, body) }

 (* 14.7.4 The for Statement *)
 | T_FOR "(" i=expression(in_disallowed)? ";" c=expression(in_allowed)? ";" incr=expression(in_allowed)? ")" st=statement
   { For_statement (Left i, c, incr, st) }
 | T_FOR "(" l=forDeclaration ";" c=expression(in_allowed)? ";" incr=expression(in_allowed)? ")" st=statement
   { For_statement (Right l, c, incr, st) }

 (* 14.7.5 The for-in and for-of Statements *)
 | T_FOR "(" left=leftHandSideExpression T_IN right=expression(in_allowed) ")" body=statement
   { let left = assignment_target_of_expr None left in
     ForIn_statement (Left left, right, body) }
 | T_FOR "(" left=forBinding T_IN right=expression(in_allowed) ")" body=statement
   { ForIn_statement (Right left, right, body) }

 | T_FOR "(" left=leftHandSideExpression T_OF right=assignmentExpression(in_allowed) ")" body=statement
    { let left = assignment_target_of_expr None left in
      ForOf_statement (Left left, right, body) }
 | T_FOR "(" left=forBinding T_OF right=assignmentExpression(in_allowed) ")" body=statement
   { ForOf_statement (Right left, right, body) }
 | T_FOR T_AWAIT "(" left=leftHandSideExpression T_OF right=assignmentExpression(in_allowed) ")" body=statement
    { let left = assignment_target_of_expr None left in
      ForAwaitOf_statement (Left left, right, body) }
 | T_FOR T_AWAIT "(" left=forBinding T_OF right=assignmentExpression(in_allowed) ")" body=statement
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
 | T_RETURN e=expression(in_allowed)? sc { (Return_statement (e, p $endpos(e))) }

(*----------------------------*)
(* 14.11 The with Statement *)
(*----------------------------*)

withStatement:
 | T_WITH "(" e=expression(in_allowed) ")" s=statement { (With_statement (e,s)) }

(*----------------------------*)
(* 14.12 The switch Statement *)
(*----------------------------*)

switchStatement:
 | T_SWITCH "(" subject=expression(in_allowed) ")" cb=caseBlock
   { let c1, d, c2 = cb in
     Switch_statement (subject, c1, d, c2)
   }

caseBlock:
 | "{" caseClause* "}" { $2, None, [] }
 | "{" caseClause* defaultClause caseClause* "}" { $2, Some $3, $4 }

caseClause:
 | T_CASE e=expression(in_allowed) ":" s= optl(statementList) { e,s }

defaultClause:
 | T_DEFAULT ":" list=optl(statementList) { list }

(*----------------------------*)
(* 14.13 Labelled Statements *)
(*----------------------------*)

labelIdentifier:
  | identifierName { Label.of_string $1 }

labelledStatement:
 | l=labelIdentifier ":" s=statement { Labelled_statement (l, s)}

(*----------------------------*)
(* 14.14 The throw Statement *)
(*----------------------------*)

throwStatement:
 | T_THROW e=expression(in_allowed) sc { (Throw_statement e) }

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
 | listc(formalParameter) ","?                    { list $1 }
 | r=functionRestParameter                           { { list = []; rest = Some r } }
 | listc(formalParameter) "," r=functionRestParameter { { list = $1; rest = Some r } }

functionRestParameter:
 | "..." singleNameBinding { $2 }

formalParameter:
  | singleNameBinding initializer_(in_allowed)? { $1, $2 }

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
arrowFunction(in_):
  | i=identifier T_ARROW b=conciseBody(in_)
    { let b,consise = b in
      EArrow (({async = false; generator = false}, list [param' i],b, p $symbolstartpos), consise, AUnknown) }
  | T_LPAREN_ARROW a=formalParameters ")" T_ARROW b=conciseBody(in_)
    { let b,consise = b in
      EArrow (({async = false; generator = false}, a,b, p $symbolstartpos), consise, AUnknown) }

conciseBody(in_):
 | "{" b=functionBody "}" { b, false }
 | e=assignmentExpressionForConciseBody(in_) { [(Return_statement (Some e, p $endpos), p $symbolstartpos)], true }

(*----------------------------*)
(* 15.4 Method Definitions *)
(*----------------------------*)

methodName:
 | identifierName    { $1 }
 | identifierKeyword { $1 }

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

yieldExpression(in_):
 | T_YIELD { EYield { delegate = false; expr = None } }
 | T_YIELD e=assignmentExpression(in_) { EYield { delegate = false; expr = Some e } }
 | T_YIELD "*" e=assignmentExpression(in_) { EYield { delegate = true; expr = Some e } }

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

classElement:
 |          m=methodDefinition(classElementName)
    { let n,m = m in [ CEMethod (false, n, m) ] }
 | T_STATIC m=methodDefinition(classElementName)
    { let n,m = m in [ CEMethod (true, n, m) ] }

 |          n=classElementName i=initializer_(in_allowed)? sc
    { [ CEField (false, n, i) ] }
 | T_STATIC n=classElementName i=initializer_(in_allowed)? sc
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

(*----------------------------*)
(* 15.9 Async Arrow Function Definitions *)
(*----------------------------*)

asyncArrowFunction(in_):
  | T_ASYNC i=identifier T_ARROW b=conciseBody(in_) {
      let b,consise = b in
      EArrow(({async = true; generator = false}, list [param' i],b, p $symbolstartpos), consise, AUnknown) }
  | T_ASYNC T_LPAREN_ARROW a=formalParameters ")" T_ARROW b=conciseBody(in_)
    { let b,consise = b in
      EArrow (({async = true; generator = false}, a,b, p $symbolstartpos), consise, AUnknown) }

(*************************************************************************)
(* Section 16: ECMAScript Language: Scripts and Modules                 *)
(*************************************************************************)

(*----------------------------*)
(* 16.1 Scripts *)
(*----------------------------*)

standalone_expression:
 | e=expression(in_allowed) T_EOF { e }

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
 | T_IMPORT kind=importClause from=fromClause wc=withClause? sc
    { let pos = $symbolstartpos in
      Import ({ from; kind; withClause=wc }, pi pos), p pos }
 | T_IMPORT from=moduleSpecifier wc=withClause? sc
    { let pos = $symbolstartpos in
      Import ({ from; kind = SideEffect; withClause=wc }, pi pos), p pos }

withClause:
 | T_WITH "{" "}" { [] }
 | T_WITH "{" l = listc(withEntry) "}"     { l }
 | T_WITH "{" l = listc(withEntry) "," "}" { l }

withEntry:
 | a=T_STRING ":" b=T_STRING { fst a, fst b  }
 | a=identifierName ":" b=T_STRING { a, fst b }
 | a=identifierKeyword ":" b=T_STRING { a, fst b }

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
 | T_EXPORT T_DEFAULT e=assignmentExpression_NoStmt sc
    {
      let k = ExportDefaultExpression e in
      let pos = $symbolstartpos in
      Export (k,pi pos), p pos }
 | T_EXPORT T_DEFAULT e=primaryExpression_Object sc
    {
      let k = ExportDefaultExpression e in
      let pos = $symbolstartpos in
      Export (k,pi pos), p pos }
 | T_EXPORT T_DEFAULT e=primaryExpression_FunClass endrule(sc | T_VIRTUAL_SEMICOLON_EXPORT_DEFAULT { () } )
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
| T_EXPORT "*" from=fromClause wc=withClause? sc {
    let kind = Export_all None in
    let pos = $symbolstartpos in
    Export (ExportFrom ({from; kind;withClause=wc}),pi pos), p pos
  }
 | T_EXPORT "*" T_AS id=moduleExportName from=fromClause wc=withClause? sc {
    let (_,id,_) = id in
    let kind = Export_all (Some id) in
    let pos = $symbolstartpos in
    Export (ExportFrom ({from; kind;withClause=wc}), pi pos), p pos
  }
| T_EXPORT names=exportClause from=fromClause wc=withClause? sc {
    let names = List.map (fun ((_,a,_), (_,b,_)) -> a, b) names in
    let kind = Export_names names in
    let pos = $symbolstartpos in
    Export (ExportFrom ({from; kind; withClause=wc}), pi pos), p pos
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
