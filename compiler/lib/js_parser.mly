(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2010-2014 Facebook
 * Copyright (C) 2019-2022 r2c
 * Copyright (C) 2013-2025 Hugo Heuzard
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

(* Originally adapted from
- https://github.com/facebookarchive/pfff/blob/master/lang_js/parsing/parser_js.mly
- https://github.com/semgrep/semgrep/blob/develop/languages/javascript/menhir/parser_js.mly
The code was later largely rewritten but still contain pieces from its predecessors.
*)

(* The grammar rules are organized to follow the structure of the
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

%{

open Js_token
open Javascript

let var pi name = ident_unsafe ~loc:(pi) name

let pi pos = (Parse_info.t_of_pos pos)

let p pos = Pi (pi pos)

let utf8_s = Stdlib.Utf8_string.of_string_exn

let vartok pos tok =
  EVar (var (p pos) (utf8_s (Js_token.to_string tok)))

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
T_DEFER
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

%token T_YIELDOFF T_YIELDON T_AWAITOFF T_AWAITON T_YIELD_AWAIT_POP



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
  | x=X                     { [x] }
  | xs=listc_rev(X) "," x=X { x :: xs }

%inline listc(X):
  | xs=listc_rev(X) { List.rev xs }

listc_with_empty_trail_rev(X):
  | ","                                      { [ None ] }
  | x=X ","                                  { [ Some x ] }
  | xs=listc_with_empty_trail_rev(X) x=X "," { Some x :: xs }
  | xs=listc_with_empty_trail_rev(X) ","     { None :: xs }

listc_with_empty(X):
  | xs=listc_with_empty_trail_rev(X) x=X?
    {
      match x with
      | None -> List.rev xs
      | Some _ -> List.rev (x :: xs)
    }
  | x=X          { [ Some x ] }
  | (* empty *)  { [] }

listc_with_empty2(X,Y):
  | xs=listc_with_empty_trail_rev(X) x=X { List.rev (Some x :: xs), None }
  | xs=listc_with_empty_trail_rev(X)     { List.rev xs, None }
  | xs=listc_with_empty_trail_rev(X) y=Y { List.rev xs, Some y }
  | x=X                                  { [Some x], None }
  | y=Y                                  { [], Some y }
  | (* empty *)                          { [], None }

empty: { }

(* Guard rules for 'in' operator - used to parameterize expression rules *)
(* in_allowed: empty rule, allows the T_IN production *)
(* in_disallowed: matches T_EOF which never appears mid-expression, effectively disabling the production *)
%inline in_allowed: { }
%inline in_disallowed: T_EOF { }

%inline using_disallowed: T_ERROR T_USING { }
%inline using_allowed: { }

%inline pop: T_YIELD_AWAIT_POP { }
%inline yieldOff: T_YIELDOFF { }
%inline yieldOn: T_YIELDON { }
%inline awaitOff: T_AWAITOFF { }
%inline awaitOn: T_AWAITON { }

(*************************************************************************)
(* Section 12: ECMAScript Language: Lexical Grammar                     *)
(*************************************************************************)

(*----------------------------*)
(* 12.7 Names and Keywords *)
(*----------------------------*)

(* IdentifierName - used for entities, parameters, labels, etc. *)
identifierName:
  | id=T_IDENTIFIER          { fst id }
  | kw=identifierSemiKeyword { utf8_s (Js_token.to_string kw) }

identifier:
  | name=identifierName { var (p $symbolstartpos) name }

(* add here keywords which are not considered reserved by ECMA *)
identifierSemiKeyword:
  | T_AS { T_AS }
  | T_ASYNC { T_ASYNC }
  | T_FROM { T_FROM }
  | T_GET { T_GET }
  | T_META { T_META }
  | T_OF { T_OF }
  | T_SET { T_SET }
  | T_TARGET {T_TARGET }

  | T_USING { T_USING }
  | T_DEFER { T_DEFER }

(* Variant excluding T_OF used for 'using' bindings in for-in/of loops
   to resolve ambiguity in 'for (using of ...)' *)
identifierSemiKeywordNoOf:
  | T_AS { T_AS }
  | T_ASYNC { T_ASYNC }
  | T_FROM { T_FROM }
  | T_GET { T_GET }
  | T_META { T_META }
  (* T_OF intentionally omitted to resolve 'for (using of ...)' ambiguity *)
  | T_SET { T_SET }
  | T_TARGET {T_TARGET }
  | T_USING { T_USING }
  | T_DEFER { T_DEFER }

identifierNameNoOf:
  | id=T_IDENTIFIER              { fst id }
  | kw=identifierSemiKeywordNoOf { utf8_s (Js_token.to_string kw) }

identifierNoOf:
  | name=identifierNameNoOf { var (p $symbolstartpos) name }

identifierKeyword:
  | kw=identifierKeywordToken { utf8_s (Js_token.to_string kw) }

identifierKeywordToken:
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
  (* *)
  | T_AWAIT { T_AWAIT }
  | T_YIELD { T_YIELD }
  (* reserved words in strict mode code. *)
  | T_LET { T_LET }
  | T_STATIC { T_STATIC }
  | T_IMPLEMENTS { T_IMPLEMENTS }
  | T_INTERFACE { T_INTERFACE }
  | T_PACKAGE { T_PACKAGE }
  | T_PRIVATE { T_PRIVATE }
  | T_PROTECTED { T_PROTECTED }
  | T_PUBLIC { T_PUBLIC }

(*----------------------------*)
(* 12.9.1 Null Literals *)
(*----------------------------*)

nullLiteral:
  | T_NULL { (vartok $symbolstartpos T_NULL) }

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
  | n=T_NUMBER { let _, raw = n in raw }

bigIntLiteral:
  | n=T_BIGINT { let _, raw = n in raw }

(*----------------------------*)
(* 12.9.4 String Literals *)
(*----------------------------*)

stringLiteral: s=T_STRING { (EStr (fst s)) }

(*----------------------------*)
(* 12.9.5 Regular Expression Literals *)
(*----------------------------*)

regularExpressionLiteral:
  | r=T_REGEXP
    {
      let (Utf8 s, f) = r in
      (ERegexp (s, if String.equal f "" then None else Some f))
    }

(*----------------------------*)
(* 12.9.6 Template Literal Lexical Components *)
(*----------------------------*)

templateLiteral: T_BACKQUOTE spans=templateSpan* T_BACKQUOTE  { spans }

templateSpan:
  | s=T_ENCAPSED_STRING                        { TStr (utf8_s s) }
  | T_DOLLARCURLY e=expression(in_allowed) "}" { TExp e }

(*************************************************************************)
(* Section 13: ECMAScript Language: Expressions                         *)
(*************************************************************************)

(*----------------------------*)
(* 13.1 Identifiers *)
(*----------------------------*)

bindingIdentifier: id=identifier { id }

(*----------------------------*)
(* 13.2 Primary Expression *)
(*----------------------------*)

primaryExpression(x):
  | e=primaryExpressionNoBraces
  | e=x { e }

d1:
  | e=primaryExpression_FunClass { e }
  | e=primaryExpression_Object   { e }

primaryExpression_Object:
  | e=objectLiteral { e }

primaryExpression_Empty: T_ERROR TComment { assert false }

primaryExpression_FunClass:
  | e=functionExpression       { e }
  | e=classExpression          { e }
  | e=generatorExpression      { e }
  | e=asyncFunctionExpression  { e }
  | e=asyncGeneratorExpression { e }

primaryExpressionNoBraces:
  | T_THIS                      { vartok $symbolstartpos T_THIS }
  | i=identifier                { EVar i }
  | T_POUND name=identifierName { EPrivName name }
  | n=nullLiteral               { n }
  | b=booleanLiteral            { b }
  | n=numericLiteral            { ENum (Num.of_string_unsafe n) }
  | n=bigIntLiteral             { ENum (Num.of_string_unsafe n) }
  | s=stringLiteral             { s }
  | t=templateLiteral           { ETemplate t }
  | r=regularExpressionLiteral  { r }
  | a=arrayLiteral              { a }
  | e=coverParenthesizedExpressionAndArrowParameterList { e }

coverParenthesizedExpressionAndArrowParameterList:
  | "(" e=expression(in_allowed) ","? ")"  { e }
  | "(" _rparen=")"                        { CoverParenthesizedExpressionAndArrowParameterList (early_error (pi $startpos(_rparen))) }
  | "(" _ellipsis="..." bindingElement ")" { CoverParenthesizedExpressionAndArrowParameterList (early_error (pi $startpos(_ellipsis)) ) }
  | "(" expression(in_allowed) "," _ellipsis="..." bindingElement ")"
    { CoverParenthesizedExpressionAndArrowParameterList (early_error (pi $startpos(_ellipsis)) ) }

(*----------------------------*)
(* 13.2.4 Array Initializer *)
(*----------------------------*)

arrayLiteral:
  | "[" l=listc_with_empty (arrayElement) "]"
    { (EArr (List.map (function None -> ElementHole | Some x -> x) l)) }

arrayElement:
  | e=assignmentExpression(in_allowed)       { Element e }
  (* SpreadElement *)
  | "..." e=assignmentExpression(in_allowed) { ElementSpread e }

(*----------------------------*)
(* 13.2.5 Object Initializer *)
(*----------------------------*)

propertyName:
  | i=identifierName    { PNI i }
  | i=identifierKeyword { PNI i }
  | s=T_STRING          { let s, _len = s in PNS s }
  | n=numericLiteral    { PNN (Num.of_string_unsafe (n)) }
  | n=bigIntLiteral     { PNN (Num.of_string_unsafe (n)) }
  | "[" p=assignmentExpression(in_allowed) "]" { PComputed p }

objectLiteral:
  | "{" "}"                                      { EObj [] }
  | "{" props=listc(propertyDefinition) ","? "}" { EObj props }

propertyDefinition:
  | name=propertyName ":" value=assignmentExpression(in_allowed)
    { Property (name, value) }
  (* shorthand property *)
  | i=identifierName
    { Property (PNI i, EVar (ident_unsafe i)) }
  | id=identifier init=initializer_(in_allowed)
    { CoverInitializedName (early_error (pi $startpos(init)), id, init)  }
  (* spread property *)
  | "..." e=assignmentExpression(in_allowed)
    { PropertySpread(e) }
  | m=methodDefinition(propertyName)
    { let n, m = m in PropertyMethod(n,m) }

(*----------------------------*)
(* 13.3 Left-Hand-Side Expressions *)
(*----------------------------*)

leftHandSideExpression: e=leftHandSideExpression_(d1) { e }

leftHandSideExpression_(x):
  | e=newExpression(x)      { e }
  | e=callExpression(x)     { e }
  | e=optionalExpression(x) { e }

(*----------------------------*)
(* 13.3.2 Property Accessors *)
(*----------------------------*)

fieldName:
  | name=identifierName  { name }
  | kw=identifierKeyword { kw }

memberExpression(x):
  | e=primaryExpression(x)
    { e }
  | _import=T_IMPORT "." i=fieldName
    { EDot (vartok $startpos(_import) T_IMPORT,ANormal,i) }
  | e1=memberExpression(x) "[" e2=expression(in_allowed) "]"
    { (EAccess (e1,ANormal, e2)) }
  | e1=memberExpression(x) "." i=fieldName
    { (EDot(e1,ANormal,i)) }
  | T_NEW e1=memberExpression(d1) a=arguments
    { (ENew(e1, Some a, p $symbolstartpos)) }
  | e=memberExpression(x) t=templateLiteral
    { ECallTemplate(e, t, p $symbolstartpos) }
  | _super=T_SUPER "[" e=expression(in_allowed) "]"
    { (EAccess (vartok $startpos(_super) T_SUPER,ANormal, e)) }
  | _super=T_SUPER "." i=fieldName
    { (EDot(vartok $startpos(_super) T_SUPER,ANormal,i)) }
  | _new=T_NEW "." T_TARGET
    { (EDot(vartok $startpos(_new) T_NEW,ANormal,utf8_s "target")) }
  | e1=memberExpression(x) "." T_POUND i=fieldName
    { (EDotPrivate(e1,ANormal,i)) }

(*----------------------------*)
(* 13.3.5 The new Operator *)
(*----------------------------*)

newExpression(x):
  | e=memberExpression(x)     { e }
  | T_NEW e=newExpression(d1) { (ENew (e,None, p $symbolstartpos)) }

(*----------------------------*)
(* 13.3.6 Function Calls *)
(*----------------------------*)

callExpression(x):
  | _import=T_IMPORT a=arguments
    { (ECall(vartok $startpos(_import) T_IMPORT, ANormal, a, p $symbolstartpos)) }
  | e=memberExpression(x) a=arguments
    { (ECall(e, ANormal, a, p $symbolstartpos)) }
  | e=callExpression(x) a=arguments
    { (ECall(e, ANormal, a, p $symbolstartpos)) }
  | e=callExpression(x) "[" e2=expression(in_allowed) "]"
    { (EAccess (e, ANormal,  e2)) }
  | e=callExpression(x) t=templateLiteral
    { ECallTemplate(e, t,p $symbolstartpos) }
  | _super=T_SUPER a=arguments { ECall(vartok $startpos(_super) T_SUPER,ANormal, a, p $symbolstartpos) }
  | e=callExpression(x) "." i=methodName
    { EDot (e,ANormal,i) }
  | e=callExpression(x) "." T_POUND i=methodName
    { EDotPrivate (e,ANormal,i) }

(*----------------------------*)
(* 13.3.8 Argument Lists *)
(*----------------------------*)

arguments:
  | "(" args=argumentList ")" { args }

argumentList:
  | (* empty *)                           { [] }
  | args=listc(argumentListElement) ","?  { args  }

(* assignmentExpression because expression supports sequence of exprs with ',' *)
argumentListElement:
  | e=assignmentExpression(in_allowed)       { Arg e }
  (* spread element, allowed not only in last position *)
  | "..." e=assignmentExpression(in_allowed) { ArgSpread e }

(*----------------------------*)
(* 13.3.9 Optional Chains *)
(*----------------------------*)

optionalExpression(x):
  | e=memberExpression(x) c=optionalChain   { c e }
  | e=callExpression(x) c=optionalChain     { c e }
  | e=optionalExpression(x) c=optionalChain { c e }

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
  | e=leftHandSideExpression_(x)           { e }
  | e=leftHandSideExpression_(x) T_INCR_NB { EUn (IncrA, e) }
  | e=leftHandSideExpression_(x) T_DECR_NB { EUn (DecrA, e) }
  | T_INCR e=unaryExpression(d1)           { EUn (IncrB, e) }
  | T_DECR e=unaryExpression(d1)           { EUn (DecrB, e) }
  | T_INCR_NB e=unaryExpression(d1)        { EUn (IncrB, e) }
  | T_DECR_NB e=unaryExpression(d1)        { EUn (DecrB, e) }

(*----------------------------*)
(* 13.5 Unary Operators *)
(*----------------------------*)

unaryExpression(x):
  | e=updateExpression(x)           { e }
  | T_DELETE e=unaryExpression(d1)  { EUn (Delete, e) }
  | T_VOID e=unaryExpression(d1)    { EUn (Void, e) }
  | T_TYPEOF e=unaryExpression(d1)  { EUn (Typeof, e) }
  | T_PLUS e=unaryExpression(d1)    { EUn (Pl, e) }
  | T_MINUS e=unaryExpression(d1)   { EUn (Neg, e)}
  | T_BIT_NOT e=unaryExpression(d1) { EUn (Bnot, e) }
  | T_NOT e=unaryExpression(d1)     { EUn (Not, e) }
  | T_AWAIT e=unaryExpression(d1)   { EUn (Await, e) }

(*----------------------------*)
(* 13.6 Exponentiation Operator *)
(*----------------------------*)

(* Note: ** is right-associative *)
exponentiationExpression(x):
  | e=unaryExpression(x)                                         { e }
  | e1=updateExpression(x) T_EXP e2=exponentiationExpression(d1) { EBin(Exp, e1, e2) }

(*----------------------------*)
(* 13.7 Multiplicative Operators *)
(*----------------------------*)

multiplicativeExpression(x):
  | e=exponentiationExpression(x)                                        { e }
  | e1=multiplicativeExpression(x) "*" e2=exponentiationExpression(d1)   { EBin(Mul, e1, e2) }
  | e1=multiplicativeExpression(x) T_DIV e2=exponentiationExpression(d1) { EBin(Div, e1, e2) }
  | e1=multiplicativeExpression(x) T_MOD e2=exponentiationExpression(d1) { EBin(Mod, e1, e2) }

(*----------------------------*)
(* 13.8 Additive Operators *)
(*----------------------------*)

additiveExpression(x):
  | e=multiplicativeExpression(x)                                    { e }
  | e1=additiveExpression(x) T_PLUS e2=multiplicativeExpression(d1)  { EBin(Plus, e1, e2) }
  | e1=additiveExpression(x) T_MINUS e2=multiplicativeExpression(d1) { EBin(Minus, e1, e2) }

(*----------------------------*)
(* 13.9 Bitwise Shift Operators *)
(*----------------------------*)

shiftExpression(x):
  | e=additiveExpression(x)                                   { e }
  | e1=shiftExpression(x) T_LSHIFT e2=additiveExpression(d1)  { EBin(Lsl, e1, e2) }
  | e1=shiftExpression(x) T_RSHIFT e2=additiveExpression(d1)  { EBin(Asr, e1, e2) }
  | e1=shiftExpression(x) T_RSHIFT3 e2=additiveExpression(d1) { EBin(Lsr, e1, e2) }

(*----------------------------*)
(* 13.10 Relational Operators *)
(*----------------------------*)

(* Parameterized by in_ which is either in_allowed or in_disallowed.
   The in_ prefix on the T_IN rule acts as a guard:
   - in_allowed is empty, so the T_IN production is active
   - in_disallowed matches T_EOF (impossible here), so the production is disabled *)
relationalExpression(x, in_):
  | e=shiftExpression(x)                                                        { e }
  | e1=relationalExpression(x, in_) T_LESS_THAN e2=shiftExpression(d1)          { EBin(Lt, e1, e2) }
  | e1=relationalExpression(x, in_) T_GREATER_THAN e2=shiftExpression(d1)       { EBin(Gt, e1, e2) }
  | e1=relationalExpression(x, in_) T_LESS_THAN_EQUAL e2=shiftExpression(d1)    { EBin(Le, e1, e2) }
  | e1=relationalExpression(x, in_) T_GREATER_THAN_EQUAL e2=shiftExpression(d1) { EBin(Ge, e1, e2) }
  | e1=relationalExpression(x, in_) T_INSTANCEOF e2=shiftExpression(d1)         { EBin (InstanceOf, e1, e2) }
  | in_ e1=relationalExpression(x, in_) T_IN e2=shiftExpression(d1)             { EBin (In, e1, e2) }

(*----------------------------*)
(* 13.11 Equality Operators *)
(*----------------------------*)

equalityExpression(x, in_):
  | e=relationalExpression(x, in_)                                                    { e }
  | e1=equalityExpression(x, in_) T_EQUAL e2=relationalExpression(d1, in_)            { EBin(EqEq, e1, e2) }
  | e1=equalityExpression(x, in_) T_NOT_EQUAL e2=relationalExpression(d1, in_)        { EBin(NotEq, e1, e2) }
  | e1=equalityExpression(x, in_) T_STRICT_EQUAL e2=relationalExpression(d1, in_)     { EBin(EqEqEq, e1, e2) }
  | e1=equalityExpression(x, in_) T_STRICT_NOT_EQUAL e2=relationalExpression(d1, in_) { EBin(NotEqEq, e1, e2) }

(*----------------------------*)
(* 13.12 Binary Bitwise Operators *)
(*----------------------------*)

bitwiseANDExpression(x, in_):
  | e=equalityExpression(x, in_)                                             { e }
  | e1=bitwiseANDExpression(x, in_) T_BIT_AND e2=equalityExpression(d1, in_) { EBin(Band, e1, e2) }

bitwiseXORExpression(x, in_):
  | e=bitwiseANDExpression(x, in_)                                             { e }
  | e1=bitwiseXORExpression(x, in_) T_BIT_XOR e2=bitwiseANDExpression(d1, in_) { EBin(Bxor, e1, e2) }

bitwiseORExpression(x, in_):
  | e=bitwiseXORExpression(x, in_)                                           { e }
  | e1=bitwiseORExpression(x, in_) T_BIT_OR e2=bitwiseXORExpression(d1, in_) { EBin(Bor, e1, e2) }

(*----------------------------*)
(* 13.13 Binary Logical Operators *)
(*----------------------------*)

logicalANDExpression(x, in_):
  | e=bitwiseORExpression(x, in_)                                         { e }
  | e1=logicalANDExpression(x, in_) T_AND e2=bitwiseORExpression(d1, in_) { EBin(And, e1, e2) }

logicalORExpression(x, in_):
  | e=logicalANDExpression(x, in_)                                       { e }
  | e1=logicalORExpression(x, in_) T_OR e2=logicalANDExpression(d1, in_) { EBin(Or, e1, e2) }

(* Coalesce expression - can't mix with || or && without parens *)
coalesceExpression(x, in_):
  | e1=coalesceExpressionHead(x, in_) T_PLING_PLING e2=bitwiseORExpression(d1, in_)  { EBin(Coalesce, e1, e2) }

coalesceExpressionHead(x, in_):
  | e=coalesceExpression(x, in_)  { e }
  | e=bitwiseORExpression(x, in_) { e }

(* ShortCircuitExpression: either logical OR chain or coalesce chain *)
shortCircuitExpression(x, in_):
  | e=logicalORExpression(x, in_) { e }
  | e=coalesceExpression(x, in_)  { e }

(*----------------------------*)
(* 13.14 Conditional Operator ( ? : ) *)
(*----------------------------*)

conditionalExpression(x, in_):
  | e=shortCircuitExpression(x, in_)
    { e }
  | cond=shortCircuitExpression(x, in_) "?" then_=assignmentExpression(in_allowed) ":" else_=assignmentExpression(in_)
    { ECond (cond, then_, else_) }

(*----------------------------*)
(* 13.15 Assignment Operators *)
(*----------------------------*)

assignmentExpression(in_):
  | e=conditionalExpression(d1, in_) { e }
  | e1=leftHandSideExpression_(d1) op=assignmentOperator e2=assignmentExpression(in_)
    {
      let e1 = assignment_target_of_expr (Some op) e1 in
      EBin (op, e1, e2)
    }
  | e=arrowFunction(in_)      { e }
  | e=asyncArrowFunction(in_) { e }
  | e=yieldExpression(in_)    { e }

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
  | e=assignmentExpression(in_)                         { e }
  | e1=expression(in_) "," e2=assignmentExpression(in_) { ESeq (e1, e2) }

(*----------------------------*)
(* Expression variants (no statement-like constructs) *)
(*----------------------------*)

expressionNoStmt:
  | e=assignmentExpression_NoStmt                               { e }
  | e1=expressionNoStmt "," e2=assignmentExpression(in_allowed) { ESeq (e1, e2) }

assignmentExpression_NoStmt:
  | e=conditionalExpression(primaryExpression_Empty, in_allowed) { e }
  | e1=leftHandSideExpression_(primaryExpression_Empty) op=assignmentOperator e2=assignmentExpression(in_allowed)
    {
      let e1 = assignment_target_of_expr (Some op) e1 in
      EBin (op, e1, e2)
    }
  | e=arrowFunction(in_allowed)      { e }
  | e=asyncArrowFunction(in_allowed) { e }
  | e=yieldExpression(in_allowed)    { e }

(*----------------------------*)
(* Expression variants (for concise arrow body) *)
(*----------------------------*)

assignmentExpressionForConciseBody(in_):
  | e=conditionalExpression(primaryExpression_FunClass, in_) { e, $endpos }
  | e1=leftHandSideExpression_(primaryExpression_FunClass) op=assignmentOperator e2=assignmentExpression(in_)
    {
      let e1 = assignment_target_of_expr (Some op) e1 in
      EBin (op, e1, e2), $endpos
    }
  | e=arrowFunction(in_)      { e, $endpos }
  | e=asyncArrowFunction(in_) { e, $endpos }
  | e=yieldExpression(in_)    { e, $endpos }

(*************************************************************************)
(* Section 14: ECMAScript Language: Statements and Declarations         *)
(*************************************************************************)

(*----------------------------*)
(* 14.1 Statement Semantics *)
(*----------------------------*)

%inline
statement: s=statementBody { s, p $symbolstartpos }

statementBody:
  | b=block               { Block b }
  | s=variableStatement   { s }
  | s=emptyStatement      { s }
  | s=expressionStatement { s }
  | s=ifStatement         { s }
  | s=iterationStatement  { s }
  | s=continueStatement   { s }
  | s=breakStatement      { s }
  | s=returnStatement     { s }
  | s=labelledStatement   { s }
  | s=switchStatement     { s }
  | s=throwStatement      { s }
  | s=tryStatement        { s }
  | s=withStatement       { s }
  | s=debuggerStatement   { s }

statementListItem:
  | s=statement   { s }
  | d=declaration { d }

declaration:
  | d=functionDeclaration
    { let i,f = d in Function_declaration (i,f), p $symbolstartpos }
  | d=generatorDeclaration
    { let i,f = d in Function_declaration (i,f), p $symbolstartpos }
  | d=asyncGeneratorDeclaration
    { let i,f = d in Function_declaration (i,f), p $symbolstartpos }
  | d=asyncFunctionDeclaration
    { let i,f = d in Function_declaration (i,f), p $symbolstartpos }
  | d=lexicalDeclaration(in_allowed)
    { let k,l = d in Variable_statement (k, l), p $symbolstartpos }
  | d=classDeclaration
    { let i,f = d in Class_declaration (i,f), p $symbolstartpos }

(*----------------------------*)
(* 14.2 Block *)
(*----------------------------*)

block: "{" l=statementList "}" { l }

statementList_noneempty: l=statementListItem+ { l }
statementList:
  | l=statementList_noneempty { l }
  | { [ ] }

(*----------------------------*)
(* 14.3 Declarations and the Variable Statement *)
(*----------------------------*)

(* 14.3.1 Let and Const Declarations *)
lexicalDeclaration(in_):
  | T_CONST l=listc(lexicalBinding(in_)) sc       { (Const, l)}
  | T_LET l=listc(lexicalBinding(in_)) sc         { (Let, l)}
 (* Explicit Resource Management *)
  | T_USING l=listc(lexicalBinding_using(in_)) sc         { (Using, l)}
  | T_AWAIT T_USING l=listc(lexicalBinding_using(in_)) sc { (AwaitUsing, l)}

(* 14.3.2 Variable Statement *)
variableStatement:
  | T_VAR l=listc(variableDeclaration(in_allowed)) sc { Variable_statement (Var, l) }

variableDeclaration(in_):
  | i=identifier e=initializer_(in_)?    { DeclIdent (i,e) }
  | p=bindingPattern e=initializer_(in_) { DeclPattern (p, e) }

lexicalBinding(in_):
  | i=identifier e=initializer_(in_)?    { DeclIdent (i,e) }
  | p=bindingPattern e=initializer_(in_) { DeclPattern (p, e) }

lexicalBinding_using(in_):
  | i=identifierNoOf e=initializer_(in_)?    { DeclIdent (i,e) }

initializer_(in_):
  | "=" e=assignmentExpression(in_) { e, p $symbolstartpos }

forDeclaration(using_):
  | T_CONST l=forBinding                   { Const, l }
  | T_LET l=forBinding                     { Let, l }
  (* Explicit Resource Management - uses restricted binding to resolve
    'for (using of ...)' ambiguity: identifier cannot be 'of' *)
  | using_ T_USING l=forBinding_using         { Using, l }
  | using_ T_AWAIT T_USING l=forBinding_using { AwaitUsing, l }

(* 'for ... in' and 'for ... of' declare only one variable *)
forBinding:
  | p=bindingPattern { BindingPattern p }
  | id=identifier    { BindingIdent id }

forBinding_using:
  | id=identifierNoOf { BindingIdent id }

(*----------------------------*)
(* 14.3.3 Destructuring Binding Patterns *)
(*----------------------------*)

bindingPattern:
  | p=objectBindingPattern { p }
  | p=arrayBindingPattern  { p }

singleNameBinding:
  | p=bindingPattern { BindingPattern p }
  | id=identifier    { BindingIdent id }

objectBindingPattern:
  | "{" "}"                               { ObjectBinding (list []) }
  | "{" r=bindingRestProperty "}"         { ObjectBinding {list = []; rest = Some r } }
  | "{" l=listc(bindingProperty) ","? "}" { ObjectBinding (list l) }
  | "{" l=listc(bindingProperty) "," r=bindingRestProperty "}"
    { ObjectBinding {list=l;rest= Some r} }

bindingProperty:
  | i=identifier e=initializer_(in_allowed)? { Prop_ident (Prop_and_ident i, e) }
  | pn=propertyName ":" e=bindingElement     { Prop_binding (pn, e) }

bindingRestProperty:
  (* can appear only at the end of a bindingPropertyList in ECMA *)
  | "..." id=identifier { id }

(* in theory used also for formal parameter as is *)
bindingElement:
  | b=singleNameBinding e=initializer_(in_allowed)? { b, e }

(* array destructuring *)

arrayBindingPattern:
  | "[" l=listc_with_empty2(bindingElement, bindingRestElement) "]"
    { ArrayBinding {list = fst l; rest = snd l } }

bindingRestElement:
  (* can appear only at the end of a arrayBindingPattern in ECMA *)
  | "..." b=singleNameBinding { b }

(*----------------------------*)
(* 14.4 Empty Statement *)
(*----------------------------*)

emptyStatement:
  | T_SEMICOLON { Empty_statement }

(*----------------------------*)
(* 14.5 Expression Statement *)
(*----------------------------*)

expressionStatement:
  | e=expressionNoStmt sc { Expression_statement e }

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
  | x = doWhileStatement { x }
  | x = whileStatement { x }
  | x = forStatement { x }
  | x = forInOfStatement { x }

(* 14.7.2 The do-while Statement *)
doWhileStatement:
  | T_DO body=statement T_WHILE "(" condition=expression(in_allowed) ")" endrule(sc | T_VIRTUAL_SEMICOLON_DO_WHILE { () } )
    { Do_while_statement (body, condition) }

(* 14.7.3 The while Statement *)
whileStatement:
  | T_WHILE "(" condition=expression(in_allowed) ")" body=statement
    { While_statement (condition, body) }

(* 14.7.4 The for Statement *)
forStatement:
  | T_FOR "(" i=expression(in_disallowed)? ";" c=expression(in_allowed)? ";" incr=expression(in_allowed)? ")" st=statement
    { For_statement (Left i, c, incr, st) }
  | T_FOR "(" T_VAR l=listc(variableDeclaration(in_disallowed) ) ";" c=expression(in_allowed)? ";" incr=expression(in_allowed)? ")" st=statement
    { For_statement (Right (Var, l), c, incr, st) }
  | T_FOR "(" l=lexicalDeclaration(in_disallowed) c=expression(in_allowed)? ";" incr=expression(in_allowed)? ")" st=statement
    { let m,l = l in
      For_statement (Right (m,l), c, incr, st)
    }

(* 14.7.5 The for-in and for-of Statements *)
forInOfStatement:
  | T_FOR "(" left=leftHandSideExpression T_IN right=expression(in_allowed) ")" body=statement
    { let left = assignment_target_of_expr None left in
      ForIn_statement (Left left, right, body) }
  | T_FOR "(" T_VAR left=forBinding T_IN right=expression(in_allowed) ")" body=statement
    { ForIn_statement (Right (Var,left), right, body) }
  | T_FOR "(" left=forDeclaration(using_disallowed) T_IN right=expression(in_allowed) ")" body=statement
    { ForIn_statement (Right left, right, body) }

  | T_FOR "(" left=leftHandSideExpression T_OF right=assignmentExpression(in_allowed) ")" body=statement
    { let left = assignment_target_of_expr None left in
      ForOf_statement (Left left, right, body) }
  | T_FOR "(" T_VAR left=forBinding T_OF right=assignmentExpression(in_allowed) ")" body=statement
    { ForOf_statement (Right (Var, left), right, body) }
  | T_FOR "(" left=forDeclaration(using_allowed) T_OF right=assignmentExpression(in_allowed) ")" body=statement
    { ForOf_statement (Right left, right, body) }
  | T_FOR T_AWAIT "(" left=leftHandSideExpression T_OF right=assignmentExpression(in_allowed) ")" body=statement
    { let left = assignment_target_of_expr None left in
      ForAwaitOf_statement (Left left, right, body) }
  | T_FOR T_AWAIT "(" T_VAR left=forBinding T_OF right=assignmentExpression(in_allowed) ")" body=statement
    { ForAwaitOf_statement (Right (Var, left), right, body) }
  | T_FOR T_AWAIT "(" left=forDeclaration(using_allowed) T_OF right=assignmentExpression(in_allowed) ")" body=statement
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
  | "{" cases=caseClause* "}" { cases, None, [] }
  | "{" before=caseClause* default=defaultClause after=caseClause* "}" { before, Some default, after }

caseClause:
  | T_CASE e=expression(in_allowed) ":" s=statementList { e,s }

defaultClause:
  | T_DEFAULT ":" list=statementList { list }

(*----------------------------*)
(* 14.13 Labelled Statements *)
(*----------------------------*)

labelIdentifier:
  | name=identifierName { Label.of_string name }

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
  | T_TRY b=block c=catch           { (Try_statement (b, Some c, None)) }
  | T_TRY b=block         f=finally { (Try_statement (b, None, Some f)) }
  | T_TRY b=block c=catch f=finally { (Try_statement (b, Some c, Some f)) }

catch:
  | T_CATCH "(" p=formalParameter ")" b=block { Some p,b }
  | T_CATCH b=block                           { None,b }

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
  | (* empty *)                                               { list [] }
  | params=listc(formalParameter) ","?                        { list params }
  | r=functionRestParameter                                   { { list = []; rest = Some r } }
  | params=listc(formalParameter) "," r=functionRestParameter { { list = params; rest = Some r } }

functionRestParameter:
  | "..." b=singleNameBinding { b }

formalParameter:
  | b=singleNameBinding init=initializer_(in_allowed)? { b, init }

callSignature(y,a): "(" y a args=formalParameters ")" { args }

functionBody:
  | { [] }
  | body=statementList_noneempty { body }

(*----------------------------*)
(* 15.2 Function Definitions *)
(*----------------------------*)

functionDeclaration:
  | T_FUNCTION name=identifier args=callSignature(yieldOff,awaitOff) "{" b=functionBody pop pop _rbrace="}"
    { (name, ({async = false; generator = false}, args, b, p $startpos(_rbrace))) }

functionExpression:
  | T_FUNCTION yieldOff awaitOff name=identifier? args=callSignature(empty, empty) "{" b=functionBody pop pop "}"
    { EFun (name, ({async = false; generator = false}, args, b, p $symbolstartpos)) }

(*----------------------------*)
(* 15.3 Arrow Function Definitions *)
(*----------------------------*)

arrowFunction(in_):
  | i=identifier T_ARROW yieldOff awaitOff b=conciseBody(in_)
    { let b,consise = b in
      EArrow (({async = false; generator = false}, list [param' i],b, p $symbolstartpos), consise, AUnknown) }
  | T_LPAREN_ARROW a=formalParameters ")" T_ARROW yieldOff awaitOff b=conciseBody(in_)
    { let b,consise = b in
      EArrow (({async = false; generator = false}, a,b, p $symbolstartpos), consise, AUnknown) }

conciseBody( in_):
  | "{" b=functionBody pop pop "}" { b, false }
  | e=assignmentExpressionForConciseBody(in_) pop pop
    {
      let e, stop = e in
      [(Return_statement (Some e, p stop), p $symbolstartpos)], true
    }

(*----------------------------*)
(* 15.4 Method Definitions *)
(*----------------------------*)

methodName:
  | name=identifierName  { name }
  | kw=identifierKeyword { kw }

methodDefinition(name):
  | T_GET name=name args=callSignature(yieldOff, awaitOff) "{" b=functionBody pop pop "}"
    { name, MethodGet(({async = false; generator = false}, args, b, p $symbolstartpos)) }
  | T_SET name=name args=callSignature(yieldOff, awaitOff) "{" b=functionBody pop pop "}"
    { name, MethodSet(({async = false; generator = false}, args, b, p $symbolstartpos)) }
  | name=name args=callSignature(yieldOff, awaitOff)"{" b=functionBody pop pop "}"
    { name, Method(({async = false; generator = false}, args, b, p $symbolstartpos)) }
  | T_ASYNC name=name args=callSignature(yieldOff, awaitOn) "{" b=functionBody pop pop "}"
    { name, Method(({async = true; generator = false}, args, b, p $symbolstartpos)) }
  | "*" name=name args=callSignature(yieldOn, awaitOff) "{" b=functionBody pop pop "}"
    { name, Method(({async = false; generator = true}, args, b, p $symbolstartpos)) }
  | T_ASYNC "*" name=name args=callSignature(yieldOn, awaitOn) "{" b=functionBody pop pop "}"
    { name, Method(({async = true; generator = true}, args, b, p $symbolstartpos)) }

(*----------------------------*)
(* 15.5 Generator Function Definitions *)
(*----------------------------*)

generatorDeclaration:
  | T_FUNCTION "*" name=identifier args=callSignature(yieldOn, awaitOff) "{" b=functionBody pop pop "}"
    { (name, ({async = false; generator = true}, args, b, p $symbolstartpos)) }

generatorExpression:
  | T_FUNCTION "*" yieldOn awaitOff name=identifier? args=callSignature(empty,empty) "{" b=functionBody pop pop "}"
    { EFun (name, ({async = false; generator = true}, args, b, p $symbolstartpos)) }

yieldExpression(in_):
  | T_YIELD { EYield { delegate = false; expr = None } }
  | T_YIELD e=assignmentExpression(in_) { EYield { delegate = false; expr = Some e } }
  | T_YIELD "*" e=assignmentExpression(in_) { EYield { delegate = true; expr = Some e } }

(*----------------------------*)
(* 15.6 Async Generator Function Definitions *)
(*----------------------------*)

asyncGeneratorDeclaration:
  | T_ASYNC T_FUNCTION "*" name=identifier args=callSignature(yieldOn, awaitOn) "{" b=functionBody pop pop "}"
    { (name, ({async = true; generator = true}, args, b, p $symbolstartpos)) }

asyncGeneratorExpression:
  | T_ASYNC T_FUNCTION "*" yieldOn awaitOn name=identifier? args=callSignature(empty, empty) "{" b=functionBody pop pop "}"
    { EFun (name, ({async = true; generator = true}, args, b, p $symbolstartpos)) }

(*----------------------------*)
(* 15.7 Class Definitions *)
(*----------------------------*)

classDeclaration: T_CLASS id=bindingIdentifier extends=classHeritage? body=classBody
    { id, {extends; body}  }

classExpression: T_CLASS i=bindingIdentifier? extends=classHeritage? body=classBody
    { EClass (i, {extends; body}) }

classHeritage: T_EXTENDS e=leftHandSideExpression { e }

classBody: "{" elements=classElement* "}" { List.flatten elements }

classElement:
  | m=methodDefinition(classElementName)
    { let n,m = m in [ CEMethod (false, n, m) ] }
  | T_STATIC m=methodDefinition(classElementName)
    { let n,m = m in [ CEMethod (true, n, m) ] }
  | n=classElementName i=initializer_(in_allowed)? sc
    { [ CEField (false, n, i) ] }
  | T_STATIC n=classElementName i=initializer_(in_allowed)? sc
    { [ CEField (true, n, i) ] }
  | T_STATIC "{" yieldOff awaitOn b=functionBody pop pop "}" { [CEStaticBLock b] }
  | sc               { [] }

classElementName:
  | name=propertyName { PropName name }
  | T_POUND name=identifierName { PrivName name }

(*----------------------------*)
(* 15.8 Async Function Definitions *)
(*----------------------------*)

asyncFunctionDeclaration:
  | T_ASYNC T_FUNCTION  name=identifier args=callSignature(yieldOff, awaitOn) "{" b=functionBody pop pop "}"
    { (name, ({async = true; generator = false}, args, b, p $symbolstartpos)) }

asyncFunctionExpression:
  | T_ASYNC T_FUNCTION yieldOff awaitOn name=identifier? args=callSignature(empty, empty) "{" b=functionBody pop pop "}"
   { EFun (name, ({async = true; generator = false}, args, b, p $symbolstartpos)) }

(*----------------------------*)
(* 15.9 Async Arrow Function Definitions *)
(*----------------------------*)

asyncArrowFunction(in_):
  (* Use identifierNoOf to resolve 'for (async of ...)' ambiguity *)
  | T_ASYNC yieldOff awaitOn i=identifierNoOf T_ARROW b=conciseBody(in_)
    {
      let b,consise = b in
      EArrow(({async = true; generator = false}, list [param' i],b, p $symbolstartpos), consise, AUnknown)
    }
  | T_ASYNC T_LPAREN_ARROW yieldOff awaitOn a=formalParameters ")" T_ARROW b=conciseBody(in_)
    { let b,consise = b in
      EArrow (({async = true; generator = false}, a,b, p $symbolstartpos), consise, AUnknown)
    }


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
  | s=statementListItem { $symbolstartpos, s }
  | d=importDeclaration { $symbolstartpos, d }
  | d=exportDeclaration { $symbolstartpos, d }

(*----------------------------*)
(* 16.2.2 Imports *)
(*----------------------------*)

importDeclaration:
  | T_IMPORT kind=importClause from=fromClause wc=withClause? sc
    { let pos = $symbolstartpos in
      Import ({ from; kind; withClause=wc }, pi pos), p pos
    }
  | T_IMPORT from=moduleSpecifier wc=withClause? sc
    { let pos = $symbolstartpos in
      Import ({ from; kind = SideEffect; withClause=wc }, pi pos), p pos
    }
  | T_IMPORT T_DEFER id=namespaceImport from=fromClause wc=withClause? sc
    { let pos = $symbolstartpos in
      Import ({ from; kind = DeferNamespace id; withClause=wc }, pi pos), p pos
    }

withClause:
  | T_WITH "{" "}"                          { [] }
  | T_WITH "{" l = listc(withEntry) "}"     { l }
  | T_WITH "{" l = listc(withEntry) "," "}" { l }

withEntry:
  | a=T_STRING ":" b=T_STRING          { fst a, fst b  }
  | a=identifierName ":" b=T_STRING    { a, fst b }
  | a=identifierKeyword ":" b=T_STRING { a, fst b }

namespaceImport:
  | "*" T_AS id=bindingIdentifier { id }

importClause:
  | b=importedDefaultBinding                        { Default b }
  | b=importedDefaultBinding "," id=namespaceImport { Namespace (Some b, id) }
  | id=namespaceImport                              { Namespace (None, id) }
  | b=importedDefaultBinding "," x=namedImports     { Named (Some b, x) }
  | x=namedImports                                  { Named (None, x) }

importedDefaultBinding: id=bindingIdentifier { id }

namedImports:
  | "{" "}"                                  { [] }
  | "{" specs=listc(importSpecifier) "}"     { specs }
  | "{" specs=listc(importSpecifier) "," "}" { specs }

(* also valid for export *)
fromClause: T_FROM spec=moduleSpecifier { spec }

importSpecifier:
  | id=bindingIdentifier { (name_of_ident id, id) }
  | name=moduleExportName T_AS id=bindingIdentifier
    { let (_,s,_) = name in (s, id) }

%inline moduleExportName:
  | s=T_STRING           { `String, fst s, $symbolstartpos }
  | name=identifierName  { `Ident, name, $symbolstartpos }
  | kw=identifierKeyword { `Ident, kw, $symbolstartpos }

moduleSpecifier:
  | s=T_STRING { fst s }

(*----------------------------*)
(* 16.2.3 Exports *)
(*----------------------------*)

exportDeclaration:
  | T_EXPORT names=exportClause sc
    {
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
      Export (k, pi pos), p pos
    }
  | T_EXPORT v=variableStatement
    {
      let pos = $symbolstartpos in
      let k = match v with
        | Variable_statement (k,l) -> ExportVar (k, l)
        | _ -> assert false
      in
      Export (k, pi pos), p pos
    }
  | T_EXPORT d=declaration
    { let k = match d with
        | Variable_statement (k,l),_ -> ExportVar (k,l)
        | Function_declaration (id, decl),_ -> ExportFun (id,decl)
        | Class_declaration (id, decl),_ -> ExportClass (id,decl)
        | _ -> assert false
      in
      let pos = $symbolstartpos in
      Export (k,pi pos), p pos
    }
  | T_EXPORT T_DEFAULT e=assignmentExpression_NoStmt sc
    {
      let k = ExportDefaultExpression e in
      let pos = $symbolstartpos in
      Export (k,pi pos), p pos
    }
  | T_EXPORT T_DEFAULT e=primaryExpression_Object sc
    {
      let k = ExportDefaultExpression e in
      let pos = $symbolstartpos in
      Export (k,pi pos), p pos
    }
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
      Export (k,pi pos), p pos
    }
  | T_EXPORT "*" from=fromClause wc=withClause? sc
    {
      let kind = Export_all None in
      let pos = $symbolstartpos in
      Export (ExportFrom ({from; kind;withClause=wc}),pi pos), p pos
    }
  | T_EXPORT "*" T_AS id=moduleExportName from=fromClause wc=withClause? sc
    {
      let (_,id,_) = id in
      let kind = Export_all (Some id) in
      let pos = $symbolstartpos in
      Export (ExportFrom ({from; kind;withClause=wc}), pi pos), p pos
    }
  | T_EXPORT names=exportClause from=fromClause wc=withClause? sc
    {
      let names = List.map (fun ((_,a,_), (_,b,_)) -> a, b) names in
      let kind = Export_names names in
      let pos = $symbolstartpos in
      Export (ExportFrom ({from; kind; withClause=wc}), pi pos), p pos
    }

exportSpecifier:
  | name=moduleExportName                                 { (name, name) }
  | local=moduleExportName T_AS exported=moduleExportName { (local, exported) }

exportClause:
  | "{" "}"                                   { [] }
  | "{" specs=listc(exportSpecifier) "}"      { specs }
  | "{" specs=listc(exportSpecifier) ","  "}" { specs }

(*************************************************************************)
(* Misc *)
(*************************************************************************)

sc:
  | semi=";"                 { semi }
  | semi=T_VIRTUAL_SEMICOLON { semi }
