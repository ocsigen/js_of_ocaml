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

optl(X):
  | (* empty *) { [] }
  | x=X         { x }

(* Guard rules for 'in' operator - used to parameterize expression rules *)
(* in_allowed: empty rule, allows the T_IN production *)
(* in_disallowed: matches T_EOF which never appears mid-expression, effectively disabling the production *)
%inline in_allowed: { }
%inline in_disallowed: T_EOF { }

(* Guard rules for 'yield' expression - controls whether yield is allowed *)
(* yield_allowed: empty rule, allows the yield production *)
(* yield_disallowed: matches T_ERROR which never appears in valid input, effectively disabling the production *)
%inline yield_allowed: { }
%inline yield_disallowed: T_ERROR { }

(* Guard rules for 'await' expression - controls whether await is allowed *)
(* await_allowed: empty rule, allows the await production *)
(* await_disallowed: matches T_ERROR sequence which never appears in valid input, effectively disabling the production *)
%inline await_allowed: { }
%inline await_disallowed: T_ERROR T_ERROR { }



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

templateLiteral(yield_, await_): T_BACKQUOTE spans=templateSpan(yield_, await_)* T_BACKQUOTE  { spans }

templateSpan(yield_, await_):
  | s=T_ENCAPSED_STRING                        { TStr (utf8_s s) }
  | T_DOLLARCURLY e=expression(in_allowed, yield_, await_) "}" { TExp e }

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

primaryExpression(x, yield_, await_):
  | e=primaryExpressionNoBraces(yield_, await_)
  | e=x { e }

d1(yield_, await_):
  | e=primaryExpression_FunClass(yield_, await_) { e }
  | e=primaryExpression_Object(yield_, await_)   { e }

primaryExpression_Object(yield_, await_):
  | e=objectLiteral(yield_, await_) { e }

primaryExpression_Empty: T_ERROR TComment { assert false }

primaryExpression_FunClass(yield_, await_):
  | e=functionExpression(yield_, await_)       { e }
  | e=classExpression(yield_, await_)          { e }
  | e=generatorExpression(yield_, await_)      { e }
  | e=asyncFunctionExpression(yield_, await_)  { e }
  | e=asyncGeneratorExpression(yield_,await_)  { e }

primaryExpressionNoBraces(yield_, await_):
  | T_THIS                      { vartok $symbolstartpos T_THIS }
  | i=identifier                { EVar i }
  | T_POUND name=identifierName { EPrivName name }
  | n=nullLiteral               { n }
  | b=booleanLiteral            { b }
  | n=numericLiteral            { ENum (Num.of_string_unsafe n) }
  | n=bigIntLiteral             { ENum (Num.of_string_unsafe n) }
  | s=stringLiteral             { s }
  | t=templateLiteral(yield_, await_)           { ETemplate t }
  | r=regularExpressionLiteral  { r }
  | a=arrayLiteral(yield_, await_)              { a }
  | e=coverParenthesizedExpressionAndArrowParameterList(yield_, await_) { e }

coverParenthesizedExpressionAndArrowParameterList(yield_, await_):
  | "(" e=expression(in_allowed, yield_, await_) ","? ")"  { e }
  | "(" _rparen=")"                        { CoverParenthesizedExpressionAndArrowParameterList (early_error (pi $startpos(_rparen))) }
  | "(" _ellipsis="..." bindingElement(yield_, await_) ")" { CoverParenthesizedExpressionAndArrowParameterList (early_error (pi $startpos(_ellipsis)) ) }
  | "(" expression(in_allowed, yield_, await_) "," _ellipsis="..." bindingElement(yield_, await_) ")"
    { CoverParenthesizedExpressionAndArrowParameterList (early_error (pi $startpos(_ellipsis)) ) }

(*----------------------------*)
(* 13.2.4 Array Initializer *)
(*----------------------------*)

arrayLiteral(yield_, await_):
  | "[" l=listc_with_empty (arrayElement(yield_, await_)) "]"
    { (EArr (List.map (function None -> ElementHole | Some x -> x) l)) }

arrayElement(yield_, await_):
  | e=assignmentExpression(in_allowed, yield_, await_)       { Element e }
  (* SpreadElement *)
  | "..." e=assignmentExpression(in_allowed, yield_, await_) { ElementSpread e }

(*----------------------------*)
(* 13.2.5 Object Initializer *)
(*----------------------------*)

propertyName(yield_, await_):
  | i=identifierName    { PNI i }
  | i=identifierKeyword { PNI i }
  | s=T_STRING          { let s, _len = s in PNS s }
  | n=numericLiteral    { PNN (Num.of_string_unsafe (n)) }
  | n=bigIntLiteral     { PNN (Num.of_string_unsafe (n)) }
  | "[" p=assignmentExpression(in_allowed, yield_, await_) "]" { PComputed p }

objectLiteral(yield_, await_):
  | "{" "}"                                      { EObj [] }
  | "{" props=listc(propertyDefinition(yield_, await_)) ","? "}" { EObj props }

propertyDefinition(yield_, await_):
  | name=propertyName(yield_, await_) ":" value=assignmentExpression(in_allowed, yield_, await_)
    { Property (name, value) }
  (* shorthand property *)
  | i=identifierName
    { Property (PNI i, EVar (ident_unsafe i)) }
  | id=identifier init=initializer_(in_allowed, yield_, await_)
    { CoverInitializedName (early_error (pi $startpos(init)), id, init)  }
  (* spread property *)
  | "..." e=assignmentExpression(in_allowed, yield_, await_)
    { PropertySpread(e) }
  | m=methodDefinition(propertyName(yield_, await_), yield_, await_)
    { let n, m = m in PropertyMethod(n,m) }

(*----------------------------*)
(* 13.3 Left-Hand-Side Expressions *)
(*----------------------------*)

leftHandSideExpression(yield_, await_): e=leftHandSideExpression_(d1(yield_, await_), yield_, await_) { e }

leftHandSideExpression_(x, yield_, await_):
  | e=newExpression(x, yield_, await_)      { e }
  | e=callExpression(x, yield_, await_)     { e }
  | e=optionalExpression(x, yield_, await_) { e }

(*----------------------------*)
(* 13.3.2 Property Accessors *)
(*----------------------------*)

fieldName:
  | name=identifierName  { name }
  | kw=identifierKeyword { kw }

memberExpression(x, yield_, await_):
  | e=primaryExpression(x, yield_, await_)
    { e }
  | _import=T_IMPORT "." T_META
    { EDot (vartok $startpos(_import) T_IMPORT,ANormal,(utf8_s "meta")) }
  | _import=T_IMPORT "." T_DEFER
    { EDot (vartok $startpos(_import) T_IMPORT,ANormal,(utf8_s "defer")) }
  | e1=memberExpression(x, yield_, await_) "[" e2=expression(in_allowed, yield_, await_) "]"
    { (EAccess (e1,ANormal, e2)) }
  | e1=memberExpression(x, yield_, await_) "." i=fieldName
    { (EDot(e1,ANormal,i)) }
  | T_NEW e1=memberExpression(d1(yield_, await_), yield_, await_) a=arguments(yield_, await_)
    { (ENew(e1, Some a, p $symbolstartpos)) }
  | e=memberExpression(x, yield_, await_) t=templateLiteral(yield_, await_)
    { ECallTemplate(e, t, p $symbolstartpos) }
  | _super=T_SUPER "[" e=expression(in_allowed, yield_, await_) "]"
    { (EAccess (vartok $startpos(_super) T_SUPER,ANormal, e)) }
  | _super=T_SUPER "." i=fieldName
    { (EDot(vartok $startpos(_super) T_SUPER,ANormal,i)) }
  | _new=T_NEW "." T_TARGET
    { (EDot(vartok $startpos(_new) T_NEW,ANormal,utf8_s "target")) }
  | e1=memberExpression(x, yield_, await_) "." T_POUND i=fieldName
    { (EDotPrivate(e1,ANormal,i)) }

(*----------------------------*)
(* 13.3.5 The new Operator *)
(*----------------------------*)

newExpression(x, yield_, await_):
  | e=memberExpression(x, yield_, await_)     { e }
  | T_NEW e=newExpression(d1(yield_, await_), yield_, await_) { (ENew (e,None, p $symbolstartpos)) }

(*----------------------------*)
(* 13.3.6 Function Calls *)
(*----------------------------*)

callExpression(x, yield_, await_):
  | _import=T_IMPORT a=arguments(yield_, await_)
    { (ECall(vartok $startpos(_import) T_IMPORT, ANormal, a, p $symbolstartpos)) }
  | e=memberExpression(x, yield_, await_) a=arguments(yield_, await_)
    { (ECall(e, ANormal, a, p $symbolstartpos)) }
  | e=callExpression(x, yield_, await_) a=arguments(yield_, await_)
    { (ECall(e, ANormal, a, p $symbolstartpos)) }
  | e=callExpression(x, yield_, await_) "[" e2=expression(in_allowed, yield_, await_) "]"
    { (EAccess (e, ANormal,  e2)) }
  | e=callExpression(x, yield_, await_) t=templateLiteral(yield_, await_)
    { ECallTemplate(e, t,p $symbolstartpos) }
  | _super=T_SUPER a=arguments(yield_, await_) { ECall(vartok $startpos(_super) T_SUPER,ANormal, a, p $symbolstartpos) }
  | e=callExpression(x, yield_, await_) "." i=methodName
    { EDot (e,ANormal,i) }
  | e=callExpression(x, yield_, await_) "." T_POUND i=methodName
    { EDotPrivate (e,ANormal,i) }

(*----------------------------*)
(* 13.3.8 Argument Lists *)
(*----------------------------*)

arguments(yield_, await_):
  | "(" args=argumentList(yield_, await_) ")" { args }

argumentList(yield_, await_):
  | (* empty *)                           { [] }
  | args=listc(argumentListElement(yield_, await_)) ","?  { args  }

(* assignmentExpression because expression supports sequence of exprs with ',' *)
argumentListElement(yield_, await_):
  | e=assignmentExpression(in_allowed, yield_, await_)       { Arg e }
  (* spread element, allowed not only in last position *)
  | "..." e=assignmentExpression(in_allowed, yield_, await_) { ArgSpread e }

(*----------------------------*)
(* 13.3.9 Optional Chains *)
(*----------------------------*)

optionalExpression(x, yield_, await_):
  | e=memberExpression(x, yield_, await_) c=optionalChain(yield_, await_)   { c e }
  | e=callExpression(x, yield_, await_) c=optionalChain(yield_, await_)     { c e }
  | e=optionalExpression(x, yield_, await_) c=optionalChain(yield_, await_) { c e }

optionalChain(yield_, await_):
   (* ?. Arguments *)
  | T_PLING_PERIOD a=arguments(yield_, await_)
    { fun e -> ECall(e, ANullish, a, p $symbolstartpos) }
  (* ?. [ Expression ] *)
  | T_PLING_PERIOD "[" e2=expression(in_allowed, yield_, await_) "]"
    { fun e -> EAccess(e, ANullish, e2) }
  (* ?. IdentifierName *)
  | T_PLING_PERIOD i=fieldName
    { fun e -> EDot(e, ANullish, i) }
  (* ?. TemplateLiteral, node is unhappy about it *)
  (* | T_PLING_PERIOD t=templateLiteral(yield_, await_)
    { fun e -> ECallTemplate(e, t, p $symbolstartpos) } *)
  (* ?. PrivateIdentifier *)
  | T_PLING_PERIOD T_POUND i=fieldName
    { fun e -> EDotPrivate(e, ANullish, i) }
  (* OptionalChain Arguments *)
  | c=optionalChain(yield_, await_) a=arguments(yield_, await_)
    { fun e -> ECall(c e, ANormal, a, p $symbolstartpos) }
  (* OptionalChain [ Expression ] *)
  | c=optionalChain(yield_, await_) "[" e2=expression(in_allowed, yield_, await_) "]"
    { fun e -> EAccess(c e, ANormal, e2) }
  (* OptionalChain . IdentifierName *)
  | c=optionalChain(yield_, await_) "." i=fieldName
    { fun e -> EDot(c e, ANormal, i) }
  (* OptionalChain TemplateLiteral *)
  | c=optionalChain(yield_, await_) t=templateLiteral(yield_, await_)
    { fun e -> ECallTemplate(c e, t, p $symbolstartpos) }
  (* OptionalChain . PrivateIdentifier *)
  | c=optionalChain(yield_, await_) "." T_POUND i=fieldName
    { fun e -> EDotPrivate(c e, ANormal, i) }

(*----------------------------*)
(* 13.4 Update Expressions *)
(*----------------------------*)

updateExpression(x, yield_, await_):
  | e=leftHandSideExpression_(x, yield_, await_)           { e }
  | e=leftHandSideExpression_(x, yield_, await_) T_INCR_NB { EUn (IncrA, e) }
  | e=leftHandSideExpression_(x, yield_, await_) T_DECR_NB { EUn (DecrA, e) }
  | T_INCR e=unaryExpression(d1(yield_, await_), yield_, await_)           { EUn (IncrB, e) }
  | T_DECR e=unaryExpression(d1(yield_, await_), yield_, await_)           { EUn (DecrB, e) }
  | T_INCR_NB e=unaryExpression(d1(yield_, await_), yield_, await_)        { EUn (IncrB, e) }
  | T_DECR_NB e=unaryExpression(d1(yield_, await_), yield_, await_)        { EUn (DecrB, e) }

(*----------------------------*)
(* 13.5 Unary Operators *)
(*----------------------------*)

unaryExpression(x, yield_, await_):
  | e=updateExpression(x, yield_, await_)           { e }
  | T_DELETE e=unaryExpression(d1(yield_, await_), yield_, await_)  { EUn (Delete, e) }
  | T_VOID e=unaryExpression(d1(yield_, await_), yield_, await_)    { EUn (Void, e) }
  | T_TYPEOF e=unaryExpression(d1(yield_, await_), yield_, await_)  { EUn (Typeof, e) }
  | T_PLUS e=unaryExpression(d1(yield_, await_), yield_, await_)    { EUn (Pl, e) }
  | T_MINUS e=unaryExpression(d1(yield_, await_), yield_, await_)   { EUn (Neg, e)}
  | T_BIT_NOT e=unaryExpression(d1(yield_, await_), yield_, await_) { EUn (Bnot, e) }
  | T_NOT e=unaryExpression(d1(yield_, await_), yield_, await_)     { EUn (Not, e) }
  | await_ T_AWAIT e=unaryExpression(d1(yield_, await_), yield_, await_)   { EUn (Await, e) }

(*----------------------------*)
(* 13.6 Exponentiation Operator *)
(*----------------------------*)

(* Note: ** is right-associative *)
exponentiationExpression(x, yield_, await_):
  | e=unaryExpression(x, yield_, await_)                                         { e }
  | e1=updateExpression(x, yield_, await_) T_EXP e2=exponentiationExpression(d1(yield_, await_), yield_, await_) { EBin(Exp, e1, e2) }

(*----------------------------*)
(* 13.7 Multiplicative Operators *)
(*----------------------------*)

multiplicativeExpression(x, yield_, await_):
  | e=exponentiationExpression(x, yield_, await_)                                        { e }
  | e1=multiplicativeExpression(x, yield_, await_) "*" e2=exponentiationExpression(d1(yield_, await_), yield_, await_)   { EBin(Mul, e1, e2) }
  | e1=multiplicativeExpression(x, yield_, await_) T_DIV e2=exponentiationExpression(d1(yield_, await_), yield_, await_) { EBin(Div, e1, e2) }
  | e1=multiplicativeExpression(x, yield_, await_) T_MOD e2=exponentiationExpression(d1(yield_, await_), yield_, await_) { EBin(Mod, e1, e2) }

(*----------------------------*)
(* 13.8 Additive Operators *)
(*----------------------------*)

additiveExpression(x, yield_, await_):
  | e=multiplicativeExpression(x, yield_, await_)                                    { e }
  | e1=additiveExpression(x, yield_, await_) T_PLUS e2=multiplicativeExpression(d1(yield_, await_), yield_, await_)  { EBin(Plus, e1, e2) }
  | e1=additiveExpression(x, yield_, await_) T_MINUS e2=multiplicativeExpression(d1(yield_, await_), yield_, await_) { EBin(Minus, e1, e2) }

(*----------------------------*)
(* 13.9 Bitwise Shift Operators *)
(*----------------------------*)

shiftExpression(x, yield_, await_):
  | e=additiveExpression(x, yield_, await_)                                   { e }
  | e1=shiftExpression(x, yield_, await_) T_LSHIFT e2=additiveExpression(d1(yield_, await_), yield_, await_)  { EBin(Lsl, e1, e2) }
  | e1=shiftExpression(x, yield_, await_) T_RSHIFT e2=additiveExpression(d1(yield_, await_), yield_, await_)  { EBin(Asr, e1, e2) }
  | e1=shiftExpression(x, yield_, await_) T_RSHIFT3 e2=additiveExpression(d1(yield_, await_), yield_, await_) { EBin(Lsr, e1, e2) }

(*----------------------------*)
(* 13.10 Relational Operators *)
(*----------------------------*)

(* Parameterized by in_ which is either in_allowed or in_disallowed.
   The in_ prefix on the T_IN rule acts as a guard:
   - in_allowed is empty, so the T_IN production is active
   - in_disallowed matches T_EOF (impossible here), so the production is disabled *)
relationalExpression(x, in_, yield_, await_):
  | e=shiftExpression(x, yield_, await_)                                                        { e }
  | e1=relationalExpression(x, in_, yield_, await_) T_LESS_THAN e2=shiftExpression(d1(yield_, await_), yield_, await_)          { EBin(Lt, e1, e2) }
  | e1=relationalExpression(x, in_, yield_, await_) T_GREATER_THAN e2=shiftExpression(d1(yield_, await_), yield_, await_)       { EBin(Gt, e1, e2) }
  | e1=relationalExpression(x, in_, yield_, await_) T_LESS_THAN_EQUAL e2=shiftExpression(d1(yield_, await_), yield_, await_)    { EBin(Le, e1, e2) }
  | e1=relationalExpression(x, in_, yield_, await_) T_GREATER_THAN_EQUAL e2=shiftExpression(d1(yield_, await_), yield_, await_) { EBin(Ge, e1, e2) }
  | e1=relationalExpression(x, in_, yield_, await_) T_INSTANCEOF e2=shiftExpression(d1(yield_, await_), yield_, await_)         { EBin (InstanceOf, e1, e2) }
  | in_ e1=relationalExpression(x, in_, yield_, await_) T_IN e2=shiftExpression(d1(yield_, await_), yield_, await_)             { EBin (In, e1, e2) }

(*----------------------------*)
(* 13.11 Equality Operators *)
(*----------------------------*)

equalityExpression(x, in_, yield_, await_):
  | e=relationalExpression(x, in_, yield_, await_)                                                    { e }
  | e1=equalityExpression(x, in_, yield_, await_) T_EQUAL e2=relationalExpression(d1(yield_, await_), in_, yield_, await_)            { EBin(EqEq, e1, e2) }
  | e1=equalityExpression(x, in_, yield_, await_) T_NOT_EQUAL e2=relationalExpression(d1(yield_, await_), in_, yield_, await_)        { EBin(NotEq, e1, e2) }
  | e1=equalityExpression(x, in_, yield_, await_) T_STRICT_EQUAL e2=relationalExpression(d1(yield_, await_), in_, yield_, await_)     { EBin(EqEqEq, e1, e2) }
  | e1=equalityExpression(x, in_, yield_, await_) T_STRICT_NOT_EQUAL e2=relationalExpression(d1(yield_, await_), in_, yield_, await_) { EBin(NotEqEq, e1, e2) }

(*----------------------------*)
(* 13.12 Binary Bitwise Operators *)
(*----------------------------*)

bitwiseANDExpression(x, in_, yield_, await_):
  | e=equalityExpression(x, in_, yield_, await_)                                             { e }
  | e1=bitwiseANDExpression(x, in_, yield_, await_) T_BIT_AND e2=equalityExpression(d1(yield_, await_), in_, yield_, await_) { EBin(Band, e1, e2) }

bitwiseXORExpression(x, in_, yield_, await_):
  | e=bitwiseANDExpression(x, in_, yield_, await_)                                             { e }
  | e1=bitwiseXORExpression(x, in_, yield_, await_) T_BIT_XOR e2=bitwiseANDExpression(d1(yield_, await_), in_, yield_, await_) { EBin(Bxor, e1, e2) }

bitwiseORExpression(x, in_, yield_, await_):
  | e=bitwiseXORExpression(x, in_, yield_, await_)                                           { e }
  | e1=bitwiseORExpression(x, in_, yield_, await_) T_BIT_OR e2=bitwiseXORExpression(d1(yield_, await_), in_, yield_, await_) { EBin(Bor, e1, e2) }

(*----------------------------*)
(* 13.13 Binary Logical Operators *)
(*----------------------------*)

logicalANDExpression(x, in_, yield_, await_):
  | e=bitwiseORExpression(x, in_, yield_, await_)                                         { e }
  | e1=logicalANDExpression(x, in_, yield_, await_) T_AND e2=bitwiseORExpression(d1(yield_, await_), in_, yield_, await_) { EBin(And, e1, e2) }

logicalORExpression(x, in_, yield_, await_):
  | e=logicalANDExpression(x, in_, yield_, await_)                                       { e }
  | e1=logicalORExpression(x, in_, yield_, await_) T_OR e2=logicalANDExpression(d1(yield_, await_), in_, yield_, await_) { EBin(Or, e1, e2) }

(* Coalesce expression - can't mix with || or && without parens *)
coalesceExpression(x, in_, yield_, await_):
  | e1=coalesceExpressionHead(x, in_, yield_, await_) T_PLING_PLING e2=bitwiseORExpression(d1(yield_, await_), in_, yield_, await_)  { EBin(Coalesce, e1, e2) }

coalesceExpressionHead(x, in_, yield_, await_):
  | e=coalesceExpression(x, in_, yield_, await_)  { e }
  | e=bitwiseORExpression(x, in_, yield_, await_) { e }

(* ShortCircuitExpression: either logical OR chain or coalesce chain *)
shortCircuitExpression(x, in_, yield_, await_):
  | e=logicalORExpression(x, in_, yield_, await_) { e }
  | e=coalesceExpression(x, in_, yield_, await_)  { e }

(*----------------------------*)
(* 13.14 Conditional Operator ( ? : ) *)
(*----------------------------*)

conditionalExpression(x, in_, yield_, await_):
  | e=shortCircuitExpression(x, in_, yield_, await_)
    { e }
  | cond=shortCircuitExpression(x, in_, yield_, await_) "?" then_=assignmentExpression(in_allowed, yield_, await_) ":" else_=assignmentExpression(in_, yield_, await_)
    { ECond (cond, then_, else_) }

(*----------------------------*)
(* 13.15 Assignment Operators *)
(*----------------------------*)

assignmentExpression(in_, yield_, await_):
  | e=conditionalExpression(d1(yield_, await_), in_, yield_, await_) { e }
  | e1=leftHandSideExpression_(d1(yield_, await_), yield_, await_) op=assignmentOperator e2=assignmentExpression(in_, yield_, await_)
    {
      let e1 = assignment_target_of_expr (Some op) e1 in
      EBin (op, e1, e2)
    }
  | e=arrowFunction(in_, yield_, await_)      { e }
  | e=asyncArrowFunction(in_, yield_, await_) { e }
  | yield_ e=yieldExpression(in_, yield_, await_)    { e }

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

expression(in_, yield_, await_):
  | e=assignmentExpression(in_, yield_, await_)                         { e }
  | e1=expression(in_, yield_, await_) "," e2=assignmentExpression(in_, yield_, await_) { ESeq (e1, e2) }

(*----------------------------*)
(* Expression variants (no statement-like constructs) *)
(*----------------------------*)

expressionNoStmt(yield_, await_):
  | e=assignmentExpression_NoStmt(yield_, await_)                               { e }
  | e1=expressionNoStmt(yield_, await_) "," e2=assignmentExpression(in_allowed, yield_, await_) { ESeq (e1, e2) }

assignmentExpression_NoStmt(yield_, await_):
  | e=conditionalExpression(primaryExpression_Empty, in_allowed, yield_, await_) { e }
  | e1=leftHandSideExpression_(primaryExpression_Empty, yield_, await_) op=assignmentOperator e2=assignmentExpression(in_allowed, yield_, await_)
    {
      let e1 = assignment_target_of_expr (Some op) e1 in
      EBin (op, e1, e2)
    }
  | e=arrowFunction(in_allowed, yield_, await_)      { e }
  | e=asyncArrowFunction(in_allowed, yield_, await_) { e }
  | yield_ e=yieldExpression(in_allowed, yield_, await_)    { e }

(*----------------------------*)
(* Expression variants (for concise arrow body) *)
(*----------------------------*)

assignmentExpressionForConciseBody(in_, yield_, await_):
  | e=conditionalExpression(primaryExpression_FunClass(yield_, await_), in_, yield_, await_) { e }
  | e1=leftHandSideExpression_(primaryExpression_FunClass(yield_, await_), yield_, await_) op=assignmentOperator e2=assignmentExpression(in_, yield_, await_)
    {
      let e1 = assignment_target_of_expr (Some op) e1 in
      EBin (op, e1, e2)
    }
  | e=arrowFunction(in_, yield_, await_)      { e }
  | e=asyncArrowFunction(in_, yield_, await_) { e }
  | yield_ e=yieldExpression(in_, yield_, await_)    { e }

(*************************************************************************)
(* Section 14: ECMAScript Language: Statements and Declarations         *)
(*************************************************************************)

(*----------------------------*)
(* 14.1 Statement Semantics *)
(*----------------------------*)

%inline
statement(yield_, await_): s=statementBody(yield_, await_) { s, p $symbolstartpos }

statementBody(yield_, await_):
  | b=block(yield_, await_)               { Block b }
  | s=variableStatement(yield_, await_)   { s }
  | s=emptyStatement      { s }
  | s=expressionStatement(yield_, await_) { s }
  | s=ifStatement(yield_, await_)         { s }
  | s=iterationStatement(yield_, await_)  { s }
  | s=continueStatement   { s }
  | s=breakStatement      { s }
  | s=returnStatement(yield_, await_)     { s }
  | s=labelledStatement(yield_, await_)   { s }
  | s=switchStatement(yield_, await_)     { s }
  | s=throwStatement(yield_, await_)      { s }
  | s=tryStatement(yield_, await_)        { s }
  | s=withStatement(yield_, await_)       { s }
  | s=debuggerStatement   { s }

statementListItem(yield_, await_):
  | s=statement(yield_, await_)   { s }
  | d=declaration(yield_, await_) { d }

declaration(yield_, await_):
  | d=functionDeclaration(yield_, await_)
    { let i,f = d in Function_declaration (i,f), p $symbolstartpos }
  | d=generatorDeclaration(yield_, await_)
    { let i,f = d in Function_declaration (i,f), p $symbolstartpos }
  | d=asyncGeneratorDeclaration(yield_, await_)
    { let i,f = d in Function_declaration (i,f), p $symbolstartpos }
  | d=asyncFunctionDeclaration(yield_, await_)
    { let i,f = d in Function_declaration (i,f), p $symbolstartpos }
  | d=lexicalDeclaration(yield_, await_)
    { d, p $symbolstartpos }
  | d=classDeclaration(yield_, await_)
    { let i,f = d in Class_declaration (i,f), p $symbolstartpos }

(*----------------------------*)
(* 14.2 Block *)
(*----------------------------*)

block(yield_, await_): "{" l=optl(statementList(yield_, await_)) "}" { l }

statementList(yield_, await_): l=statementListItem(yield_, await_)+ { l }

(*----------------------------*)
(* 14.3 Declarations and the Variable Statement *)
(*----------------------------*)

(* 14.3.1 Let and Const Declarations *)
lexicalDeclaration(yield_, await_):
  | T_CONST l=listc(lexicalBinding(yield_, await_)) sc       { Variable_statement (Const, l)}
  | T_LET l=listc(lexicalBinding(yield_, await_)) sc         { Variable_statement (Let, l)}
 (* Explicit Resource Management *)
  | T_USING l=listc(usingBinding(yield_, await_)) sc         { Variable_statement (Using, l)}
  | T_AWAIT T_USING l=listc(usingBinding(yield_, await_)) sc { Variable_statement (AwaitUsing, l)}

(* 14.3.2 Variable Statement *)
variableStatement(yield_, await_):
  | T_VAR l=listc(variableDeclaration(in_allowed, yield_, await_)) sc { Variable_statement (Var, l) }

variableDeclaration(in_, yield_, await_):
  | i=identifier e=initializer_(in_, yield_, await_)?    { DeclIdent (i,e) }
  | p=bindingPattern(yield_, await_) e=initializer_(in_, yield_, await_) { DeclPattern (p, e) }

lexicalBinding(yield_, await_):
  | i=identifier e=initializer_(in_allowed, yield_, await_)?    { DeclIdent (i,e) }
  | p=bindingPattern(yield_, await_) e=initializer_(in_allowed, yield_, await_) { DeclPattern (p, e) }

(* using bindings require an initializer and only support BindingIdentifier per spec *)
usingBinding(yield_, await_):
  | i=identifier e=initializer_(in_allowed, yield_, await_) { DeclIdent (i, Some e) }

initializer_(in_, yield_, await_):
  | "=" e=assignmentExpression(in_, yield_, await_) { e, p $symbolstartpos }

forDeclaration(yield_, await_):
  | T_VAR l=listc(variableDeclaration(in_disallowed, yield_, await_) )  { Var, l }
  | T_CONST l=listc(variableDeclaration(in_disallowed, yield_, await_)) { Const, l }
  | T_LET l=listc(variableDeclaration(in_disallowed, yield_, await_))   { Let, l }

(* 'for ... in' and 'for ... of' declare only one variable *)
forBinding(yield_, await_):
  | T_VAR b=forBindingElement(yield_, await_)                { Var, b }
  | T_CONST b=forBindingElement(yield_, await_)              { Const, b }
  | T_LET  b=forBindingElement(yield_, await_)               { Let, b }
  (* Explicit Resource Management - uses restricted binding to resolve
    'for (using of ...)' ambiguity: identifier cannot be 'of' *)
  | T_USING b=forUsingBindingElement         { Using, b }
  | T_AWAIT T_USING b=forUsingBindingElement { AwaitUsing, b }

forBindingElement(yield_, await_):
  | b=singleNameBinding(yield_, await_)               { b }

(* Restricted binding for 'using' in for-in/of: only identifier, no 'of' *)
forUsingBindingElement:
  | id=identifierNoOf { BindingIdent id }

(*----------------------------*)
(* 14.3.3 Destructuring Binding Patterns *)
(*----------------------------*)

bindingPattern(yield_, await_):
  | p=objectBindingPattern(yield_, await_) { p }
  | p=arrayBindingPattern(yield_, await_)  { p }

singleNameBinding(yield_, await_):
  | p=bindingPattern(yield_, await_) { BindingPattern p }
  | id=identifier    { BindingIdent id }

objectBindingPattern(yield_, await_):
  | "{" "}"                               { ObjectBinding (list []) }
  | "{" r=bindingRestProperty "}"         { ObjectBinding {list = []; rest = Some r } }
  | "{" l=listc(bindingProperty(yield_, await_)) ","? "}" { ObjectBinding (list l) }
  | "{" l=listc(bindingProperty(yield_, await_)) "," r=bindingRestProperty "}"
    { ObjectBinding {list=l;rest= Some r} }

bindingProperty(yield_, await_):
  | i=identifier e=initializer_(in_allowed, yield_, await_)? { Prop_ident (Prop_and_ident i, e) }
  | pn=propertyName(yield_, await_) ":" e=bindingElement(yield_, await_)     { Prop_binding (pn, e) }

bindingRestProperty:
  (* can appear only at the end of a bindingPropertyList in ECMA *)
  | "..." id=identifier { id }

(* in theory used also for formal parameter as is *)
bindingElement(yield_, await_):
  | b=singleNameBinding(yield_, await_) e=initializer_(in_allowed, yield_, await_)? { b, e }

(* array destructuring *)

arrayBindingPattern(yield_, await_):
  | "[" l=listc_with_empty2(bindingElement(yield_, await_), bindingRestElement(yield_, await_)) "]"
    { ArrayBinding {list = fst l; rest = snd l } }

bindingRestElement(yield_, await_):
  (* can appear only at the end of a arrayBindingPattern in ECMA *)
  | "..." b=singleNameBinding(yield_, await_) { b }

(*----------------------------*)
(* 14.4 Empty Statement *)
(*----------------------------*)

emptyStatement:
  | T_SEMICOLON { Empty_statement }

(*----------------------------*)
(* 14.5 Expression Statement *)
(*----------------------------*)

expressionStatement(yield_, await_):
  | e=expressionNoStmt(yield_, await_) sc { Expression_statement e }

(*----------------------------*)
(* 14.6 The if Statement *)
(*----------------------------*)

ifStatement(yield_, await_):
  | T_IF "(" c=expression(in_allowed, yield_, await_) ")" t=statement(yield_, await_) T_ELSE e=statement(yield_, await_)
    { If_statement (c, t, Some e) }
  | T_IF "(" c=expression(in_allowed, yield_, await_) ")" t=statement(yield_, await_) %prec p_IF
    { If_statement (c, t, None) }

(*----------------------------*)
(* 14.7 Iteration Statements *)
(*----------------------------*)

iterationStatement(yield_, await_):
  (* 14.7.2 The do-while Statement *)
  | T_DO body=statement(yield_, await_) T_WHILE "(" condition=expression(in_allowed, yield_, await_) ")" endrule(sc | T_VIRTUAL_SEMICOLON_DO_WHILE { () } )
    { Do_while_statement (body, condition) }

  (* 14.7.3 The while Statement *)
  | T_WHILE "(" condition=expression(in_allowed, yield_, await_) ")" body=statement(yield_, await_)
    { While_statement (condition, body) }

  (* 14.7.4 The for Statement *)
  | T_FOR "(" i=expression(in_disallowed, yield_, await_)? ";" c=expression(in_allowed, yield_, await_)? ";" incr=expression(in_allowed, yield_, await_)? ")" st=statement(yield_, await_)
    { For_statement (Left i, c, incr, st) }
  | T_FOR "(" l=forDeclaration(yield_, await_) ";" c=expression(in_allowed, yield_, await_)? ";" incr=expression(in_allowed, yield_, await_)? ")" st=statement(yield_, await_)
    { For_statement (Right l, c, incr, st) }

  (* 14.7.5 The for-in and for-of Statements *)
  | T_FOR "(" left=leftHandSideExpression(yield_, await_) T_IN right=expression(in_allowed, yield_, await_) ")" body=statement(yield_, await_)
    { let left = assignment_target_of_expr None left in
      ForIn_statement (Left left, right, body) }
  | T_FOR "(" left=forBinding(yield_, await_) T_IN right=expression(in_allowed, yield_, await_) ")" body=statement(yield_, await_)
    { ForIn_statement (Right left, right, body) }

  | T_FOR "(" left=leftHandSideExpression(yield_, await_) T_OF right=assignmentExpression(in_allowed, yield_, await_) ")" body=statement(yield_, await_)
    { let left = assignment_target_of_expr None left in
      ForOf_statement (Left left, right, body) }
  | T_FOR "(" left=forBinding(yield_, await_) T_OF right=assignmentExpression(in_allowed, yield_, await_) ")" body=statement(yield_, await_)
    { ForOf_statement (Right left, right, body) }
  | T_FOR T_AWAIT "(" left=leftHandSideExpression(yield_, await_) T_OF right=assignmentExpression(in_allowed, yield_, await_) ")" body=statement(yield_, await_)
    { let left = assignment_target_of_expr None left in
      ForAwaitOf_statement (Left left, right, body) }
  | T_FOR T_AWAIT "(" left=forBinding(yield_, await_) T_OF right=assignmentExpression(in_allowed, yield_, await_) ")" body=statement(yield_, await_)
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

returnStatement(yield_, await_):
  | T_RETURN e=expression(in_allowed, yield_, await_)? sc { (Return_statement (e, p $endpos(e))) }

(*----------------------------*)
(* 14.11 The with Statement *)
(*----------------------------*)

withStatement(yield_, await_):
  | T_WITH "(" e=expression(in_allowed, yield_, await_) ")" s=statement(yield_, await_) { (With_statement (e,s)) }

(*----------------------------*)
(* 14.12 The switch Statement *)
(*----------------------------*)

switchStatement(yield_, await_):
  | T_SWITCH "(" subject=expression(in_allowed, yield_, await_) ")" cb=caseBlock(yield_, await_)
    { let c1, d, c2 = cb in
      Switch_statement (subject, c1, d, c2)
    }

caseBlock(yield_, await_):
  | "{" cases=caseClause(yield_, await_)* "}" { cases, None, [] }
  | "{" before=caseClause(yield_, await_)* default=defaultClause(yield_, await_) after=caseClause(yield_, await_)* "}" { before, Some default, after }

caseClause(yield_, await_):
  | T_CASE e=expression(in_allowed, yield_, await_) ":" s= optl(statementList(yield_, await_)) { e,s }

defaultClause(yield_, await_):
  | T_DEFAULT ":" list=optl(statementList(yield_, await_)) { list }

(*----------------------------*)
(* 14.13 Labelled Statements *)
(*----------------------------*)

labelIdentifier:
  | name=identifierName { Label.of_string name }

labelledStatement(yield_, await_):
  | l=labelIdentifier ":" s=statement(yield_, await_) { Labelled_statement (l, s)}

(*----------------------------*)
(* 14.14 The throw Statement *)
(*----------------------------*)

throwStatement(yield_, await_):
  | T_THROW e=expression(in_allowed, yield_, await_) sc { (Throw_statement e) }

(*----------------------------*)
(* 14.15 The try Statement *)
(*----------------------------*)

tryStatement(yield_, await_):
  | T_TRY b=block(yield_, await_) c=catch(yield_, await_)           { (Try_statement (b, Some c, None)) }
  | T_TRY b=block(yield_, await_)         f=finally(yield_, await_) { (Try_statement (b, None, Some f)) }
  | T_TRY b=block(yield_, await_) c=catch(yield_, await_) f=finally(yield_, await_) { (Try_statement (b, Some c, Some f)) }

catch(yield_, await_):
  | T_CATCH "(" p=formalParameter(yield_, await_) ")" b=block(yield_, await_) { Some p,b }
  | T_CATCH b=block(yield_, await_)                           { None,b }

finally(yield_, await_):
  | T_FINALLY b=block(yield_, await_) { b }

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

formalParameters(yield_, await_):
  | (* empty *)                                               { list [] }
  | params=listc(formalParameter(yield_, await_)) ","?                        { list params }
  | r=functionRestParameter(yield_, await_)                                   { { list = []; rest = Some r } }
  | params=listc(formalParameter(yield_, await_)) "," r=functionRestParameter(yield_, await_) { { list = params; rest = Some r } }

functionRestParameter(yield_, await_):
  | "..." b=singleNameBinding(yield_, await_) { b }

formalParameter(yield_, await_):
  | b=singleNameBinding(yield_, await_) init=initializer_(in_allowed, yield_, await_)? { b, init }

callSignature(yield_, await_): "(" args=formalParameters(yield_, await_) ")" { args }

functionBody(yield_, await_): body=optl(statementList(yield_, await_)) { body }

(*----------------------------*)
(* 15.2 Function Definitions *)
(*----------------------------*)

functionDeclaration(yield_, await_):
  | T_FUNCTION name=identifier args=callSignature(yield_, await_) "{" b=functionBody(yield_disallowed, await_disallowed) _rbrace="}"
    { (name, ({async = false; generator = false}, args, b, p $startpos(_rbrace))) }

functionExpression(yield_, await_):
  | T_FUNCTION name=identifier? args=callSignature(yield_, await_) "{" b=functionBody(yield_disallowed, await_disallowed) "}"
    { EFun (name, ({async = false; generator = false}, args, b, p $symbolstartpos)) }

(*----------------------------*)
(* 15.3 Arrow Function Definitions *)
(*----------------------------*)

arrowFunction(in_, yield_, await_):
  | i=identifier T_ARROW b=conciseBody(in_, yield_disallowed, await_disallowed)
    { let b,consise = b in
      EArrow (({async = false; generator = false}, list [param' i],b, p $symbolstartpos), consise, AUnknown) }
  | T_LPAREN_ARROW a=formalParameters(yield_, await_) ")" T_ARROW b=conciseBody(in_, yield_disallowed, await_disallowed)
    { let b,consise = b in
      EArrow (({async = false; generator = false}, a,b, p $symbolstartpos), consise, AUnknown) }

conciseBody(in_, yield_, await_):
  | "{" b=functionBody(yield_, await_) "}" { b, false }
  | e=assignmentExpressionForConciseBody(in_, yield_, await_) { [(Return_statement (Some e, p $endpos), p $symbolstartpos)], true }

(*----------------------------*)
(* 15.4 Method Definitions *)
(*----------------------------*)

methodName:
  | name=identifierName  { name }
  | kw=identifierKeyword { kw }

methodDefinition(name, yield_, await_):
  | T_GET name=name args=callSignature(yield_, await_) "{" b=functionBody(yield_disallowed, await_disallowed) "}"
    { name, MethodGet(({async = false; generator = false}, args, b, p $symbolstartpos)) }
  | T_SET name=name args=callSignature(yield_, await_) "{" b=functionBody(yield_disallowed, await_disallowed) "}"
    { name, MethodSet(({async = false; generator = false}, args, b, p $symbolstartpos)) }
  | name=name args=callSignature(yield_, await_) "{" b=functionBody(yield_disallowed, await_disallowed) "}"
    { name, Method(({async = false; generator = false}, args, b, p $symbolstartpos)) }
  | T_ASYNC name=name args=callSignature(yield_, await_) "{" b=functionBody(yield_disallowed, await_allowed) "}"
    { name, Method(({async = true; generator = false}, args, b, p $symbolstartpos)) }
  | "*" name=name args=callSignature(yield_, await_) "{" b=functionBody(yield_allowed, await_disallowed) "}"
    { name, Method(({async = false; generator = true}, args, b, p $symbolstartpos)) }
  | T_ASYNC "*" name=name args=callSignature(yield_, await_) "{" b=functionBody(yield_allowed, await_allowed) "}"
    { name, Method(({async = true; generator = true}, args, b, p $symbolstartpos)) }

(*----------------------------*)
(* 15.5 Generator Function Definitions *)
(*----------------------------*)

generatorDeclaration(yield_, await_):
  | T_FUNCTION "*" name=identifier args=callSignature(yield_, await_disallowed) "{" b=functionBody(yield_allowed, await_disallowed) "}"
    { (name, ({async = false; generator = true}, args, b, p $symbolstartpos)) }

generatorExpression(yield_, await_):
  | T_FUNCTION "*" name=identifier? args=callSignature(yield_, await_) "{" b=functionBody(yield_allowed, await_disallowed) "}"
    { EFun (name, ({async = false; generator = true}, args, b, p $symbolstartpos)) }

yieldExpression(in_, yield_, await_):
  | T_YIELD { EYield { delegate = false; expr = None } }
  | T_YIELD e=assignmentExpression(in_, yield_allowed, await_) { EYield { delegate = false; expr = Some e } }
  | T_YIELD "*" e=assignmentExpression(in_, yield_allowed, await_) { EYield { delegate = true; expr = Some e } }

(*----------------------------*)
(* 15.6 Async Generator Function Definitions *)
(*----------------------------*)

asyncGeneratorDeclaration(yield_, await_):
  | T_ASYNC T_FUNCTION "*" name=identifier args=callSignature(yield_, await_) "{" b=functionBody(yield_allowed, await_allowed) "}"
    { (name, ({async = true; generator = true}, args, b, p $symbolstartpos)) }

asyncGeneratorExpression(yield_, await_):
  | T_ASYNC T_FUNCTION "*" name=identifier? args=callSignature(yield_, await_) "{" b=functionBody(yield_allowed, await_allowed) "}"
    { EFun (name, ({async = true; generator = true}, args, b, p $symbolstartpos)) }

(*----------------------------*)
(* 15.7 Class Definitions *)
(*----------------------------*)

classDeclaration(yield_, await_): T_CLASS id=bindingIdentifier extends=classHeritage(yield_, await_)? body=classBody(yield_, await_)
    { id, {extends; body}  }

classExpression(yield_, await_): T_CLASS i=bindingIdentifier? extends=classHeritage(yield_, await_)? body=classBody(yield_, await_)
    { EClass (i, {extends; body}) }

classHeritage(yield_, await_): T_EXTENDS e=leftHandSideExpression(yield_, await_) { e }

classBody(yield_, await_): "{" elements=classElement(yield_, await_)* "}" { List.flatten elements }

classElement(yield_, await_):
  | m=methodDefinition(classElementName(yield_, await_), yield_, await_)
    { let n,m = m in [ CEMethod (false, n, m) ] }
  | T_STATIC m=methodDefinition(classElementName(yield_, await_), yield_, await_)
    { let n,m = m in [ CEMethod (true, n, m) ] }
  | n=classElementName(yield_, await_) i=initializer_(in_allowed, yield_, await_)? sc
    { [ CEField (false, n, i) ] }
  | T_STATIC n=classElementName(yield_, await_) i=initializer_(in_allowed, yield_, await_)? sc
    { [ CEField (true, n, i) ] }
  | T_STATIC b=block(yield_disallowed, await_allowed) { [CEStaticBLock b] }
  | sc               { [] }

classElementName(yield_, await_):
  | name=propertyName(yield_, await_) { PropName name }
  | T_POUND name=identifierName { PrivName name }

(*----------------------------*)
(* 15.8 Async Function Definitions *)
(*----------------------------*)

asyncFunctionDeclaration(yield_, await_):
  | T_ASYNC T_FUNCTION  name=identifier args=callSignature(yield_, await_) "{" b=functionBody(yield_disallowed, await_allowed) "}"
    { (name, ({async = true; generator = false}, args, b, p $symbolstartpos)) }

asyncFunctionExpression(yield_, await_):
  | T_ASYNC T_FUNCTION name=identifier? args=callSignature(yield_, await_) "{" b=functionBody(yield_disallowed, await_allowed) "}"
   { EFun (name, ({async = true; generator = false}, args, b, p $symbolstartpos)) }

(*----------------------------*)
(* 15.9 Async Arrow Function Definitions *)
(*----------------------------*)

asyncArrowFunction(in_, yield_, await_):
  (* Use identifierNoOf to resolve 'for (async of ...)' ambiguity *)
  | T_ASYNC i=identifierNoOf T_ARROW b=conciseBody(in_, yield_disallowed, await_allowed)
    {
      let b,consise = b in
      EArrow(({async = true; generator = false}, list [param' i],b, p $symbolstartpos), consise, AUnknown)
    }
  | T_ASYNC T_LPAREN_ARROW a=formalParameters(yield_, await_) ")" T_ARROW b=conciseBody(in_, yield_disallowed, await_allowed)
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
  | e=expression(in_allowed, yield_disallowed, await_disallowed) T_EOF { e }

program:
  | l=moduleItem* T_EOF { l }

(*----------------------------*)
(* 16.2 Modules *)
(*----------------------------*)

moduleItem:
  | s=statementListItem(yield_disallowed, await_allowed) { $symbolstartpos, s }
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
  | T_EXPORT v=variableStatement(yield_disallowed, await_allowed)
    {
      let pos = $symbolstartpos in
      let k = match v with
        | Variable_statement (k,l) -> ExportVar (k, l)
        | _ -> assert false
      in
      Export (k, pi pos), p pos
    }
  | T_EXPORT d=declaration(yield_disallowed, await_allowed)
    { let k = match d with
        | Variable_statement (k,l),_ -> ExportVar (k,l)
        | Function_declaration (id, decl),_ -> ExportFun (id,decl)
        | Class_declaration (id, decl),_ -> ExportClass (id,decl)
        | _ -> assert false
      in
      let pos = $symbolstartpos in
      Export (k,pi pos), p pos
    }
  | T_EXPORT T_DEFAULT e=assignmentExpression_NoStmt(yield_disallowed, await_allowed) sc
    {
      let k = ExportDefaultExpression e in
      let pos = $symbolstartpos in
      Export (k,pi pos), p pos
    }
  | T_EXPORT T_DEFAULT e=primaryExpression_Object(yield_disallowed, await_allowed) sc
    {
      let k = ExportDefaultExpression e in
      let pos = $symbolstartpos in
      Export (k,pi pos), p pos
    }
  | T_EXPORT T_DEFAULT e=primaryExpression_FunClass(yield_disallowed, await_allowed) endrule(sc | T_VIRTUAL_SEMICOLON_EXPORT_DEFAULT { () } )
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
