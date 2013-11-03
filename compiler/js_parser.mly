(* Js_of_ocaml compiler *)
(* Copyright (C) 2013 Hugo Heuzard *)

(* Yoann Padioleau *)

(* Copyright (C) 2010 Facebook *)

(* This library is free software; you can redistribute it and/or *)
(* modify it under the terms of the GNU Lesser General Public License *)
(* version 2.1 as published by the Free Software Foundation, with the *)
(* special exception on linking described in file license.txt. *)

(* This library is distributed in the hope that it will be useful, but *)
(* WITHOUT ANY WARRANTY; without even the implied warranty of *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file *)
(* license.txt for more details. *)

%{
(*
 * src: ocamlyaccified from Marcel Laverdet 'fbjs2' via emacs macros, itself
 * extracted from the official ECMAscript specification at:
 *  http://www.ecma-international.org/publications/standards/ecma-262.htm
 *
 * see also http://en.wikipedia.org/wiki/ECMAScript_syntax
 *
 * related work:
 *  - http://marijnhaverbeke.nl/parse-js/, js parser in common lisp
 *    (which has been since ported to javascript by nodejs people)
 *  - jslint
 *)

module J = Javascript

let bop op a b= J.EBin(op,a,b)
let uop op a = J.EUn(op,a)
let var name = J.S name
%}

/*(*************************************************************************)*/
/*(*1 Tokens *)*/
/*(*************************************************************************)*/

/*(*-----------------------------------------*)*/
/*(*2 the comment tokens *)*/
/*(*-----------------------------------------*)*/
/*(* coupling: Token_helpers.is_real_comment *)*/
%token <Parse_info.t * string> TCommentSpace TCommentNewline   TComment TCommentML

/*(*-----------------------------------------*)*/
/*(*2 the normal tokens *)*/
/*(*-----------------------------------------*)*/

/*(* tokens with a value *)*/
%token<string * [`Float of float | `Int of int] * Parse_info.t> T_NUMBER
%token<string * Parse_info.t> T_IDENTIFIER
%token<string * Parse_info.t> T_STRING
%token<string * Parse_info.t> T_REGEX

/*(* keywords tokens *)*/
%token <Parse_info.t>
 T_FUNCTION T_IF T_RETURN T_SWITCH T_THIS T_THROW T_TRY
 T_VAR T_WHILE T_WITH T_NULL T_FALSE T_TRUE
 T_BREAK T_CASE T_CATCH T_CONTINUE T_DEFAULT T_DO T_FINALLY T_FOR

%token <Parse_info.t> T_ELSE

%token <Parse_info.t> T_NEW

/*(* syntax *)*/
%token <Parse_info.t>
 T_LCURLY T_RCURLY
 T_LPAREN T_RPAREN
 T_LBRACKET T_RBRACKET
 T_SEMICOLON
 T_COMMA
 T_PERIOD

/*(* operators *)*/
%token <Parse_info.t>
 T_RSHIFT3_ASSIGN T_RSHIFT_ASSIGN T_LSHIFT_ASSIGN
 T_BIT_XOR_ASSIGN T_BIT_OR_ASSIGN T_BIT_AND_ASSIGN T_MOD_ASSIGN T_DIV_ASSIGN
 T_MULT_ASSIGN T_MINUS_ASSIGN T_PLUS_ASSIGN T_ASSIGN

%token <Parse_info.t>
 T_PLING T_COLON
 T_OR
 T_AND
 T_BIT_OR
 T_BIT_XOR
 T_BIT_AND
 T_EQUAL T_NOT_EQUAL T_STRICT_EQUAL T_STRICT_NOT_EQUAL
 T_LESS_THAN_EQUAL T_GREATER_THAN_EQUAL T_LESS_THAN T_GREATER_THAN
 T_IN T_INSTANCEOF
 T_LSHIFT T_RSHIFT T_RSHIFT3
 T_PLUS T_MINUS
 T_DIV T_MULT T_MOD
 T_NOT T_BIT_NOT T_INCR T_DECR T_DELETE T_TYPEOF T_VOID

/*(*-----------------------------------------*)*/
/*(*2 extra tokens: *)*/
/*(*-----------------------------------------*)*/

%token <Parse_info.t> T_VIRTUAL_SEMICOLON

/*(* classic *)*/
%token <Parse_info.t> TUnknown
%token <Parse_info.t> EOF

/*(*-----------------------------------------*)*/
/*(*2 priorities *)*/
/*(*-----------------------------------------*)*/

/*(* Special if / else associativity*)*/
%nonassoc p_IF
%nonassoc T_ELSE

%left T_OR
%left T_AND
%left T_BIT_OR
%left T_BIT_XOR
%left T_BIT_AND
%left T_EQUAL T_NOT_EQUAL T_STRICT_EQUAL T_STRICT_NOT_EQUAL
%left
 T_LESS_THAN_EQUAL T_GREATER_THAN_EQUAL T_LESS_THAN T_GREATER_THAN
 T_IN T_INSTANCEOF
%left T_LSHIFT T_RSHIFT T_RSHIFT3
%left T_PLUS T_MINUS
%left T_DIV T_MULT T_MOD
%right T_NOT T_BIT_NOT T_INCR T_DECR T_DELETE T_TYPEOF T_VOID

/*(*************************************************************************)*/
/*(*1 Rules type declaration *)*/
/*(*************************************************************************)*/

%start program
%type <Javascript.program> program

%%

/*(*************************************************************************)*/
/*(*1 Toplevel *)*/
/*(*************************************************************************)*/

program:
    | l=list(source_element) EOF { l }
    | fake { assert false }

fake:
    | TCommentSpace
    | TCommentNewline
    | TComment
    | TCommentML
    | TUnknown { () }

source_element:
 | statement option(T_VIRTUAL_SEMICOLON)            { J.Statement $1 }
 | function_declaration option(T_VIRTUAL_SEMICOLON) { J.Function_declaration $1 }

/*(*************************************************************************)*/
/*(*1 statement *)*/
/*(*************************************************************************)*/

statement_all:
 | block                { J.Block $1 }
 (* this is not allowed but some browsers accept it *)
 (* | function_declaration { *)
 (*  let var,params,body,_ = $1 in *)
 (*  J.Variable_statement [var,Some (J.EFun((None,params,body),None))]} *)
 | variable_statement   { $1 }
 | empty_statement      { $1 }
 | expression_statement { $1 }
 | if_statement         { $1 }
 | iteration_statement  { $1 }
 | continue_statement   { $1 }
 | break_statement      { $1 }
 | return_statement     { $1 }
 | with_statement       { $1 }
 | labelled_statement   { $1 }
 | switch_statement     { $1 }
 | throw_statement      { $1 }
 | try_statement        { $1 }

statement:
 | statement_all option(T_VIRTUAL_SEMICOLON) {$1}

statement_bloc:
  list(T_VIRTUAL_SEMICOLON) s=statement { s }

block:
 | T_LCURLY l=list(statement) T_RCURLY { l }

variable_statement:
 | T_VAR separated_nonempty_list(T_COMMA,variable_declaration) semicolon  { J.Variable_statement $2 }

variable_declaration:
 | variable option(initializeur) { $1, $2 }

initializeur:
 | T_ASSIGN assignment_expression { $2 }


empty_statement:
 | T_SEMICOLON { J.Empty_statement }

expression_statement:
 | expression_no_statement semicolon { J.Expression_statement ($1, None) }


if_statement:
 | T_IF T_LPAREN expression T_RPAREN statement T_ELSE statement
     { J.If_statement ($3, $5, Some $7) }
 | T_IF T_LPAREN expression T_RPAREN statement %prec p_IF
     { J.If_statement ($3, $5, None) }


iteration_statement:
 | T_DO statement_bloc T_WHILE T_LPAREN expression T_RPAREN semicolon
     { J.Do_while_statement ($2, $5) }
 | T_WHILE T_LPAREN expression T_RPAREN statement_bloc
     { J.While_statement ($3, $5) }
 | T_FOR T_LPAREN
     option(expression_no_in) T_SEMICOLON
     option(expression) T_SEMICOLON
     option(expression)
     T_RPAREN statement_bloc
     { J.For_statement ( J.Left $3, $5, $7, $9, None) }
 | T_FOR T_LPAREN
     T_VAR separated_nonempty_list(T_COMMA,variable_declaration_no_in) T_SEMICOLON
     option(expression) T_SEMICOLON
     option(expression)
     T_RPAREN statement_bloc
     {
       J.For_statement (J.Right($4), $6, $8, $10, None)
     }
 | T_FOR T_LPAREN left_hand_side_expression T_IN expression T_RPAREN statement_bloc
     { J.ForIn_statement (J.Left $3,$5,$7,None) }
 | T_FOR T_LPAREN T_VAR variable_declaration_no_in T_IN expression T_RPAREN
     statement_bloc
     { J.ForIn_statement ( J.Right $4, $6, $8, None) }

variable_declaration_no_in:
 | variable option(initializer_no_in) { $1, $2 }

initializer_no_in:
 | T_ASSIGN assignment_expression_no_in { $2 }


continue_statement:
 | T_CONTINUE option(label) semicolon { J.Continue_statement $2 }

break_statement:
 | T_BREAK option(label) semicolon { J.Break_statement $2 }

return_statement:
 | T_RETURN option(expression) semicolon { J.Return_statement ($2) }

with_statement:
 | T_WITH T_LPAREN expression T_RPAREN statement { assert false }

switch_statement:
 | T_SWITCH T_LPAREN expression T_RPAREN T_LCURLY list(case_clause) option(default_clause) option(T_VIRTUAL_SEMICOLON) T_RCURLY
   { J.Switch_statement ($3, $6, $7) }

labelled_statement:
 | label T_COLON statement { J.Labelled_statement ($1, $3) }


throw_statement:
 | T_THROW expression semicolon { J.Throw_statement $2 }


try_statement:
 | T_TRY block catch option(finally) { J.Try_statement ($2, Some $3, $4,None) }
 | T_TRY block       finally { J.Try_statement ($2, None, Some $3,None) }


catch:
 | T_CATCH T_LPAREN variable T_RPAREN block { $3, $5 }


finally:
 | T_FINALLY block { $2 }

/*(*----------------------------*)*/
/*(*2 auxillary statements *)*/
/*(*----------------------------*)*/

case_clause:
 | T_CASE expression T_COLON list(statement) { $2, $4 }

default_clause:
 | T_DEFAULT T_COLON list(statement) { $3 }

/*(*************************************************************************)*/
/*(*1 function declaration *)*/
/*(*************************************************************************)*/

function_declaration:
 | T_FUNCTION v=variable T_LPAREN args=separated_list(T_COMMA,variable) T_RPAREN
     T_LCURLY b=function_body T_RCURLY
     { v, args, b, None }


function_expression:
 | T_FUNCTION v=option(variable) T_LPAREN args=separated_list(T_COMMA,variable) T_RPAREN
   T_LCURLY b=function_body T_RCURLY
   { J.EFun ((v, args, b),None) }

function_body:
 | l=list(source_element)  { l }

/*(*************************************************************************)*/
/*(*1 expression *)*/
/*(*************************************************************************)*/

expression:
 | assignment_expression { $1 }
 | expression T_COMMA assignment_expression { J.ESeq ($1, $3) }

assignment_expression:
 | conditional_expression { $1 }
 | left_hand_side_expression assignment_operator assignment_expression
   { J.EBin ($2, $1, $3) }

assignment_operator:
 | T_ASSIGN         { J.Eq }
 | T_MULT_ASSIGN    { J.StarEq }
 | T_DIV_ASSIGN     { J.SlashEq }
 | T_MOD_ASSIGN     { J.ModEq }
 | T_PLUS_ASSIGN    { J.PlusEq }
 | T_MINUS_ASSIGN   { J.MinusEq }
 | T_LSHIFT_ASSIGN  { J.LslEq }
 | T_RSHIFT_ASSIGN  { J.AsrEq }
 | T_RSHIFT3_ASSIGN { J.LsrEq }
 | T_BIT_AND_ASSIGN { J.BandEq }
 | T_BIT_XOR_ASSIGN { J.BxorEq }
 | T_BIT_OR_ASSIGN  { J.BorEq }

left_hand_side_expression:
 | new_expression  { $1 }
 | call_expression { $1 }

conditional_expression:
 | post_in_expression { $1 }
 | post_in_expression
     T_PLING assignment_expression
     T_COLON assignment_expression
     { J.ECond ($1, $3, $5) }

post_in_expression:
 | pre_in_expression { $1 }
 | post_in_expression T_LESS_THAN post_in_expression
   { bop J.Lt $1 $3 }
 | post_in_expression T_GREATER_THAN post_in_expression
   { bop J.Gt $1 $3 }
 | post_in_expression T_LESS_THAN_EQUAL post_in_expression
   { bop J.Le $1 $3 }
 | post_in_expression T_GREATER_THAN_EQUAL post_in_expression
   { bop J.Ge $1 $3 }
 | post_in_expression T_INSTANCEOF post_in_expression
   { bop J.InstanceOf $1 $3 }
 | post_in_expression T_IN post_in_expression
   { bop J.In $1 $3 }
 | post_in_expression T_EQUAL post_in_expression
   { bop J.EqEq $1 $3 }
 | post_in_expression T_NOT_EQUAL post_in_expression
   { bop J.NotEq $1 $3 }
 | post_in_expression T_STRICT_EQUAL post_in_expression
   { bop J.EqEqEq $1 $3 }
 | post_in_expression T_STRICT_NOT_EQUAL post_in_expression
   { bop J.NotEqEq $1 $3 }
 | post_in_expression T_BIT_AND post_in_expression
   { bop J.Band $1 $3 }
 | post_in_expression T_BIT_XOR post_in_expression
   { bop J.Bxor $1 $3 }
 | post_in_expression T_BIT_OR post_in_expression
   { bop J.Bor $1 $3 }
 | post_in_expression T_AND post_in_expression
   { bop J.And $1 $3 }
 | post_in_expression T_OR post_in_expression
   { bop J.Or $1 $3 }

pre_in_expression:
 | left_hand_side_expression
   { $1 }
 | pre_in_expression T_INCR
   { uop J.IncrA $1 }
 | pre_in_expression T_DECR
   { uop J.DecrA $1 }
 | T_DELETE pre_in_expression
   { uop J.Delete $2 }
 | T_VOID pre_in_expression
   { uop J.Void $2 }
 | T_TYPEOF pre_in_expression
   { uop J.Typeof $2 }
 | T_INCR pre_in_expression
   { uop J.IncrB $2 }
 | T_DECR pre_in_expression
   { uop J.DecrB $2 }
 | T_PLUS pre_in_expression
   { uop J.Pl $2 }
 | T_MINUS pre_in_expression
   { uop J.Neg $2}
 | T_BIT_NOT pre_in_expression
   { uop J.Bnot $2 }
 | T_NOT pre_in_expression
   { uop J.Not $2 }

 | pre_in_expression T_MULT pre_in_expression    { bop J.Mul $1 $3 }
 | pre_in_expression T_DIV pre_in_expression     { bop J.Div $1 $3 }
 | pre_in_expression T_MOD pre_in_expression     { bop J.Mod $1 $3 }
 | pre_in_expression T_PLUS pre_in_expression    { bop J.Plus $1 $3 }
 | pre_in_expression T_MINUS pre_in_expression   { bop J.Minus $1 $3 }
 | pre_in_expression T_LSHIFT pre_in_expression  { bop J.Lsl $1 $3 }
 | pre_in_expression T_RSHIFT pre_in_expression  { bop J.Asr $1 $3 }
 | pre_in_expression T_RSHIFT3 pre_in_expression { bop J.Lsr $1 $3 }

call_expression:
 | member_expression arguments                      { J.ECall($1, $2) }
 | call_expression arguments                        { J.ECall($1, $2) }
 | call_expression T_LBRACKET expression T_RBRACKET { J.EAccess ($1, $3) }
 | call_expression T_PERIOD identifier              { J.EDot ($1, $3) }

new_expression:
 | member_expression    { $1 }
 | T_NEW new_expression { J.ENew ($2,None) }

member_expression:
 | primary_expression                                 { $1 }
 | member_expression T_LBRACKET expression T_RBRACKET { J.EAccess ($1, $3) }
 | member_expression T_PERIOD identifier              { J.EDot($1,$3) }
 | T_NEW member_expression arguments
     { J.ENew($2, Some $3) }

primary_expression:
 | primary_expression_no_statement { $1 }
 | object_literal                  { J.EObj $1 }
 | function_expression             { $1 }

primary_expression_no_statement:
 | T_THIS          { J.EVar (var "this") }
 | variable        { J.EVar $1 }

 | null_literal    { J.EVar (var "null") }
 | boolean_literal { J.EBool $1 }
 | numeric_literal { J.ENum $1 }
 | string_literal  { J.EStr ($1, `Utf8) }
 /*(* marcel: this isn't an expansion of literal in ECMA-262... mistake? *)*/
 | regex_literal                { $1 }
 | array_literal                { $1 }
 | T_LPAREN expression T_RPAREN { $2 }

/*(*----------------------------*)*/
/*(*2 no in *)*/
/*(*----------------------------*)*/
expression_no_in:
 | assignment_expression_no_in { $1 }
 | expression_no_in T_COMMA assignment_expression_no_in { J.ESeq ($1, $3) }

assignment_expression_no_in:
 | conditional_expression_no_in { $1 }
 | left_hand_side_expression assignment_operator assignment_expression_no_in
     { J.EBin($2,$1,$3) }

conditional_expression_no_in:
 | post_in_expression_no_in { $1 }
 | post_in_expression_no_in
     T_PLING assignment_expression_no_in
     T_COLON assignment_expression_no_in
     { J.ECond ($1, $3, $5) }

post_in_expression_no_in:
 | pre_in_expression { $1 }
 | post_in_expression_no_in T_LESS_THAN post_in_expression
   { bop J.Lt $1 $3 }
 | post_in_expression_no_in T_GREATER_THAN post_in_expression
   { bop J.Gt $1 $3 }
 | post_in_expression_no_in T_LESS_THAN_EQUAL post_in_expression
   { bop J.Le $1 $3 }
 | post_in_expression_no_in T_GREATER_THAN_EQUAL post_in_expression
   { bop J.Ge $1 $3 }
 | post_in_expression_no_in T_INSTANCEOF post_in_expression
   { bop J.InstanceOf $1 $3 }
 | post_in_expression_no_in T_EQUAL post_in_expression
   { bop J.EqEq $1 $3 }
 | post_in_expression_no_in T_NOT_EQUAL post_in_expression
   { bop J.NotEq $1 $3 }
 | post_in_expression_no_in T_STRICT_EQUAL post_in_expression
   { bop J.EqEqEq $1 $3 }
 | post_in_expression_no_in T_STRICT_NOT_EQUAL post_in_expression
   { bop J.NotEqEq $1 $3 }
 | post_in_expression_no_in T_BIT_AND post_in_expression
   { bop J.Band $1 $3 }
 | post_in_expression_no_in T_BIT_XOR post_in_expression
   { bop J.Bxor $1 $3 }
 | post_in_expression_no_in T_BIT_OR post_in_expression
   { bop J.Bor $1 $3 }
 | post_in_expression_no_in T_AND post_in_expression
   { bop J.And $1 $3 }
 | post_in_expression_no_in T_OR post_in_expression
   { bop J.Or $1 $3 }

/*(*----------------------------*)*/
/*(*2 (no statement)*)*/
/*(*----------------------------*)*/
expression_no_statement:
 | assignment_expression_no_statement { $1 }
 | expression_no_statement T_COMMA assignment_expression { J.ESeq($1,$3) }

assignment_expression_no_statement:
 | conditional_expression_no_statement { $1 }
 | left_hand_side_expression_no_statement assignment_operator assignment_expression
     { J.EBin ($2,$1,$3) }

conditional_expression_no_statement:
 | post_in_expression_no_statement { $1 }
 | post_in_expression_no_statement
     T_PLING assignment_expression
     T_COLON assignment_expression
     { J.ECond ($1, $3, $5) }


post_in_expression_no_statement:
 | pre_in_expression_no_statement { $1 }
 | post_in_expression_no_statement T_LESS_THAN post_in_expression
   { bop J.Lt $1 $3 }
 | post_in_expression_no_statement T_GREATER_THAN post_in_expression
   { bop J.Gt $1 $3 }
 | post_in_expression_no_statement T_LESS_THAN_EQUAL post_in_expression
   { bop J.Le $1 $3 }
 | post_in_expression_no_statement T_GREATER_THAN_EQUAL post_in_expression
   { bop J.Ge $1 $3 }
 | post_in_expression_no_statement T_INSTANCEOF post_in_expression
   { bop J.InstanceOf $1 $3 }
 | post_in_expression_no_statement T_IN post_in_expression
   { bop J.In $1 $3 }
 | post_in_expression_no_statement T_EQUAL post_in_expression
   { bop J.EqEq $1 $3 }
 | post_in_expression_no_statement T_NOT_EQUAL post_in_expression
   { bop J.NotEq $1 $3 }
 | post_in_expression_no_statement T_STRICT_EQUAL post_in_expression
   { bop J.EqEqEq $1 $3 }
 | post_in_expression_no_statement T_STRICT_NOT_EQUAL post_in_expression
   { bop J.NotEqEq $1 $3 }
 | post_in_expression_no_statement T_BIT_AND post_in_expression
   { bop J.Band $1 $3 }
 | post_in_expression_no_statement T_BIT_XOR post_in_expression
   { bop J.Bxor $1 $3 }
 | post_in_expression_no_statement T_BIT_OR post_in_expression
   { bop J.Bor $1 $3 }
 | post_in_expression_no_statement T_AND post_in_expression
   { bop J.And $1 $3 }
 | post_in_expression_no_statement T_OR post_in_expression
   { bop J.Or $1 $3 }


pre_in_expression_no_statement:
 | left_hand_side_expression_no_statement
   { $1 }
 | pre_in_expression_no_statement T_INCR
   { uop J.IncrA $1 }
 | pre_in_expression_no_statement T_DECR
   { uop J.DecrA $1 }
 | T_DELETE pre_in_expression
   { uop J.Delete $2 }
 | T_VOID pre_in_expression
   { uop J.Void $2 }
 | T_TYPEOF pre_in_expression
   { uop J.Typeof $2 }
 | T_INCR pre_in_expression
   { uop J.IncrB $2 }
 | T_DECR pre_in_expression
   { uop J.DecrB $2 }
 | T_PLUS pre_in_expression
   { uop J.Pl $2 }
 | T_MINUS pre_in_expression
   { uop J.Neg $2}
 | T_BIT_NOT pre_in_expression
   { uop J.Bnot $2 }
 | T_NOT pre_in_expression
   { uop J.Not $2 }

 | pre_in_expression_no_statement T_MULT pre_in_expression    { bop J.Mul $1 $3 }
 | pre_in_expression_no_statement T_DIV pre_in_expression     { bop J.Div $1 $3 }
 | pre_in_expression_no_statement T_MOD pre_in_expression     { bop J.Mod $1 $3 }
 | pre_in_expression_no_statement T_PLUS pre_in_expression    { bop J.Plus $1 $3 }
 | pre_in_expression_no_statement T_MINUS pre_in_expression   { bop J.Minus $1 $3 }
 | pre_in_expression_no_statement T_LSHIFT pre_in_expression  { bop J.Lsl $1 $3 }
 | pre_in_expression_no_statement T_RSHIFT pre_in_expression  { bop J.Asr $1 $3 }
 | pre_in_expression_no_statement T_RSHIFT3 pre_in_expression { bop J.Lsr $1 $3 }

left_hand_side_expression_no_statement:
 | new_expression_no_statement { $1 }
 | call_expression_no_statement { $1 }

new_expression_no_statement:
 | member_expression_no_statement { $1 }
 | T_NEW new_expression { J.ENew ($2,None) }

call_expression_no_statement:
 | member_expression_no_statement arguments
   { J.ECall($1, $2) }
 | call_expression_no_statement arguments
   { J.ECall($1, $2) }
 | call_expression_no_statement T_LBRACKET expression T_RBRACKET
   { J.EAccess($1, $3) }
 | call_expression_no_statement T_PERIOD identifier
   { J.EDot($1,$3) }

member_expression_no_statement:
 | primary_expression_no_statement                                 { $1 }
 | member_expression_no_statement T_LBRACKET expression T_RBRACKET
   { J.EAccess($1, $3) }
 | member_expression_no_statement T_PERIOD identifier
   { J.EDot($1, $3) }
 | T_NEW member_expression arguments
   { J.ENew($2,Some $3) }

/*(*----------------------------*)*/
/*(*2 scalar *)*/
/*(*----------------------------*)*/
null_literal:
 | T_NULL { }

boolean_literal:
 | T_TRUE  { true }
 | T_FALSE { false }

numeric_literal:
 | T_NUMBER {
   match $1 with
   | _,`Float f,_  -> f
   | _,`Int i, _ -> float_of_int i }

regex_literal:
 | T_REGEX {
   let s,_ = $1 in
   let len = String.length s in
   let regexp,option =
     if s.[len - 1] = '/'
     then String.sub s 1 (len - 2),None
     else
       let i = String.rindex s '/' in
       String.sub s 1 (i - 1),Some (String.sub s (i+1) (len - i - 1))
   in
   J.ERegexp (regexp,option) }
   (* J.ENew(J.EVar (var "RegExp"), Some (List.map (fun s -> J.EStr (s,`Bytes)) args)) } *)

string_literal:
 | str=T_STRING { let s,_ = str in s}

/*(*----------------------------*)*/
/*(*2 array *)*/
/*(*----------------------------*)*/

array_literal:
 | T_LBRACKET elison T_RBRACKET              { J.EArr $2 }
 | T_LBRACKET        T_RBRACKET              { J.EArr [] }
 | T_LBRACKET element_list T_RBRACKET        { J.EArr $2 }
 | T_LBRACKET element_list elison T_RBRACKET { J.EArr ($2 @ $3) }


element_list:
 | elison   assignment_expression { $1 @ [Some $2] }
 |          assignment_expression { [Some $1] }
 | element_list   elison   assignment_expression { $1 @ $2 @ [Some $3] }



separated_nonempty_list2(sep,X):
| x = X { [ x ] }
| x = X; sep { [ x ] }
| x = X; sep; xs = separated_nonempty_list2(sep, X) { x :: xs }

object_literal:
 | T_LCURLY T_RCURLY { [] }
 | T_LCURLY
     l = separated_nonempty_list2(
       T_COMMA,
       separated_pair(property_name,T_COLON,assignment_expression)
     )
     option(T_VIRTUAL_SEMICOLON)
   T_RCURLY { l }

/*(*----------------------------*)*/
/*(*2 variable *)*/
/*(*----------------------------*)*/

/*(*----------------------------*)*/
/*(*2 function call *)*/
/*(*----------------------------*)*/

arguments:
 | T_LPAREN l=separated_list(T_COMMA,assignment_expression) T_RPAREN { l }

/*(*----------------------------*)*/
/*(*2 auxillary bis *)*/
/*(*----------------------------*)*/

/*(*************************************************************************)*/
/*(*1 Entities, names *)*/
/*(*************************************************************************)*/
identifier:
 | T_IDENTIFIER { let s,_ = $1 in s  }

variable:
 | i=identifier { var i }

label:
 | identifier { J.Label.of_string $1 }

property_name:
 | i=identifier      { J.PNI i }
 | s=string_literal  { J.PNS s }
 | n=numeric_literal { J.PNN n }

/*(*************************************************************************)*/
/*(*1 xxx_opt, xxx_list *)*/
/*(*************************************************************************)*/

semicolon:
 | T_SEMICOLON         { Some $1 }
 | T_VIRTUAL_SEMICOLON { None }

elison:
 | T_COMMA { [] }
 | elison T_COMMA { $1 @ [None] }
