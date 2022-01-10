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

open Javascript

let var pi name = ident ~loc:(Pi pi) name

%}

(*************************************************************************)
(* 1 Tokens                                                              *)
(*************************************************************************)

(*-----------------------------------------*)
(* 2 the normal tokens                     *)
(*-----------------------------------------*)

(* Tokens with a value *)
%token<string * Parse_info.t> T_NUMBER
%token<string * Parse_info.t> T_IDENTIFIER
%token<string * Parse_info.t * int> T_STRING
%token<string * Parse_info.t> T_REGEX

(* Keywords tokens *)
%token <Parse_info.t>
T_FUNCTION T_IF T_RETURN T_SWITCH T_THIS T_THROW T_TRY
T_VAR T_WHILE T_WITH T_NULL T_FALSE T_TRUE
T_BREAK T_CASE T_CATCH T_CONTINUE T_DEFAULT T_DO T_FINALLY T_FOR
T_DEBUGGER

%token <Parse_info.t> T_ELSE

%token <Parse_info.t> T_NEW

(* Syntax *)
%token <Parse_info.t>
T_LCURLY T_RCURLY
T_LPAREN T_RPAREN
T_LBRACKET T_RBRACKET
T_SEMICOLON
T_COMMA
T_SPREAD
T_PERIOD

(* Operators *)
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
T_NOT T_BIT_NOT T_INCR T_DECR T_INCR_NB T_DECR_NB T_DELETE T_TYPEOF T_VOID

(*-----------------------------------------*)
(* 2 extra tokens:                         *)
(*-----------------------------------------*)

%token <Parse_info.t> T_VIRTUAL_SEMICOLON
%token <Js_token.Annot.t> TAnnot
%token <string * Parse_info.t> TUnknown
%token <string * Parse_info.t> TComment
%token <string * Parse_info.t> TCommentLineDirective


(* classic *)
%token <Parse_info.t> EOF

(*-----------------------------------------*)
(* 2 priorities                            *)
(*-----------------------------------------*)

(* Special if / else associativity*)
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
%right T_NOT T_BIT_NOT T_INCR T_DECR T_INCR_NB T_DECR_NB T_DELETE T_TYPEOF T_VOID

(*************************************************************************)
(* 1 Rules type declaration                                              *)
(*************************************************************************)

%start <Javascript.program_with_annots> program
%start <Javascript.expression> standalone_expression

%%

(*************************************************************************)
(* 1 Toplevel                                                            *)
(*************************************************************************)

program:
 | l=source_element_with_annot* EOF { l }

standalone_expression:
 | e=expression EOF { e }

annot:
  | a=TAnnot { a }

source_element_with_annot:
 | annots=annot* s=source_element {s,annots}

source_element:
 | statement
   { let statement, pi = $1 in Statement statement, pi }
 | function_declaration
   { let declaration, pi = $1 in Function_declaration declaration, pi }

(*************************************************************************)
(* 1 statement                                                           *)
(*************************************************************************)

statement_no_semi:
 | block=curly_block(statement*)
   { let statements, pi_start, _pi_end = block in
     Block statements, Pi pi_start }
 | s=if_statement
 | s=while_statement
 | s=for_statement
 | s=for_in_statement
 | s=with_statement
 | s=switch_statement
 | s=try_statement
 | s=labeled_statement
 | s=empty_statement { s }

statement_need_semi:
 | s=variable_statement
 | s=expression_statement
 | s=do_while_statement
 | s=continue_statement
 | s=break_statement
 | s=return_statement
 | s=throw_statement
 | s=debugger_statement { s }

statement:
 | s=statement_no_semi { s }
 | s=statement_need_semi either(T_SEMICOLON, T_VIRTUAL_SEMICOLON) { s }

labeled_statement:
| l=label T_COLON s=statement { Labelled_statement (fst l, s), Pi (snd l)}

block:
 | block=curly_block(statement*)
   { let statements, _, _ = block in statements }

variable_statement:
 | pi=T_VAR list=separated_nonempty_list(T_COMMA, pair(variable, initializer_?))
   { Variable_statement list, Pi pi }

initializer_:
 | pi=T_ASSIGN e=assignment_expression { e, Pi pi }

empty_statement:
 | pi=T_SEMICOLON { Empty_statement, Pi pi }

debugger_statement:
 | pi=T_DEBUGGER { Debugger_statement, Pi pi }

expression_statement:
 | expression_no_statement { Expression_statement $1, N }

if_statement:
 | pi=T_IF condition=parenthesised(expression) t=statement T_ELSE e=statement
     { (If_statement (condition, t, Some e), Pi pi) }
 | pi=T_IF condition=parenthesised(expression) t=statement %prec p_IF
     { (If_statement (condition, t, None), Pi pi) }

do_while_statement:
  | pi=T_DO body=statement T_WHILE condition=parenthesised(expression)
    { (Do_while_statement (body, condition), Pi pi) }

while_statement:
 | pi=T_WHILE condition=parenthesised(expression) body=statement
     { (While_statement (condition, body), Pi pi) }

for_statement:
 | pi=T_FOR T_LPAREN initial=expression_no_in?
   T_SEMICOLON condition=expression? T_SEMICOLON increment=expression?
   T_RPAREN statement=statement
   { For_statement (Left initial, condition, increment, statement), Pi pi }
 | pi=T_FOR T_LPAREN T_VAR
   initial=separated_nonempty_list(T_COMMA, pair(variable, initializer_no_in?))
   T_SEMICOLON condition=expression?  T_SEMICOLON increment=expression?
   T_RPAREN statement=statement
   { For_statement (Right initial, condition, increment, statement), Pi pi }

for_in_statement:
 | pi=T_FOR T_LPAREN left=left_hand_side_expression
   T_IN right=expression T_RPAREN body=statement
   { ForIn_statement (Left left, right, body), Pi pi }
 | pi=T_FOR T_LPAREN T_VAR left=pair(variable, initializer_no_in?)
   T_IN right=expression T_RPAREN body=statement
   { ForIn_statement (Right left, right, body), Pi pi }

initializer_no_in:
 | T_ASSIGN assignment_expression_no_in { $2, Pi $1 }

continue_statement:
 | pi=T_CONTINUE label? { (Continue_statement (Stdlib.Option.map ~f:fst $2),Pi pi) }

break_statement:
 | pi=T_BREAK label? { (Break_statement (Stdlib.Option.map ~f:fst $2), Pi pi) }

return_statement:
 | pi=T_RETURN expression? { (Return_statement $2, Pi pi) }

with_statement:
 | T_WITH parenthesised(expression) statement { assert false }

switch_statement:
 | pi=T_SWITCH subject=parenthesised(expression)
   T_LCURLY pair=pair(case_clause*, pair(default_clause, case_clause*)?) T_RCURLY
   { let switch = match pair with
       | cases, None ->
         Switch_statement (subject, cases, None, [])
       | cases, Some (default, more_cases) ->
         Switch_statement (subject, cases, Some default, more_cases)
      in switch, Pi pi }

throw_statement:
 | pi=T_THROW expression { (Throw_statement $2, Pi pi) }

try_statement:
 | pi=T_TRY block catch finally? { (Try_statement ($2, Some $3, $4), Pi pi) }
 | pi=T_TRY block       finally { (Try_statement ($2, None, Some $3), Pi pi) }

catch:
 | T_CATCH pair=pair(parenthesised(variable), block) { pair }

finally:
 | T_FINALLY block { $2 }

(*----------------------------*)
(* 2 auxiliary statements     *)
(*----------------------------*)

case_clause:
 | T_CASE pair=separated_pair(expression, T_COLON, statement*) { pair }

default_clause:
 | T_DEFAULT T_COLON list=statement* { list }

(*************************************************************************)
(* 1 function declaration                                                *)
(*************************************************************************)

function_declaration:
 | pi=T_FUNCTION name=variable args=parenthesised(separated_list(T_COMMA, variable))
   block=curly_block(source_element*)
   { let elements, _pi_start, pi_end = block in
     (name, args, elements, Pi pi_end), Pi pi }

function_expression:
 | pi=T_FUNCTION name=variable? args=parenthesised(separated_list(T_COMMA, variable))
   block=curly_block(source_element*)
   { let elements, _pi_start, _pi_end = block in
     pi, EFun (name, args, elements, Pi pi) }

(*************************************************************************)
(* 1 expression                                                          *)
(*************************************************************************)

expression:
 | assignment_expression { $1 }
 | expression T_COMMA assignment_expression { ESeq ($1, $3) }

assignment_expression:
 | conditional_expression { $1 }
 | left_hand_side_expression assignment_operator assignment_expression
   { EBin ($2, $1, $3) }

left_hand_side_expression:
 | new_expression  { snd $1 }
 | call_expression { snd $1 }

conditional_expression:
 | post_in_expression { $1 }
 | ternary(post_in_expression, assignment_expression) { $1 }

ternary(condition, consequence):
 | condition=condition T_PLING consequence=consequence
                       T_COLON alternative=consequence
   { ECond (condition, consequence, alternative) }

post_in_expression:
 | pre_in_expression { $1 }
 | left=post_in_expression
   op=comparison_or_logical_or_bit_operator
   right=post_in_expression
   { EBin (op, left, right) }

pre_in_expression:
 | left_hand_side_expression
   { $1 }
 | e=pre_in_expression op=postfix_operator
 | op=prefix_operator e=pre_in_expression
   { EUn (op, e) }
 | left=pre_in_expression
   op=arithmetic_or_shift_operator
   right=pre_in_expression
   { EBin (op, left, right) }

call_expression:
 | member_expression arguments
     { let (start, e) = $1 in (start, ECall(e, $2, Pi start)) }
 | call_expression arguments
     { let (start, e) = $1 in (start, ECall(e, $2, Pi start)) }
 | call_expression T_LBRACKET expression T_RBRACKET
     { let (start, e) = $1 in (start, EAccess (e, $3)) }
 | call_expression T_PERIOD identifier_or_kw
     { let (start, e) = $1 in (start, EDot (e, $3)) }

new_expression:
 | member_expression    { $1 }
 | pi=T_NEW new_expression { (pi, ENew (snd $2,None)) }

member_expression:
 | e=primary_expression
     { e }
 | member_expression T_LBRACKET e2=expression T_RBRACKET
     { let (start, e1) = $1 in (start, EAccess (e1,e2)) }
 | member_expression T_PERIOD i=identifier_or_kw
     { let (start, e1) = $1 in (start, EDot(e1,i)) }
 | pi=T_NEW e1=member_expression a=arguments
     { (pi, ENew(snd e1, Some a)) }

primary_expression:
 | e=primary_expression_no_statement
 | e=object_literal
 | e=function_expression { e }

primary_expression_no_statement:
 | pi=T_THIS         { (pi, EVar (var pi "this")) }
 | variable_with_loc { let (i, pi) = $1 in (pi, EVar i) }
 | n=null_literal    { n }
 | b=boolean_literal { b }
 | numeric_literal   { let (start, n) = $1 in (start, ENum (Num.of_string_unsafe n)) }
 | T_STRING          { let (s, start, _len) = $1 in (start, EStr (s, `Utf8)) }
 | r=regex_literal                { r }
 | a=array_literal                { a }
 | pi=T_LPAREN e=expression T_RPAREN { (pi, e) }

(*----------------------------*)
(* 2 no in                    *)
(*----------------------------*)

expression_no_in:
 | assignment_expression_no_in { $1 }
 | expression_no_in T_COMMA assignment_expression_no_in { ESeq ($1, $3) }

assignment_expression_no_in:
 | conditional_expression_no_in { $1 }
 | left_hand_side_expression assignment_operator assignment_expression_no_in
     { EBin($2,$1,$3) }

conditional_expression_no_in:
 | post_in_expression_no_in { $1 }
 | ternary(post_in_expression_no_in, assignment_expression_no_in) { $1 }

post_in_expression_no_in:
 | pre_in_expression { $1 }
 | left=post_in_expression_no_in
   op=comparison_or_logical_or_bit_operator_except_in
   right=post_in_expression
   { EBin (op, left, right) }

(*----------------------------*)
(* 2 (no statement)           *)
(*----------------------------*)

expression_no_statement:
 | assignment_expression_no_statement { $1 }
 | expression_no_statement T_COMMA assignment_expression { ESeq($1,$3) }

assignment_expression_no_statement:
 | conditional_expression_no_statement { $1 }
 | left_hand_side_expression_no_statement assignment_operator assignment_expression
   { EBin ($2,$1,$3) }

conditional_expression_no_statement:
 | post_in_expression_no_statement { $1 }
 | ternary(post_in_expression_no_statement, assignment_expression) { $1 }

post_in_expression_no_statement:
 | pre_in_expression_no_statement { $1 }
 | left=post_in_expression_no_statement
   op=comparison_or_logical_or_bit_operator
   right=post_in_expression
   { EBin (op, left, right) }

pre_in_expression_no_statement:
 | left_hand_side_expression_no_statement
   { $1 }
 | e=pre_in_expression_no_statement op=postfix_operator
 | op=prefix_operator e=pre_in_expression
   { EUn (op, e) }
 | left=pre_in_expression_no_statement
   op=arithmetic_or_shift_operator
   right=pre_in_expression
   { EBin (op, left, right) }

left_hand_side_expression_no_statement:
 | new_expression_no_statement { snd $1 }
 | call_expression_no_statement { snd $1 }

new_expression_no_statement:
 | member_expression_no_statement { $1 }
 | pi=T_NEW new_expression { (pi, ENew (snd $2,None)) }

call_expression_no_statement:
 | member_expression_no_statement arguments
   { let (start, e) = $1 in (start, ECall(e, $2, Pi start)) }
 | call_expression_no_statement arguments
   { let (start, e) = $1 in (start, ECall(e, $2, Pi start)) }
 | call_expression_no_statement T_LBRACKET expression T_RBRACKET
   { let (start, e) = $1 in (start, EAccess(e, $3)) }
 | call_expression_no_statement T_PERIOD identifier_or_kw
   { let (start, e) = $1 in (start, EDot(e,$3)) }

member_expression_no_statement:
 | e=primary_expression_no_statement
   { e }
 | member_expression_no_statement T_LBRACKET e2=expression T_RBRACKET
   { let (start, e1) = $1 in (start, EAccess(e1, e2)) }
 | member_expression_no_statement T_PERIOD i=identifier_or_kw
   { let (start, e1) = $1 in (start, EDot(e1,i)) }
 | pi=T_NEW e=member_expression a=arguments
   { (pi, ENew(snd e,Some a)) }

(*----------------------------*)
(* 2 scalar                   *)
(*----------------------------*)

null_literal:
 | pi=T_NULL { (pi, EVar (var pi "null")) }

boolean_literal:
 | pi=T_TRUE  { (pi, EBool true) }
 | pi=T_FALSE { (pi, EBool false) }

numeric_literal:
 | T_NUMBER { let (f, pi) = $1 in (pi, f) }

regex_literal:
 | T_REGEX {
   let (s, pi) = $1 in
   let len = String.length s in
   let regexp, option =
     if s.[len - 1] = '/'
     then String.sub s 1 (len - 2),None
     else
       let i = String.rindex s '/' in
       String.sub s 1 (i - 1),Some (String.sub s (i+1) (len - i - 1))
   in
   (pi, ERegexp (regexp, option)) }

(*----------------------------*)
(* 2 array                    *)
(*----------------------------*)

array_literal:
 | pi=T_LBRACKET elison T_RBRACKET
     { (pi, EArr $2) }
 | pi=T_LBRACKET        T_RBRACKET
     { (pi, EArr []) }
 | pi=T_LBRACKET element_list T_RBRACKET
     { (pi, EArr $2) }
 | pi=T_LBRACKET element_list_rev elison_rev T_RBRACKET
     { (pi, EArr (List.rev_append $2 (List.rev $3))) }

element_list:
 | element_list_rev { List.rev $1 }

element_list_rev:
 | elison_rev assignment_expression { (Some $2)::$1 }
 |            assignment_expression { [Some $1] }
 | element_list_rev elison assignment_expression { (Some $3) :: (List.rev_append $2 $1) }

object_literal:
 | block=curly_block(empty)
   { let _pairs, pi_start, _pi_end = block in pi_start, EObj [] }
 | block=curly_block(separated_or_terminated_list(T_COMMA, object_key_value))
   { let pairs, pi_start, _pi_end = block in pi_start, EObj pairs }

object_key_value:
 | pair=separated_pair(property_name, T_COLON, assignment_expression) { pair }

(*----------------------------*)
(* 2 variable                 *)
(*----------------------------*)

(*----------------------------*)
(* 2 function call            *)
(*----------------------------*)

arg:
 | T_SPREAD arg=assignment_expression { arg, `Spread }
 | arg=assignment_expression { arg, `Not_spread }

arguments:
 | args=parenthesised(separated_list(T_COMMA, arg)) { args }

(*----------------------------*)
(* 2 auxiliary bis            *)
(*----------------------------*)

(*************************************************************************)
(* 1 Entities, names                                                     *)
(*************************************************************************)

identifier_or_kw:
   | T_IDENTIFIER { fst $1 }
   | T_CATCH { "catch" }
   | T_FINALLY { "finally" }
   | T_IN { "in" }
   | T_INSTANCEOF { "instanceof" }
   | T_ELSE { "else" }
   | T_BREAK { "break" }
   | T_CASE { "case" }
   | T_CONTINUE { "continue" }
   | T_DEFAULT { "default" }
   | T_DELETE { "delete" }
   | T_DO { "do" }
   | T_FOR { "for" }
   | T_FUNCTION { "function" }
   | T_IF { "if" }
   | T_NEW { "new" }
   | T_RETURN { "return" }
   | T_SWITCH { "switch" }
   | T_THIS { "this" }
   | T_THROW { "throw" }
   | T_TRY { "try" }
   | T_TYPEOF { "typeof" }
   | T_VAR { "var" }
   | T_VOID { "void" }
   | T_WHILE { "while" }
   | T_WITH { "with" }
   | T_NULL { "null" }
   | T_FALSE { "false" }
   | T_TRUE { "true" }
   | T_DEBUGGER { "debugger" }

variable:
 | i=variable_with_loc { fst i }

variable_with_loc:
 | i=T_IDENTIFIER { let name, pi = i in var pi name, pi }

label:
 | T_IDENTIFIER { Label.of_string (fst $1), snd $1 }

property_name:
 | i=identifier_or_kw { PNI i }
 | s=T_STRING         {
    let s, _info, _len = s in PNS s }
 | n=numeric_literal  { PNN (Num.of_string_unsafe (snd n)) }

(*************************************************************************)
(* 1 xxx_opt, xxx_list                                                   *)
(*************************************************************************)

elison_rev:
 | T_COMMA { [] }
 | elison T_COMMA { None :: $1 }

elison: elison_rev {$1}
 (* | elison_rev { List.rev $1} *)

curly_block(X):
 | pi1=T_LCURLY x=X pi2=T_RCURLY { (x, pi1, pi2) }

(*----------------------------*)
(* Infix binary operators     *)
(*----------------------------*)

%inline comparison_or_logical_or_bit_operator_except_in:
 | T_LESS_THAN          { Lt         }
 | T_GREATER_THAN       { Gt         }
 | T_LESS_THAN_EQUAL    { Le         }
 | T_GREATER_THAN_EQUAL { Ge         }
 | T_INSTANCEOF         { InstanceOf }
 | T_EQUAL              { EqEq       }
 | T_NOT_EQUAL          { NotEq      }
 | T_STRICT_EQUAL       { EqEqEq     }
 | T_STRICT_NOT_EQUAL   { NotEqEq    }
 | T_BIT_AND            { Band       }
 | T_BIT_XOR            { Bxor       }
 | T_BIT_OR             { Bor        }
 | T_AND                { And        }
 | T_OR                 { Or         }

%inline comparison_or_logical_or_bit_operator:
 | op=comparison_or_logical_or_bit_operator_except_in { op }
 | T_IN { In }

%inline arithmetic_or_shift_operator:
 | T_MULT    { Mul   }
 | T_DIV     { Div   }
 | T_MOD     { Mod   }
 | T_PLUS    { Plus  }
 | T_MINUS   { Minus }
 | T_LSHIFT  { Lsl   }
 | T_RSHIFT  { Asr   }
 | T_RSHIFT3 { Lsr   }

%inline prefix_operator:
 | T_DELETE  { Delete }
 | T_VOID    { Void   }
 | T_TYPEOF  { Typeof }
 | T_INCR    { IncrB  }
 | T_INCR_NB { IncrB  }
 | T_DECR    { DecrB  }
 | T_DECR_NB { DecrB  }
 | T_PLUS    { Pl     }
 | T_MINUS   { Neg    }
 | T_BIT_NOT { Bnot   }
 | T_NOT     { Not    }

postfix_operator:
 | T_INCR_NB { IncrA }
 | T_DECR_NB { DecrA }

assignment_operator:
 | T_ASSIGN         { Eq }
 | T_MULT_ASSIGN    { StarEq }
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

(* Library definitions *)

either(a, b): a { $1 } | b { $1 }

empty: {}

%inline parenthesised(ITEM): T_LPAREN item=ITEM T_RPAREN { item }

separated_or_terminated_list(separator, X):
 | x=X { [x] }
 | x=X separator { [x] }
 | x=X separator xs=separated_or_terminated_list(separator, X) { x :: xs }
