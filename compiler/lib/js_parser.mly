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

let var pi name = ident_unsafe ~loc:(pi) name

let pi pos = (Parse_info.t_of_pos pos)

let p pos = Pi (pi pos)

let utf8_s = Stdlib.Utf8_string.of_string_exn

%}

(*************************************************************************)
(* 1 Tokens                                                              *)
(*************************************************************************)

(*-----------------------------------------*)
(* 2 the normal tokens                     *)
(*-----------------------------------------*)

(* Tokens with a value *)
%token<Js_token.number_type * string> T_NUMBER
%token<Js_token.bigint_type * string> T_BIGINT
%token<Stdlib.Utf8_string.t * string> T_IDENTIFIER
%token<Stdlib.Utf8_string.t * int> T_STRING
%token<Stdlib.Utf8_string.t * string> T_REGEXP
%token<Stdlib.Utf8_string.t> T_TEMPLATE_PART
(* Keywords tokens *)
%token
T_FUNCTION T_IF T_RETURN T_SWITCH T_THIS T_THROW T_TRY
T_VAR T_WHILE T_WITH T_NULL T_FALSE T_TRUE
T_BREAK T_CASE T_CATCH T_CONTINUE T_DEFAULT T_DO T_FINALLY T_FOR
T_DEBUGGER
T_ASYNC
T_AWAIT
T_YIELD
T_LET
T_CONST
T_CLASS
T_SUPER
T_EXPORT
T_PACKAGE
T_INTERFACE
T_IMPLEMENTS
T_DECLARE
T_TYPE
T_PUBLIC
T_PRIVATE
T_OPAQUE
T_PROTECTED
T_EXTENDS
T_STATIC
T_ENUM
T_IMPORT
T_OF

%token T_ELSE

%token T_NEW

(* Syntax *)
%token 
T_LCURLY T_RCURLY
T_LPAREN T_RPAREN
T_LBRACKET T_RBRACKET
T_SEMICOLON
T_COMMA
T_ELLIPSIS
T_PERIOD

(* Operators *)
%token 
T_RSHIFT3_ASSIGN T_RSHIFT_ASSIGN T_LSHIFT_ASSIGN
T_BIT_XOR_ASSIGN T_BIT_OR_ASSIGN T_BIT_AND_ASSIGN T_MOD_ASSIGN T_DIV_ASSIGN
T_MULT_ASSIGN T_MINUS_ASSIGN T_PLUS_ASSIGN T_ASSIGN
T_OR_ASSIGN T_AND_ASSIGN T_EXP_ASSIGN
T_EXP T_NULLISH_ASSIGN T_PLING_PERIOD T_PLING_PLING T_AT T_POUND

%token 
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
T_ARROW
(*-----------------------------------------*)
(* 2 extra tokens:                         *)
(*-----------------------------------------*)

%token T_VIRTUAL_SEMICOLON
%token <Js_token.Annot.t> TAnnot
%token <string> T_ERROR
%token <string> TComment
%token <string> TCommentLineDirective


(* classic *)
%token T_EOF

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
 | l=source_element_with_annot* T_EOF { l }

standalone_expression:
 | e=expression T_EOF { e }

annot:
  | a=TAnnot { a, pi $symbolstartpos }

source_element_with_annot:
 | annots=annot* s=source_element {s,annots}

source_element:
 | statement
   { let statement,pos = $1 in Statement statement, pos }
 | function_declaration
   { let declaration = $1 in Function_declaration declaration, p $symbolstartpos }

(*************************************************************************)
(* 1 statement                                                           *)
(*************************************************************************)

statement_no_semi:
 | block=curly_block(statement*)
   { let statements,_ = block in
     Block statements }
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
 | s=statement_no_semi { (s : statement), p $symbolstartpos }
 | s=statement_need_semi either(T_SEMICOLON, T_VIRTUAL_SEMICOLON) { (s : statement), p $symbolstartpos }

labeled_statement:
| l=label T_COLON s=statement { Labelled_statement (l, s)}

block:
 | block=curly_block(statement*)
   { let statements,_ = block in statements }

variable_statement:
 | T_VAR list=separated_nonempty_list(T_COMMA, pair(variable, initializer_?))
   { Variable_statement list }

initializer_:
 | T_ASSIGN e=assignment_expression { e, p $symbolstartpos }

empty_statement:
 | T_SEMICOLON { Empty_statement }

debugger_statement:
 | T_DEBUGGER { Debugger_statement }

expression_statement:
 | expression_no_statement { Expression_statement $1 }

if_statement:
 | T_IF condition=parenthesised(expression) t=statement T_ELSE e=statement
     { If_statement (condition, t, Some e) }
 | T_IF condition=parenthesised(expression) t=statement %prec p_IF
     { If_statement (condition, t, None) }

do_while_statement:
  | T_DO body=statement T_WHILE condition=parenthesised(expression)
    { Do_while_statement (body, condition) }

while_statement:
 | T_WHILE condition=parenthesised(expression) body=statement
     { While_statement (condition, body) }

for_statement:
 | T_FOR T_LPAREN initial=expression_no_in?
   T_SEMICOLON condition=expression? T_SEMICOLON increment=expression?
   T_RPAREN statement=statement
   { For_statement (Left initial, condition, increment, statement) }
 | T_FOR T_LPAREN T_VAR
   initial=separated_nonempty_list(T_COMMA, pair(variable, initializer_no_in?))
   T_SEMICOLON condition=expression?  T_SEMICOLON increment=expression?
   T_RPAREN statement=statement
   { For_statement (Right initial, condition, increment, statement) }

for_in_statement:
 | T_FOR T_LPAREN left=left_hand_side_expression
   T_IN right=expression T_RPAREN body=statement
   { ForIn_statement (Left left, right, body) }
 | T_FOR T_LPAREN T_VAR left=pair(variable, initializer_no_in?)
   T_IN right=expression T_RPAREN body=statement
   { ForIn_statement (Right left, right, body) }

initializer_no_in:
 | T_ASSIGN e=assignment_expression_no_in { e, p $symbolstartpos }

continue_statement:
 | T_CONTINUE l=label? { (Continue_statement (l)) }

break_statement:
 | T_BREAK l=label? { (Break_statement (l)) }

return_statement:
 | T_RETURN e=expression? { (Return_statement e) }

with_statement:
 | T_WITH parenthesised(expression) statement { assert false }

switch_statement:
 | T_SWITCH subject=parenthesised(expression)
   T_LCURLY pair=pair(case_clause*, pair(default_clause, case_clause*)?) T_RCURLY
   { let switch = match pair with
       | cases, None ->
         Switch_statement (subject, cases, None, [])
       | cases, Some (default, more_cases) ->
         Switch_statement (subject, cases, Some default, more_cases)
      in switch }

throw_statement:
 | T_THROW e=expression { (Throw_statement e) }

try_statement:
 | T_TRY b=block c=catch f=finally? { (Try_statement (b, Some c, f)) }
 | T_TRY b=block       f=finally { (Try_statement (b, None, Some f)) }

catch:
 | T_CATCH pair=pair(parenthesised(variable), block) { pair }

finally:
 | T_FINALLY b=block { b }

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
 | T_FUNCTION name=variable args=parenthesised(separated_list(T_COMMA, variable))
   block=curly_block(source_element*)
   { let elements,(_,loc) = block in
     (name, args, elements, p loc) }

function_expression:
 | T_FUNCTION name=variable? args=parenthesised(separated_list(T_COMMA, variable))
   block=curly_block(source_element*)
   { let elements,_ = block in
     EFun (name, args, elements, p $symbolstartpos) }

(*************************************************************************)
(* 1 expression                                                          *)
(*************************************************************************)

expression:
 | assignment_expression { $1 }
 | e1=expression T_COMMA e2=assignment_expression { ESeq (e1, e2) }

assignment_expression:
 | conditional_expression { $1 }
 | e1=left_hand_side_expression op=assignment_operator e2=assignment_expression
   { EBin (op, e1, e2) }

left_hand_side_expression:
 | new_expression  { $1 }
 | call_expression { $1 }

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
 | e=member_expression a=arguments
     { (ECall(e, a, p $symbolstartpos)) }
 | e=call_expression a=arguments
     { (ECall(e, a, p $symbolstartpos)) }
 | e=call_expression T_LBRACKET e2=expression T_RBRACKET
     { (EAccess (e, e2)) }
 | e=call_expression T_PERIOD i=identifier_or_kw
     { (EDot (e, i)) }

new_expression:
 | e=member_expression    { e }
 | T_NEW e=new_expression { (ENew (e,None)) }

member_expression:
 | e=primary_expression
     { e }
 | e1=member_expression T_LBRACKET e2=expression T_RBRACKET
     { (EAccess (e1,e2)) }
 | e1=member_expression T_PERIOD i=identifier_or_kw
     { (EDot(e1,i)) }
 | T_NEW e1=member_expression a=arguments
     { (ENew(e1, Some a)) }

primary_expression:
 | e=primary_expression_no_statement
 | e=object_literal
 | e=function_expression { e }

primary_expression_no_statement:
 | T_THIS         { (EVar (var (p $symbolstartpos) (Stdlib.Utf8_string.of_string_exn "this"))) }
 | i=variable_with_loc { (EVar i) }
 | n=null_literal    { n }
 | b=boolean_literal { b }
 | n=numeric_literal   { (ENum (Num.of_string_unsafe n)) }
 | s=T_STRING          { (EStr (fst s)) }
 | r=regex_literal                { r }
 | a=array_literal                { a }
 | T_LPAREN e=expression T_RPAREN { (e) }

(*----------------------------*)
(* 2 no in                    *)
(*----------------------------*)

expression_no_in:
 | assignment_expression_no_in { $1 }
 | e1=expression_no_in T_COMMA e2=assignment_expression_no_in { ESeq (e1, e2) }

assignment_expression_no_in:
 | conditional_expression_no_in { $1 }
 | e1=left_hand_side_expression op=assignment_operator e2=assignment_expression_no_in
     { EBin(op,e1,e2) }

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
 | e1=expression_no_statement T_COMMA e2=assignment_expression { ESeq(e1,e2) }

assignment_expression_no_statement:
 | conditional_expression_no_statement { $1 }
 | e1=left_hand_side_expression_no_statement op=assignment_operator e2=assignment_expression
   { EBin (op,e1,e2) }

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
 | new_expression_no_statement { $1 }
 | call_expression_no_statement { $1 }

new_expression_no_statement:
 | member_expression_no_statement { $1 }
 | T_NEW e=new_expression { (ENew (e,None)) }

call_expression_no_statement:
 | e=member_expression_no_statement e2=arguments
   { ( ECall(e, e2, p $symbolstartpos)) }
 | e=call_expression_no_statement a=arguments
   { ( ECall(e, a, p $symbolstartpos)) }
 | e=call_expression_no_statement T_LBRACKET e2=expression T_RBRACKET
   { ( EAccess(e, e2)) }
 | e=call_expression_no_statement T_PERIOD i=identifier_or_kw
   { ( EDot(e,i)) }

member_expression_no_statement:
 | e=primary_expression_no_statement
   { e }
 | e1=member_expression_no_statement T_LBRACKET e2=expression T_RBRACKET
   { ( EAccess(e1, e2)) }
 | e1=member_expression_no_statement T_PERIOD i=identifier_or_kw
   { ( EDot(e1,i)) }
 | T_NEW e=member_expression a=arguments
   { (ENew(e,Some a)) }

(*----------------------------*)
(* 2 scalar                   *)
(*----------------------------*)

null_literal:
 | T_NULL { (EVar (var (p $symbolstartpos) (Stdlib.Utf8_string.of_string_exn "null"))) }

boolean_literal:
 | T_TRUE  { (EBool true) }
 | T_FALSE { (EBool false) }

numeric_literal:
 | T_NUMBER { let _,f = $1 in (f) }

regex_literal:
 | r=T_REGEXP {
   let (Utf8 s, f) = r in
   (ERegexp (s, if String.equal f "" then None else Some f)) }

(*----------------------------*)
(* 2 array                    *)
(*----------------------------*)

array_literal:
 | T_LBRACKET e=elison T_RBRACKET
     { (EArr e) }
 | T_LBRACKET        T_RBRACKET
     { (EArr []) }
 | T_LBRACKET l=element_list T_RBRACKET
     { (EArr l) }
 | T_LBRACKET l=element_list_rev last=elison_rev T_RBRACKET
     { (EArr (List.rev_append l (List.rev last))) }

element_list:
 | element_list_rev { List.rev $1 }

element_list_rev:
 | empty=elison_rev e=assignment_expression { (Some e)::empty }
 | e=assignment_expression { [Some e] }
 | fst=element_list_rev empty=elison e=assignment_expression { (Some e) :: (List.rev_append empty fst) }

object_literal:
 | block=curly_block(empty)
   { let _pairs, _ = block in EObj [] }
 | block=curly_block(separated_or_terminated_list(T_COMMA, object_key_value))
   { let pairs, _ = block in EObj pairs }

object_key_value:
 | pair=separated_pair(property_name, T_COLON, assignment_expression) { pair }

(*----------------------------*)
(* 2 variable                 *)
(*----------------------------*)

(*----------------------------*)
(* 2 function call            *)
(*----------------------------*)

arg:
 | T_ELLIPSIS arg=assignment_expression { arg, `Spread }
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
  | T_IDENTIFIER {
     let name, _raw = $1 in
     name }
  | T_ASYNC { utf8_s "async" }
  | T_AWAIT { utf8_s "await" }
  | T_BREAK { utf8_s "break" }
  | T_CASE { utf8_s "case" }
  | T_CATCH { utf8_s "catch" }
  | T_CLASS { utf8_s "class" }
  | T_CONST { utf8_s "const" }
  | T_CONTINUE { utf8_s "continue" }
  | T_DEBUGGER { utf8_s "debugger" }
  | T_DECLARE { utf8_s "declare" }
  | T_DEFAULT { utf8_s "default" }
  | T_DELETE { utf8_s "delete" }
  | T_DO { utf8_s "do" }
  | T_ELSE { utf8_s "else" }
  | T_ENUM { utf8_s "enum" }
  | T_EXPORT { utf8_s "export" }
  | T_EXTENDS { utf8_s "extends" }
  | T_FALSE { utf8_s "false" }
  | T_FINALLY { utf8_s "finally" }
  | T_FOR { utf8_s "for" }
  | T_FUNCTION { utf8_s "function" }
  | T_IF { utf8_s "if" }
  | T_IMPLEMENTS { utf8_s "implements" }
  | T_IMPORT { utf8_s "import" }
  | T_IN { utf8_s "in" }
  | T_INSTANCEOF { utf8_s "instanceof" }
  | T_INTERFACE { utf8_s "interface" }
  | T_LET { utf8_s "let" }
  | T_NEW { utf8_s "new" }
  | T_NULL { utf8_s "null" }
  | T_OF { utf8_s "of" }
  | T_OPAQUE { utf8_s "opaque" }
  | T_PACKAGE { utf8_s "package" }
  | T_PRIVATE { utf8_s "private" }
  | T_PROTECTED { utf8_s "protected" }
  | T_PUBLIC { utf8_s "public" }
  | T_RETURN { utf8_s "return" }
  | T_STATIC { utf8_s "static" }
  | T_SUPER { utf8_s "super" }
  | T_SWITCH { utf8_s "switch" }
  | T_THIS { utf8_s "this" }
  | T_THROW { utf8_s "throw" }
  | T_TRUE { utf8_s "true" }
  | T_TRY { utf8_s "try" }
  | T_TYPE { utf8_s "type" }
  | T_TYPEOF { utf8_s "typeof" }
  | T_VAR { utf8_s "var" }
  | T_VOID { utf8_s "void" }
  | T_WHILE { utf8_s "while" }
  | T_WITH { utf8_s "with" }
  | T_YIELD { utf8_s "yield" }

variable:
 | i=variable_with_loc { i }

variable_with_loc:
  | i=T_IDENTIFIER {
          let name, _raw = i in
          var (p $symbolstartpos) name
        }
  | ident_semi_keyword { var (p $symbolstartpos) (utf8_s (Js_token.to_string $1)) }

(* add here keywords which are not considered reserved by ECMA *)
ident_semi_keyword:
 | T_OF { T_OF }
 | T_TYPE { T_TYPE }
 | T_DECLARE { T_DECLARE }
 | T_PUBLIC { T_PUBLIC } | T_PRIVATE { T_PRIVATE } | T_PROTECTED { T_PROTECTED }
 (* can have AS and ASYNC here but need to restrict arrow_function then *)
 | T_ASYNC { T_ASYNC }
 (* TODO: would like to add T_IMPORT here, but cause conflicts *)
 | T_PACKAGE { T_PACKAGE }
 | T_IMPLEMENTS { T_IMPLEMENTS }
 | T_OPAQUE { T_OPAQUE }

label:
  | T_IDENTIFIER {
          let name, _raw = $1 in
          Label.of_string name }

property_name:
 | i=identifier_or_kw { PNI i }
 | s=T_STRING         {
    let s, _len = s in PNS s }
 | n=numeric_literal  { PNN (Num.of_string_unsafe (n)) }

(*************************************************************************)
(* 1 xxx_opt, xxx_list                                                   *)
(*************************************************************************)

elison_rev:
 | T_COMMA { [] }
 | elison T_COMMA { None :: $1 }

elison: elison_rev {$1}
 (* | elison_rev { List.rev $1} *)

curly_block(X):
 | T_LCURLY x=X T_RCURLY { x, ($startpos($1),$startpos($3)) }

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
