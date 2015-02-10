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
open Js_token

let var name = J.S {J.name;J.var=None}

(* This is need to fake menhir while using `--infer`. *)
let _tok = EOF Parse_info.zero

%}

(*************************************************************************)
(* 1 Tokens                                                              *)
(*************************************************************************)

(*-----------------------------------------*)
(* 2 the normal tokens                     *)
(*-----------------------------------------*)

(* Tokens with a value *)
%token<string * float * Parse_info.t> T_NUMBER
%token<string * Parse_info.t> T_IDENTIFIER
%token<string * Parse_info.t> T_STRING
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

%start <Javascript.program> program
%start <Javascript.expression> standalone_expression

%%

(*************************************************************************)
(* 1 Toplevel                                                            *)
(*************************************************************************)

program:
 | l=source_elements EOF { l }

standalone_expression:
 | e=expression EOF {e}

source_element:
 | s=statement
     { let (s, loc) = s in (J.Statement s, loc) }
 | f=function_declaration
     { let (f, loc) = f in (J.Function_declaration f, loc) }

source_elements:
 | l=source_element* { l }

(*************************************************************************)
(* 1 statement                                                           *)
(*************************************************************************)

statement_no_semi:
 | b=block_with_pi { (J.Block (fst b), J.Pi (snd b)) }
 (* this is not allowed but some browsers accept it *)
 (* | function_declaration { *)
 (*  let var,params,body,_ = $1 in *)
 (*  J.Variable_statement [var,Some (J.EFun((None,params,body),None))]} *)
 | s=if_statement         { s }
 | s=iteration_statement  { s }
 | s=with_statement       { s }
 | s=switch_statement     { s }
 | s=try_statement        { s }
 | s=labeled_statement    { s }
 | s=empty_statement      { s }

statement_need_semi:
 | variable_statement   { $1 }
 | expression_statement { $1 }
 | do_while_statement   { $1 }
 | continue_statement   { $1 }
 | break_statement      { $1 }
 | return_statement     { $1 }
 | throw_statement      { $1 }
 | debugger_statement   { $1 }

statement:
 | s=statement_no_semi {s}
 | s=statement_need_semi semicolon {s}
 | s=statement_need_semi {
    (* 7.9.1 - 1 *)
    (* When, as the program is parsed from left to right, a token (called the offending token)
       is encountered that is not allowed by any production of the grammar, then a semicolon
       is automatically inserted before the offending token if one or more of the following
       conditions is true:
       - The offending token is }.
       - The offending token is separated from the previous
         token by at least one LineTerminator. *)

    (* 7.9.1 - 2 *)
    (* When, as the program is parsed from left to right, the end of the input stream of tokens *)
    (* is encountered and the parser is unable to parse the input token stream as a single *)
    (* complete ECMAScript Program, then a semicolon is automatically inserted at the end *)
    (* of the input stream. *)

    (* @@@@@@@@@ HACK @@@@@@@@@@ *)
    (* menhir internal's         *)
    (* look the current token:   *)
    (* - if it is on another line (linebreak inbetween), accept the statement *)
    (* - fail otherwise *)
    (* @@@@@@@@@ HACK @@@@@@@@@@ *)

    match _tok with
      | EOF _ -> s
      | T_RCURLY _ -> s
      | t ->
        let info = Js_token.info_of_tok t in
        match info.Parse_info.fol with
          | Some true -> s
          | _ -> $syntaxerror
  }

semicolon:
 | T_SEMICOLON {}
 | T_VIRTUAL_SEMICOLON {}

labeled_statement:
| l=label T_COLON s=statement { (J.Labelled_statement (l, s), J.N) }

block_with_pi:
 | l=curly_block(statement*) { (fst l, fst (snd l)) }

block:
 | block_with_pi { fst $1 }

variable_statement:
 | pi=T_VAR separated_nonempty_list(T_COMMA,variable_declaration)
     { (J.Variable_statement $2, J.Pi pi) }

variable_declaration:
 | variable option(initializeur) { $1, $2 }

initializeur:
 | T_ASSIGN assignment_expression { $2, J.Pi $1 }

empty_statement:
 | pi=T_SEMICOLON { (J.Empty_statement, J.Pi pi) }

debugger_statement:
 | pi=T_DEBUGGER { (J.Debugger_statement, J.Pi pi) }

expression_statement:
 | expression_no_statement { (J.Expression_statement $1, J.N) }

if_statement:
 | pi=T_IF T_LPAREN i=expression T_RPAREN t=statement T_ELSE e=statement
     { (J.If_statement (i, t, Some e), J.Pi pi) }
 | pi=T_IF T_LPAREN i=expression T_RPAREN t=statement %prec p_IF
     { (J.If_statement (i, t, None), J.Pi pi) }

do_while_statement:
  | pi=T_DO statement T_WHILE T_LPAREN expression T_RPAREN
    { (J.Do_while_statement ($2, $5), J.Pi pi) }

iteration_statement:
 | pi=T_WHILE T_LPAREN expression T_RPAREN statement
     { (J.While_statement ($3, $5), J.Pi pi) }
 | pi=T_FOR T_LPAREN
     option(expression_no_in) T_SEMICOLON
     option(expression) T_SEMICOLON
     option(expression)
     T_RPAREN statement
     { (J.For_statement (J.Left $3, $5, $7, $9), J.Pi pi) }
 | pi=T_FOR T_LPAREN
     T_VAR separated_nonempty_list(T_COMMA,variable_declaration_no_in) T_SEMICOLON
     option(expression) T_SEMICOLON
     option(expression)
     T_RPAREN statement
     { (J.For_statement (J.Right($4), $6, $8, $10), J.Pi pi) }
 | pi=T_FOR T_LPAREN left_hand_side_expression T_IN expression T_RPAREN statement
     { (J.ForIn_statement (J.Left $3,$5,$7),J.Pi pi) }
 | pi=T_FOR T_LPAREN T_VAR variable_declaration_no_in T_IN expression T_RPAREN
     statement
     { (J.ForIn_statement ( J.Right $4, $6, $8), J.Pi pi) }

variable_declaration_no_in:
 | variable option(initializer_no_in) { $1, $2 }

initializer_no_in:
 | T_ASSIGN assignment_expression_no_in { $2, J.Pi $1 }

continue_statement:
 | pi=T_CONTINUE option(label) { (J.Continue_statement $2,J.Pi pi) }

break_statement:
 | pi=T_BREAK option(label) { (J.Break_statement $2, J.Pi pi) }

return_statement:
 | pi=T_RETURN option(expression) { (J.Return_statement $2, J.Pi pi) }

with_statement:
 | T_WITH T_LPAREN expression T_RPAREN statement { assert false }

switch_statement:
 | pi=T_SWITCH T_LPAREN e=expression T_RPAREN
    b=curly_block(
    pair(case_clause*, option(pair(default_clause, case_clause*))))
    {
      let (l, d, l') =
        match fst b with
          (l, None) -> (l, None, [])
        | (l, Some (d, l')) -> (l, Some d, l')
      in
      (J.Switch_statement (e, l, d, l'), J.Pi pi) }

throw_statement:
 | pi=T_THROW expression { (J.Throw_statement $2, J.Pi pi) }

try_statement:
 | pi=T_TRY block catch option(finally) { (J.Try_statement ($2, Some $3, $4), J.Pi pi) }
 | pi=T_TRY block       finally { (J.Try_statement ($2, None, Some $3), J.Pi pi) }

catch:
 | T_CATCH T_LPAREN variable T_RPAREN block { $3, $5 }

finally:
 | T_FINALLY block { $2 }

(*----------------------------*)
(* 2 auxillary statements     *)
(*----------------------------*)

case_clause:
 | T_CASE e=expression T_COLON l=statement* { e,l }

default_clause:
 | T_DEFAULT T_COLON l=statement* { l }

(*************************************************************************)
(* 1 function declaration                                                *)
(*************************************************************************)

function_declaration:
 | pi=T_FUNCTION v=variable T_LPAREN args=separated_list(T_COMMA,variable) T_RPAREN
     b=curly_block(function_body)
     { ((v, args, fst b, J.Pi (snd (snd b))), J.Pi pi) }

function_expression:
 | pi=T_FUNCTION v=option(variable)
   T_LPAREN args=separated_list(T_COMMA,variable) T_RPAREN
   b=curly_block(function_body)
   { (pi, J.EFun (v, args, fst b, J.Pi pi)) }

function_body:
 | l=source_elements  { l }

(*************************************************************************)
(* 1 expression                                                          *)
(*************************************************************************)

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
 | new_expression  { snd $1 }
 | call_expression { snd $1 }

conditional_expression:
 | post_in_expression { $1 }
 | post_in_expression
     T_PLING assignment_expression
     T_COLON assignment_expression
     { J.ECond ($1, $3, $5) }

post_in_expression:
 | pre_in_expression { $1 }
 | left=post_in_expression
   op=comparison_or_logical_or_bit_operator
   right=post_in_expression
   { J.EBin (op, left, right) }

pre_in_expression:
 | left_hand_side_expression
   { $1 }
 | e=pre_in_expression op=postfix_operator
 | op=prefix_operator e=pre_in_expression
   { J.EUn (op, e) }
 | left=pre_in_expression
   op=arithmetic_or_shift_operator
   right=pre_in_expression
   { J.EBin (op, left, right) }

call_expression:
 | member_expression arguments
     { let (start, e) = $1 in (start, J.ECall(e, $2, J.Pi start)) }
 | call_expression arguments
     { let (start, e) = $1 in (start, J.ECall(e, $2, J.Pi start)) }
 | call_expression T_LBRACKET expression T_RBRACKET
     { let (start, e) = $1 in (start, J.EAccess (e, $3)) }
 | call_expression T_PERIOD method_name
     { let (start, e) = $1 in (start, J.EDot (e, $3)) }

new_expression:
 | member_expression    { $1 }
 | pi=T_NEW new_expression { (pi, J.ENew (snd $2,None)) }

member_expression:
 | e=primary_expression
     { e }
 | member_expression T_LBRACKET e2=expression T_RBRACKET
     { let (start, e1) = $1 in (start, J.EAccess (e1,e2)) }
 | member_expression T_PERIOD i=field_name
     { let (start, e1) = $1 in (start, J.EDot(e1,i)) }
 | pi=T_NEW e1=member_expression a=arguments
     { (pi, J.ENew(snd e1, Some a)) }

primary_expression:
 | p=primary_expression_no_statement { p }
 | o=object_literal                  { o }
 | f=function_expression             { f }

primary_expression_no_statement:
 | pi=T_THIS         { (pi, J.EVar (var "this")) }
 | variable_with_loc { let (i, pi) = $1 in (pi, J.EVar (var i)) }

 | n=null_literal    { n }
 | b=boolean_literal { b }
 | numeric_literal   { let (start, n) = $1 in (start, J.ENum n) }
 | string_literal    { let (s, start) = $1 in (start, J.EStr (s, `Utf8)) }
   (* marcel: this isn't an expansion of literal in ECMA-262... mistake? *)
 | r=regex_literal                { r }
 | a=array_literal                { a }
 | pi=T_LPAREN e=expression T_RPAREN { (pi, e) }

(*----------------------------*)
(* 2 no in                    *)
(*----------------------------*)

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
 | left=post_in_expression_no_in
   op=comparison_or_logical_or_bit_operator_except_in
   right=post_in_expression
   { J.EBin (op, left, right) }

(*----------------------------*)
(* 2 (no statement)           *)
(*----------------------------*)

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
 | left=post_in_expression_no_statement
   op=comparison_or_logical_or_bit_operator
   right=post_in_expression
   { J.EBin (op, left, right) }

pre_in_expression_no_statement:
 | left_hand_side_expression_no_statement
   { $1 }
 | e=pre_in_expression_no_statement op=postfix_operator
 | op=prefix_operator e=pre_in_expression
   { J.EUn (op, e) }
 | left=pre_in_expression_no_statement
   op=arithmetic_or_shift_operator
   right=pre_in_expression
   { J.EBin (op, left, right) }

left_hand_side_expression_no_statement:
 | new_expression_no_statement { snd $1 }
 | call_expression_no_statement { snd $1 }

new_expression_no_statement:
 | member_expression_no_statement { $1 }
 | pi=T_NEW new_expression { (pi, J.ENew (snd $2,None)) }

call_expression_no_statement:
 | member_expression_no_statement arguments
   { let (start, e) = $1 in (start, J.ECall(e, $2, J.Pi start)) }
 | call_expression_no_statement arguments
   { let (start, e) = $1 in (start, J.ECall(e, $2, J.Pi start)) }
 | call_expression_no_statement T_LBRACKET expression T_RBRACKET
   { let (start, e) = $1 in (start, J.EAccess(e, $3)) }
 | call_expression_no_statement T_PERIOD method_name
   { let (start, e) = $1 in (start, J.EDot(e,$3)) }

member_expression_no_statement:
 | e=primary_expression_no_statement
   { e }
 | member_expression_no_statement T_LBRACKET e2=expression T_RBRACKET
   { let (start, e1) = $1 in (start, J.EAccess(e1, e2)) }
 | member_expression_no_statement T_PERIOD i=field_name
   { let (start, e1) = $1 in (start, J.EDot(e1,i)) }
 | pi=T_NEW e=member_expression a=arguments
   { (pi, J.ENew(snd e,Some a)) }

(*----------------------------*)
(* 2 scalar                   *)
(*----------------------------*)

null_literal:
 | pi=T_NULL { (pi, J.EVar (var "null")) }

boolean_literal:
 | pi=T_TRUE  { (pi, J.EBool true) }
 | pi=T_FALSE { (pi, J.EBool false) }

numeric_literal:
 | T_NUMBER { let (_, f, pi) = $1 in (pi, f) }

regex_literal:
 | T_REGEX {
   let (s, pi) = $1 in
   let len = String.length s in
   let regexp,option =
     if s.[len - 1] = '/'
     then String.sub s 1 (len - 2),None
     else
       let i = String.rindex s '/' in
       String.sub s 1 (i - 1),Some (String.sub s (i+1) (len - i - 1))
   in
   (pi, J.ERegexp (regexp,option)) }
   (* J.ENew(J.EVar (var "RegExp"), Some (List.map (fun s -> J.EStr (s,`Bytes)) args)) } *)

string_literal:
 | str=T_STRING { str }

(*----------------------------*)
(* 2 array                    *)
(*----------------------------*)

array_literal:
 | pi=T_LBRACKET elison T_RBRACKET
     { (pi, J.EArr $2) }
 | pi=T_LBRACKET        T_RBRACKET
     { (pi, J.EArr []) }
 | pi=T_LBRACKET element_list T_RBRACKET
     { (pi, J.EArr $2) }
 | pi=T_LBRACKET element_list_rev elison_rev T_RBRACKET
     { (pi, J.EArr (List.rev_append $2 (List.rev $3))) }

element_list:
 | element_list_rev { List.rev $1 }

element_list_rev:
 | elison_rev assignment_expression { (Some $2)::$1 }
 |            assignment_expression { [Some $1] }
 | element_list_rev elison assignment_expression { (Some $3) :: (List.rev_append $2 $1) }

separated_or_terminated_list(sep, X):
 | x=X { [x] }
 | x=X; sep { [x] }
 | x=X; sep; xs=separated_or_terminated_list(sep, X) { x :: xs }

object_literal:
 | res=curly_block(empty) { (fst (snd res), J.EObj []) }
 | res=curly_block(
     separated_or_terminated_list(
       T_COMMA,
       separated_pair(property_name,T_COLON,assignment_expression)
     ))  { (fst (snd res), J.EObj (fst res)) }

empty:
 | {}

(*----------------------------*)
(* 2 variable                 *)
(*----------------------------*)

(*----------------------------*)
(* 2 function call            *)
(*----------------------------*)

arguments:
 | T_LPAREN l=separated_list(T_COMMA,assignment_expression) T_RPAREN { l }

(*----------------------------*)
(* 2 auxillary bis            *)
(*----------------------------*)

(*************************************************************************)
(* 1 Entities, names                                                     *)
(*************************************************************************)

identifier:
 | T_IDENTIFIER { fst $1  }

(* should some keywork be allowed for field_name and method_name ??*)
field_name:
 | T_IDENTIFIER { fst $1 }
method_name:
 | T_IDENTIFIER { fst $1 }

variable:
 | i=identifier { var i }

variable_with_loc:
 | T_IDENTIFIER { $1 }

label:
 | identifier { J.Label.of_string $1 }

property_name:
 | i=T_IDENTIFIER    { J.PNI (fst i) }
 | s=string_literal  { J.PNS (fst s) }
 | n=numeric_literal { J.PNN (snd n) }

(*************************************************************************)
(* 1 xxx_opt, xxx_list                                                   *)
(*************************************************************************)

elison_rev:
 | T_COMMA { [] }
 | elison T_COMMA { None :: $1 }

elison: elison_rev {$1}
 (* | elison_rev { List.rev $1} *)

curly_block(X):
 | pi1=T_LCURLY x=X pi2=T_RCURLY { (x, (pi1, pi2)) }

(*----------------------------*)
(* Infix binary operators     *)
(*----------------------------*)

%inline comparison_or_logical_or_bit_operator_except_in:
 | T_LESS_THAN          { J.Lt         }
 | T_GREATER_THAN       { J.Gt         }
 | T_LESS_THAN_EQUAL    { J.Le         }
 | T_GREATER_THAN_EQUAL { J.Ge         }
 | T_INSTANCEOF         { J.InstanceOf }
 | T_EQUAL              { J.EqEq       }
 | T_NOT_EQUAL          { J.NotEq      }
 | T_STRICT_EQUAL       { J.EqEqEq     }
 | T_STRICT_NOT_EQUAL   { J.NotEqEq    }
 | T_BIT_AND            { J.Band       }
 | T_BIT_XOR            { J.Bxor       }
 | T_BIT_OR             { J.Bor        }
 | T_AND                { J.And        }
 | T_OR                 { J.Or         }

%inline comparison_or_logical_or_bit_operator:
 | op=comparison_or_logical_or_bit_operator_except_in { op }
 | T_IN { J.In }

%inline arithmetic_or_shift_operator:
 | T_MULT    { J.Mul   }
 | T_DIV     { J.Div   }
 | T_MOD     { J.Mod   }
 | T_PLUS    { J.Plus  }
 | T_MINUS   { J.Minus }
 | T_LSHIFT  { J.Lsl   }
 | T_RSHIFT  { J.Asr   }
 | T_RSHIFT3 { J.Lsr   }

%inline prefix_operator:
 | T_DELETE  { J.Delete }
 | T_VOID    { J.Void   }
 | T_TYPEOF  { J.Typeof }
 | T_INCR    { J.IncrB  }
 | T_INCR_NB { J.IncrB  }
 | T_DECR    { J.DecrB  }
 | T_DECR_NB { J.DecrB  }
 | T_PLUS    { J.Pl     }
 | T_MINUS   { J.Neg    }
 | T_BIT_NOT { J.Bnot   }
 | T_NOT     { J.Not    }

%inline postfix_operator:
 | T_INCR_NB { J.IncrA }
 | T_DECR_NB { J.DecrA }
