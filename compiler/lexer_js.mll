{
(* Yoann Padioleau
 *
 * Copyright (C) 2010 Facebook
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

open Parser_js

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

exception Lexical of string

(* pad: hack around ocamllex to emulate the yyless of flex. It seems
 * to work.
 *)
let yyless n lexbuf =
  lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_curr_pos - n;
  let currp = lexbuf.Lexing.lex_curr_p in
  lexbuf.Lexing.lex_curr_p <- { currp with
    Lexing.pos_cnum = currp.Lexing.pos_cnum - n;
  }

let tok     lexbuf  =
  Lexing.lexeme lexbuf
let tokinfo lexbuf  = Parse_info.t_of_lexbuf lexbuf
  (* Parse_info.tokinfo_str_pos (Lexing.lexeme lexbuf) (Lexing.lexeme_start lexbuf) *)

(* ---------------------------------------------------------------------- *)
let keyword_table =
  let h = Hashtbl.create 17 in
  List.iter (fun (s,f) -> Hashtbl.add h s f ) [

   (* todo? had some special handling in lexer of marcel *)
  "catch",      (fun ii -> T_CATCH ii);
  "finally",    (fun ii -> T_FINALLY ii);
  "in",         (fun ii -> T_IN ii);
  "instanceof", (fun ii -> T_INSTANCEOF ii);

  (* todo? had some special handling in lexer of marcel *)
  "else",       (fun ii -> T_ELSE ii);
  "while",      (fun ii -> T_WHILE ii);

  "break",      (fun ii -> T_BREAK ii);
  "case",       (fun ii -> T_CASE ii);
  "continue",   (fun ii -> T_CONTINUE ii);
  "default",    (fun ii -> T_DEFAULT ii);
  "delete",     (fun ii -> T_DELETE ii);
  "do",         (fun ii -> T_DO ii);
  "else",       (fun ii -> T_ELSE ii);
  "for",        (fun ii -> T_FOR ii);
  "function",   (fun ii -> T_FUNCTION ii);
  "if",         (fun ii -> T_IF ii);
  "new",        (fun ii -> T_NEW ii);
  "return",     (fun ii -> T_RETURN ii);
  "switch",     (fun ii -> T_SWITCH ii);
  "this",       (fun ii -> T_THIS ii);
  "throw",      (fun ii -> T_THROW ii);
  "try",        (fun ii -> T_TRY ii);
  "typeof",     (fun ii -> T_TYPEOF ii);
  "var",        (fun ii -> T_VAR ii);
  "void",       (fun ii -> T_VOID ii);
  "while",      (fun ii -> T_WHILE ii);
  "with",       (fun ii -> T_WITH ii);
  "const",      (fun ii -> T_CONST ii);
  "null",       (fun ii -> T_NULL ii);
  "false",      (fun ii -> T_FALSE ii);
  "true",       (fun ii -> T_TRUE ii);
];
  h

(* ---------------------------------------------------------------------- *)

type state_mode =
  | INITIAL

let default_state = INITIAL

(* The logic to modify _last_non_whitespace_like_token is in the
 * caller of the lexer, that is in Parse_js.tokens.
 * Used for ambiguity between / as a divisor and start of regexp.
 *)
let _last_non_whitespace_like_token =
  ref (None: Parser_js.token option)

exception Token of Parser_js.token

let reset () =
   _last_non_whitespace_like_token := None;
  ()

}

(*****************************************************************************)
let _WHITESPACE = [' ' '\n' '\r' '\t']+
let TABS_AND_SPACES = [' ''\t']*
let NEWLINE = ("\r"|"\n"|"\r\n")

(*****************************************************************************)

rule initial = parse

  (* ----------------------------------------------------------------------- *)
  (* spacing/comments *)
  (* ----------------------------------------------------------------------- *)
  | "/*" {
      let info = tokinfo lexbuf in
      let com = st_comment lexbuf in
      TComment(info)
    }

  | "//" {
      let info = tokinfo lexbuf in
      let com = st_one_line_comment lexbuf in
      TComment(info)
    }

  | [' ' '\t' ]+            { TCommentSpace(tokinfo lexbuf) }
  | NEWLINE {
      lexbuf.Lexing.lex_curr_p <- { lexbuf.Lexing.lex_curr_p with
                                      Lexing.pos_lnum = lexbuf.Lexing.lex_curr_p.Lexing.pos_lnum + 1 };
      TCommentNewline(tokinfo lexbuf) }

  (* ----------------------------------------------------------------------- *)
  (* symbols *)
  (* ----------------------------------------------------------------------- *)

   (* todo? marcel does some stack push/pop on that *)
  | "{" { T_LCURLY (tokinfo lexbuf); }
  | "}" { T_RCURLY (tokinfo lexbuf); }

   (* todo? marcel does some stack push/pop on that *)
  | "(" { T_LPAREN (tokinfo lexbuf); }
  | ")" { T_RPAREN (tokinfo lexbuf); }

  | "[" { T_LBRACKET (tokinfo lexbuf); }
  | "]" { T_RBRACKET (tokinfo lexbuf); }
  | "." { T_PERIOD (tokinfo lexbuf); }
  | ";" { T_SEMICOLON (tokinfo lexbuf); }
  | "," { T_COMMA (tokinfo lexbuf); }
  | ":" { T_COLON (tokinfo lexbuf); }
  | "?" { T_PLING (tokinfo lexbuf); }
  | "&&" { T_AND (tokinfo lexbuf); }
  | "||" { T_OR (tokinfo lexbuf); }
  | "===" { T_STRICT_EQUAL (tokinfo lexbuf); }
  | "!==" { T_STRICT_NOT_EQUAL (tokinfo lexbuf); }
  | "<=" { T_LESS_THAN_EQUAL (tokinfo lexbuf); }
  | ">=" { T_GREATER_THAN_EQUAL (tokinfo lexbuf); }
  | "==" { T_EQUAL (tokinfo lexbuf); }
  | "!=" { T_NOT_EQUAL (tokinfo lexbuf); }
  | "++" { T_INCR (tokinfo lexbuf); }
  | "--" { T_DECR (tokinfo lexbuf); }
  | "<<=" { T_LSHIFT_ASSIGN (tokinfo lexbuf); }
  | "<<" { T_LSHIFT (tokinfo lexbuf); }
  | ">>=" { T_RSHIFT_ASSIGN (tokinfo lexbuf); }
  | ">>>=" { T_RSHIFT3_ASSIGN (tokinfo lexbuf); }
  | ">>>" { T_RSHIFT3 (tokinfo lexbuf); }
  | ">>" { T_RSHIFT (tokinfo lexbuf); }
  | "+=" { T_PLUS_ASSIGN (tokinfo lexbuf); }
  | "-=" { T_MINUS_ASSIGN (tokinfo lexbuf); }

  | "*=" { T_MULT_ASSIGN (tokinfo lexbuf); }
  | "%=" { T_MOD_ASSIGN (tokinfo lexbuf); }
  | "&=" { T_BIT_AND_ASSIGN (tokinfo lexbuf); }
  | "|=" { T_BIT_OR_ASSIGN (tokinfo lexbuf); }
  | "^=" { T_BIT_XOR_ASSIGN (tokinfo lexbuf); }
  | "<" { T_LESS_THAN (tokinfo lexbuf); }
  | ">" { T_GREATER_THAN (tokinfo lexbuf); }
  | "+" { T_PLUS (tokinfo lexbuf); }
  | "-" { T_MINUS (tokinfo lexbuf); }
  | "*" { T_MULT (tokinfo lexbuf); }
  (* for '/' see below the regexp handling *)
  | "%" { T_MOD (tokinfo lexbuf); }
  | "|" { T_BIT_OR (tokinfo lexbuf); }
  | "&" { T_BIT_AND (tokinfo lexbuf); }
  | "^" { T_BIT_XOR (tokinfo lexbuf); }
  | "!" { T_NOT (tokinfo lexbuf); }
  | "~" { T_BIT_NOT (tokinfo lexbuf); }
  | "=" { T_ASSIGN (tokinfo lexbuf); }

  (* ----------------------------------------------------------------------- *)
  (* Keywords and ident *)
  (* ----------------------------------------------------------------------- *)
  | ['a'-'z''A'-'Z''$''_']['a'-'z''A'-'Z''$''_''0'-'9']* {
      let s = tok lexbuf in
      let info = tokinfo lexbuf in
      try
        let f = Hashtbl.find keyword_table s in
        f info (* need case insensitive ? *)
      with
        | Not_found -> T_IDENTIFIER (s, info)
    }

  (* ----------------------------------------------------------------------- *)
  (* Constant *)
  (* ----------------------------------------------------------------------- *)

  | "0x"['a'-'f''A'-'F''0'-'9']+ {
      let s = tok lexbuf in
      let info = tokinfo lexbuf in
      T_NUMBER (s, info)
    }
  | '0'['0'-'7']+ {
      let s = tok lexbuf in
      let info = tokinfo lexbuf in
      T_NUMBER (s, info)
    }

  | ['0'-'9']*'.'?['0'-'9']+['e''E']['-''+']?['0'-'9']+ (* {1,3} *) {
      let s = tok lexbuf in
      let info = tokinfo lexbuf in
      T_NUMBER (s, info)
    }

  | ['0'-'9']+'.'? |
    ['0'-'9']*'.'['0'-'9']+ {
      let s = tok lexbuf in
      let info = tokinfo lexbuf in
      T_NUMBER (s, info)
    }

  (* ----------------------------------------------------------------------- *)
  (* Strings *)
  (* ----------------------------------------------------------------------- *)
  | "'" {
      let info = tokinfo lexbuf in
      let s = string_quote lexbuf in
      (* s does not contain the enclosing "'" but the info does *)
      T_STRING (s, info)
    }

  | '"' {
      let info = tokinfo lexbuf in
      let s = string_double_quote lexbuf in
      T_STRING (s, info)
    }

  (* ----------------------------------------------------------------------- *)
  (* Regexp *)
  (* ----------------------------------------------------------------------- *)
  (* take care of ambiguity with start of comment //, and with
   * '/' as a divisor operator
   *
   * it can not be '/' [^ '/']* '/' because then
   * comments will not be recognized as lex tries
   * to find the longest match.
   *
   * It can not be
   * '/' [^'*''/'] ([^'/''\n'])* '/' ['A'-'Z''a'-'z']*
   * because a / (b/c)  will be recognized as a regexp.
   *
   *)

  (* todo? marcel was changing of state context condition there *)
  | "/=" { T_DIV_ASSIGN (tokinfo lexbuf); }

  | "/" {
      let info = tokinfo lexbuf in

      match !_last_non_whitespace_like_token with
      | Some (
            T_IDENTIFIER _
          | T_NUMBER _
          | T_STRING _
          | T_REGEX _
          | T_INCR _ | T_DECR _
          | T_RBRACKET _
          | T_RPAREN _
          | T_FALSE _ | T_TRUE _
          | T_NULL _
          | T_THIS _
        ) ->
          T_DIV (info);
      | _ ->
          (* raise (Token t); *)
          let s = regexp lexbuf in
          T_REGEX ("/" ^ s, info)
    }

  (* ----------------------------------------------------------------------- *)
  (* Misc *)
  (* ----------------------------------------------------------------------- *)

  (* ----------------------------------------------------------------------- *)
  (* eof *)
  (* ----------------------------------------------------------------------- *)

  | eof { EOF (tokinfo lexbuf) }

  | _ {
      Printf.eprintf "LEXER:unrecognised symbol, in token rule: %s" (tok lexbuf);
      TUnknown (tokinfo lexbuf)
    }
(*****************************************************************************)
and string_quote = parse
  | "'"            { "" }
  | (_ as x)       { String.make 1  x^string_quote lexbuf}
  | ("\\" (_ as v)) as x {
      (* check char ? *)
      (match v with
      | _ -> ()
      );
      x ^ string_quote lexbuf
    }
  | eof { Printf.eprintf  "LEXER: WIERD end of file in quoted string"; ""}

and string_double_quote  = parse
  | '"'            { "" }
  | (_ as x)       { String.make 1  x^string_double_quote lexbuf}
  | ("\\" (_ as v)) as x {
      (* check char ? *)
      (match v with
      | _ -> ()
      );
      x ^ string_double_quote lexbuf
    }
  | eof { Printf.eprintf "LEXER: WIERD end of file in double quoted string"; ""}

(*****************************************************************************)
and regexp = parse
  | '/'            { "/" ^ regexp_maybe_ident lexbuf }
  | (_ as x)       { String.make 1 x^regexp lexbuf}
  | ("\\" (_ as v)) as x {
      (* check char ? *)
      (match v with
      | _ -> ()
      );
      x ^ regexp lexbuf
    }
  | eof { Printf.eprintf "LEXER: WIERD end of file in regexp"; ""}

and regexp_maybe_ident = parse
  | ['A'-'Z''a'-'z']* { tok lexbuf }

(*****************************************************************************)

and st_comment = parse
  | "*/" { tok lexbuf }

  (* noteopti: *)
  | [^'*']+ { let s = tok lexbuf in s ^ st_comment lexbuf }
  | "*"     { let s = tok lexbuf in s ^ st_comment lexbuf }

  | eof { Printf.eprintf "LEXER: end of file in comment"; "*/"}
  | _  {
      let s = tok lexbuf in
      Printf.eprintf "LEXER: unrecognised symbol in comment: %s" s;
      s ^ st_comment lexbuf
    }

and st_one_line_comment = parse
  | [^'\n' '\r']* {
      let s = tok lexbuf in
      s ^ st_one_line_comment lexbuf
    }

  | NEWLINE {
      lexbuf.Lexing.lex_curr_p <- { lexbuf.Lexing.lex_curr_p with
                                      Lexing.pos_lnum = lexbuf.Lexing.lex_curr_p.Lexing.pos_lnum + 1 };

      tok lexbuf }

  | eof { Printf.eprintf "LEXER: end of file in comment"; "\n" }
  | _ {
      Printf.eprintf "LEXER:unrecognised symbol, in st_one_line_comment rule: %s " (tok lexbuf);
      tok lexbuf
    }
