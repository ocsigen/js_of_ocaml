{

(* Js_of_ocaml compiler
 * Copyright (C) 2013 Hugo Heuzard
 *)

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

open Js_token

let tok lexbuf = Lexing.lexeme lexbuf

let keyword_table =
  let h = Hashtbl.create 17 in
  List.iter (fun (s,f) -> Hashtbl.add h s f ) [

    "catch",      (fun ii -> T_CATCH ii);
    "finally",    (fun ii -> T_FINALLY ii);
    "in",         (fun ii -> T_IN ii);
    "instanceof", (fun ii -> T_INSTANCEOF ii);

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
    "null",       (fun ii -> T_NULL ii);
    "false",      (fun ii -> T_FALSE ii);
    "true",       (fun ii -> T_TRUE ii);
    "debugger",   (fun ii -> T_DEBUGGER ii);
  ];
  h

}

(*****************************************************************************)

let NEWLINE = ("\r"|"\n"|"\r\n")
let hexa = ['0'-'9''a'-'f''A'-'F']
let inputCharacter = [^ '\r' '\n' ]
(*****************************************************************************)

rule initial tokinfo prev = parse

  (* ----------------------------------------------------------------------- *)
  (* spacing/comments *)
  (* ----------------------------------------------------------------------- *)
  | "/*" {
      let info = tokinfo lexbuf in
      let buf = Buffer.create 127 in
      let nl = ref false in
      st_comment buf nl lexbuf;
      let content = Buffer.contents buf in
      if !nl
      then TCommentML(info,content)
      else TComment(info,content)
    }
  (* don't keep the trailing \n; it will be in another token *)
  | "//" (inputCharacter* as cmt) { TComment(tokinfo lexbuf,cmt) }

  | ([' ' '\t' ]+ as cmt)         { TCommentSpace(tokinfo lexbuf,cmt) }
  | NEWLINE {
      lexbuf.Lexing.lex_curr_p <- { lexbuf.Lexing.lex_curr_p with
                                      Lexing.pos_lnum = lexbuf.Lexing.lex_curr_p.Lexing.pos_lnum + 1 };
      TCommentNewline(tokinfo lexbuf,"") }

  (* ----------------------------------------------------------------------- *)
  (* symbols *)
  (* ----------------------------------------------------------------------- *)

  | "{" { T_LCURLY (tokinfo lexbuf); }
  | "}" { T_RCURLY (tokinfo lexbuf); }

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
  | "++" {
      let cpi = tokinfo lexbuf in
      match prev with
        | Some p when (Js_token.info_of_tok p).Parse_info.line = cpi.Parse_info.line ->
          T_INCR_NB(cpi)
        | _ -> T_INCR(cpi) }
  | "--" {
      let cpi = tokinfo lexbuf in
      match prev with
        | Some p when (Js_token.info_of_tok p).Parse_info.line = cpi.Parse_info.line ->
          T_DECR_NB(cpi)
        | _ -> T_DECR(cpi) }
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

  | "0" ['X''x'] hexa+ {
      let s = tok lexbuf in
      let info = tokinfo lexbuf in
      T_NUMBER (s, Int64.(to_float (of_string s)), info)
    }
  | '0'['0'-'7']+ {
      let s = tok lexbuf in
      let s' = String.sub s 1 (String.length s - 1 ) in
      let info = tokinfo lexbuf in
      T_NUMBER (s, Int64.(to_float (of_string ("0o"^s'))), info)
    }

  | ['0'-'9']*'.'?['0'-'9']+['e''E']['-''+']?['0'-'9']+ (* {1,3} *) {
      let s = tok lexbuf in
      let info = tokinfo lexbuf in
      T_NUMBER (s, float_of_string s,info)
    }

  | ['0'-'9']+'.'? |
    ['0'-'9']*'.'['0'-'9']+ {
      let s = tok lexbuf in
      let info = tokinfo lexbuf in
      T_NUMBER (s, float_of_string s, info)
    }

  (* ----------------------------------------------------------------------- *)
  (* Strings *)
  (* ----------------------------------------------------------------------- *)
  | ("'"|'"') as quote {
      let info = tokinfo lexbuf in
      let buf = Buffer.create 127 in
      string_quote quote buf lexbuf;
      let s = Buffer.contents buf in
      (* s does not contain the enclosing "'" but the info does *)
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

  | "/" | "/=" {
    let s = tok lexbuf in
      let info = tokinfo lexbuf in

      match prev with
      | Some (
            T_IDENTIFIER _
          | T_NUMBER _ | T_STRING _ | T_REGEX _
          | T_FALSE _ | T_TRUE _ | T_NULL _
          | T_THIS _
          | T_INCR _ | T_DECR _
          | T_RBRACKET _ | T_RPAREN _
        ) -> begin match s with
          | "/" -> T_DIV (info);
          | "/=" -> T_DIV_ASSIGN info
          | _ -> assert false
        end
      | _ ->
          (* raise (Token t); *)
          let buf = Buffer.create 127 in
          Buffer.add_string buf s;
          regexp buf lexbuf;
          T_REGEX (Buffer.contents buf, info)
    }

  (* ----------------------------------------------------------------------- *)
  (* eof *)
  (* ----------------------------------------------------------------------- *)

  | eof { EOF (tokinfo lexbuf) }

  | _ {
      (* Format.eprintf "LEXER:unrecognised symbol, in token rule: %s@." (tok lexbuf); *)
      TUnknown (tokinfo lexbuf, tok lexbuf)
    }
(*****************************************************************************)

and string_escape quote buf = parse
  | '\\'{ Buffer.add_string buf "\\\\" }
  | 'x' hexa hexa
  | 'u' hexa hexa hexa hexa {
      Buffer.add_char buf '\\';
      Buffer.add_string buf (Lexing.lexeme lexbuf) }
  | (_ as c)
    { if c <> '\'' && c <> '\"' then Buffer.add_char buf '\\';
      Buffer.add_char buf c }
  | eof { Format.eprintf  "LEXER: WIERD end of file in string_escape@."; ()}

and string_quote q buf = parse
  | ("'"|'"') as q' {
    if q = q'
    then ()
    else (Buffer.add_char buf q'; string_quote q buf lexbuf) }
  | '\\' {
      string_escape q buf lexbuf;
      string_quote q buf lexbuf
    }
  | (_ as x)       { Buffer.add_char buf x; string_quote q buf lexbuf }
  | eof { Format.eprintf  "LEXER: WIERD end of file in quoted string@."; ()}

(*****************************************************************************)
and regexp buf = parse
  | '\\' (_ as x) { Buffer.add_char buf '\\';
                    Buffer.add_char buf x;
                    regexp buf lexbuf }
  | '/' { Buffer.add_char buf '/'; regexp_maybe_ident buf lexbuf }
  | '[' { Buffer.add_char buf '['; regexp_class buf lexbuf }
  | (_ as x)       { Buffer.add_char buf x; regexp buf lexbuf }
  | eof { Format.eprintf "LEXER: WIERD end of file in regexp@."; ()}

and regexp_class buf = parse
  | ']' { Buffer.add_char buf ']';
             regexp buf lexbuf }
  | '\\' (_ as x) { Buffer.add_char buf '\\';
                    Buffer.add_char buf x;
                    regexp_class buf lexbuf }
  | (_ as x) { Buffer.add_char buf x; regexp_class buf lexbuf }
  | eof { Format.eprintf "LEXER: WIERD end of file in regexp_class@."; ()}

and regexp_maybe_ident buf = parse
  | ['A'-'Z''a'-'z']* { Buffer.add_string buf (tok lexbuf) }

(*****************************************************************************)

and st_comment buf nl = parse
  | "*/" { Buffer.add_string buf (tok lexbuf) }

  (* noteopti: *)
  | NEWLINE { Buffer.add_string buf (tok lexbuf);
              nl := true;
              st_comment buf nl lexbuf }
  | [^'*' '\n' '\r' ]+ { Buffer.add_string buf (tok lexbuf);st_comment buf nl lexbuf }
  | '*'     { Buffer.add_char buf '*';st_comment buf nl lexbuf }

  | eof { Format.eprintf "LEXER: end of file in comment@."; Buffer.add_string buf "*/"}
  | _  {
      let s = tok lexbuf in
      Format.eprintf "LEXER: unrecognised symbol in comment: %s@." s;
      Buffer.add_string buf s;
      st_comment buf nl lexbuf
    }

and pos = parse
  | '#' [' ' '\t' ]+ (['0'-'9']+ as line) [' ' '\t' ]+ (("'"|'"') as quote) {
      let buf = Buffer.create 127 in
      string_quote quote buf lexbuf;
      Buffer.contents buf, int_of_string line }
