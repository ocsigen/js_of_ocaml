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

open Stdlib
open Js_token

let tok lexbuf = Lexing.lexeme lexbuf

let keyword_table =
  let h = Hashtbl.create 17 in
  List.iter ~f:(fun (s,f) -> Hashtbl.add h s f ) [
    "break",      (fun ii -> T_BREAK ii);
    "case",       (fun ii -> T_CASE ii);
    "catch",      (fun ii -> T_CATCH ii);
    "continue",   (fun ii -> T_CONTINUE ii);
    "debugger",   (fun ii -> T_DEBUGGER ii);
    "default",    (fun ii -> T_DEFAULT ii);
    "delete",     (fun ii -> T_DELETE ii);
    "do",         (fun ii -> T_DO ii);
    "else",       (fun ii -> T_ELSE ii);
    "false",      (fun ii -> T_FALSE ii);
    "finally",    (fun ii -> T_FINALLY ii);
    "for",        (fun ii -> T_FOR ii);
    "function",   (fun ii -> T_FUNCTION ii);
    "if",         (fun ii -> T_IF ii);
    "in",         (fun ii -> T_IN ii);
    "instanceof", (fun ii -> T_INSTANCEOF ii);
    "new",        (fun ii -> T_NEW ii);
    "null",       (fun ii -> T_NULL ii);
    "return",     (fun ii -> T_RETURN ii);
    "switch",     (fun ii -> T_SWITCH ii);
    "this",       (fun ii -> T_THIS ii);
    "throw",      (fun ii -> T_THROW ii);
    "true",       (fun ii -> T_TRUE ii);
    "try",        (fun ii -> T_TRY ii);
    "typeof",     (fun ii -> T_TYPEOF ii);
    "var",        (fun ii -> T_VAR ii);
    "void",       (fun ii -> T_VOID ii);
    "while",      (fun ii -> T_WHILE ii);
    "while",      (fun ii -> T_WHILE ii);
    "with",       (fun ii -> T_WITH ii);
  ];
  h

(* Update the current location with file name and line number. *)

let update_loc lexbuf ?file ~line ~absolute chars =
  let pos = lexbuf.Lexing.lex_curr_p in
  let new_file = match file with
                 | None -> pos.pos_fname
                 | Some s -> s
  in
  lexbuf.Lexing.lex_curr_p <- { pos with
    pos_fname = new_file;
    pos_lnum = if absolute then line else pos.pos_lnum + line;
    pos_bol = pos.pos_cnum - chars;
                              }

let tokinfo prev lexbuf =
  let pi = Parse_info.t_of_lexbuf lexbuf in
  match prev with
  | None -> { pi with Parse_info.fol = Yes }
  | Some prev ->
    let prev_pi = Js_token.info prev in
    if prev_pi.Parse_info.line <> pi.Parse_info.line
    && Option.equal String.equal prev_pi.Parse_info.name pi.Parse_info.name
    then { pi with Parse_info.fol = Yes }
    else { pi with Parse_info.fol = No }

}

(*****************************************************************************)

let NEWLINE = ("\r"|"\n"|"\r\n")
let hexa = ['0'-'9''a'-'f''A'-'F']
let inputCharacter = [^ '\r' '\n' ]
(*****************************************************************************)

rule main prev = parse

  (* ----------------------------------------------------------------------- *)
  (* spacing/comments *)
  (* ----------------------------------------------------------------------- *)
  | "/*" {
      let info = tokinfo prev lexbuf in
      let buf = Buffer.create 127 in
      Buffer.add_string buf (tok lexbuf);
      st_comment buf lexbuf;
      let content = Buffer.contents buf in
      TComment(content, info)
    }
  | ("//#" [' ' '\t' ]*
     (['0'-'9']+ as line) [' ' '\t' ]*
     '"' ([^ '"' '\n']* as file) '"' [' ' '\t' ]*
    ) as raw NEWLINE {
      let info = tokinfo prev lexbuf in
      let line = int_of_string line in
      update_loc lexbuf ~file ~line ~absolute:true 0;
      TCommentLineDirective (raw, info)
    }
  (* don't keep the trailing \n; it will be handled later *)
  | ("//" inputCharacter*) as cmt { TComment(cmt, tokinfo prev lexbuf) }

  | [' ' '\t' ]+ {
      main prev lexbuf
    }
  | NEWLINE {
      update_loc lexbuf ~line:1 ~absolute:false 0;
      main prev lexbuf
    }

  (* ----------------------------------------------------------------------- *)
  (* symbols *)
  (* ----------------------------------------------------------------------- *)

  | "{" { T_LCURLY (tokinfo prev lexbuf); }
  | "}" { T_RCURLY (tokinfo prev lexbuf); }

  | "(" { T_LPAREN (tokinfo prev lexbuf); }
  | ")" { T_RPAREN (tokinfo prev lexbuf); }

  | "[" { T_LBRACKET (tokinfo prev lexbuf); }
  | "]" { T_RBRACKET (tokinfo prev lexbuf); }
  | "." { T_PERIOD (tokinfo prev lexbuf); }
  | ";" { T_SEMICOLON (tokinfo prev lexbuf); }
  | "," { T_COMMA (tokinfo prev lexbuf); }
  | ":" { T_COLON (tokinfo prev lexbuf); }
  | "?" { T_PLING (tokinfo prev lexbuf); }
  | "&&" { T_AND (tokinfo prev lexbuf); }
  | "||" { T_OR (tokinfo prev lexbuf); }
  | "===" { T_STRICT_EQUAL (tokinfo prev lexbuf); }
  | "!==" { T_STRICT_NOT_EQUAL (tokinfo prev lexbuf); }
  | "<=" { T_LESS_THAN_EQUAL (tokinfo prev lexbuf); }
  | ">=" { T_GREATER_THAN_EQUAL (tokinfo prev lexbuf); }
  | "==" { T_EQUAL (tokinfo prev lexbuf); }
  | "!=" { T_NOT_EQUAL (tokinfo prev lexbuf); }
  | "++" {
      let cpi = tokinfo prev lexbuf in
      match prev with
        | Some p when (Js_token.info p).Parse_info.line = cpi.Parse_info.line ->
          T_INCR_NB(cpi)
        | _ -> T_INCR(cpi) }
  | "--" {
      let cpi = tokinfo prev lexbuf in
      match prev with
        | Some p when (Js_token.info p).Parse_info.line = cpi.Parse_info.line ->
          T_DECR_NB(cpi)
        | _ -> T_DECR(cpi) }
  | "<<=" { T_LSHIFT_ASSIGN (tokinfo prev lexbuf); }
  | "<<" { T_LSHIFT (tokinfo prev lexbuf); }
  | ">>=" { T_RSHIFT_ASSIGN (tokinfo prev lexbuf); }
  | ">>>=" { T_RSHIFT3_ASSIGN (tokinfo prev lexbuf); }
  | "..." { T_SPREAD (tokinfo prev lexbuf); }
  | ">>>" { T_RSHIFT3 (tokinfo prev lexbuf); }
  | ">>" { T_RSHIFT (tokinfo prev lexbuf); }
  | "+=" { T_PLUS_ASSIGN (tokinfo prev lexbuf); }
  | "-=" { T_MINUS_ASSIGN (tokinfo prev lexbuf); }

  | "*=" { T_MULT_ASSIGN (tokinfo prev lexbuf); }
  | "%=" { T_MOD_ASSIGN (tokinfo prev lexbuf); }
  | "&=" { T_BIT_AND_ASSIGN (tokinfo prev lexbuf); }
  | "|=" { T_BIT_OR_ASSIGN (tokinfo prev lexbuf); }
  | "^=" { T_BIT_XOR_ASSIGN (tokinfo prev lexbuf); }
  | "<" { T_LESS_THAN (tokinfo prev lexbuf); }
  | ">" { T_GREATER_THAN (tokinfo prev lexbuf); }
  | "+" { T_PLUS (tokinfo prev lexbuf); }
  | "-" { T_MINUS (tokinfo prev lexbuf); }
  | "*" { T_MULT (tokinfo prev lexbuf); }
  (* for '/' see below the regexp handling *)
  | "%" { T_MOD (tokinfo prev lexbuf); }
  | "|" { T_BIT_OR (tokinfo prev lexbuf); }
  | "&" { T_BIT_AND (tokinfo prev lexbuf); }
  | "^" { T_BIT_XOR (tokinfo prev lexbuf); }
  | "!" { T_NOT (tokinfo prev lexbuf); }
  | "~" { T_BIT_NOT (tokinfo prev lexbuf); }
  | "=" { T_ASSIGN (tokinfo prev lexbuf); }

  (* ----------------------------------------------------------------------- *)
  (* Keywords and ident *)
  (* ----------------------------------------------------------------------- *)
  | ['a'-'z''A'-'Z''$''_']['a'-'z''A'-'Z''$''_''0'-'9']* {
      let s = tok lexbuf in
      let info = tokinfo prev lexbuf in
      try
        let f = Hashtbl.find keyword_table s in
        f info
      with
        | Not_found -> T_IDENTIFIER (s, info)
    }

  (* ----------------------------------------------------------------------- *)
  (* Constant *)
  (* ----------------------------------------------------------------------- *)

  | "0" ['X''x'] hexa+ {
      let s = tok lexbuf in
      let info = tokinfo prev lexbuf in
      T_NUMBER (s, info)
    }
  | '0'['0'-'7']+ {
      let s = tok lexbuf in
      let info = tokinfo prev lexbuf in
      T_NUMBER (s, info)
    }

  | ['0'-'9']*'.'?['0'-'9']+['e''E']['-''+']?['0'-'9']+ (* {1,3} *) {
      let s = tok lexbuf in
      let info = tokinfo prev lexbuf in
      T_NUMBER (s, info)
    }
  | ['0'-'9']+'.'? |
    ['0'-'9']*'.'['0'-'9']+ {
      let s = tok lexbuf in
      let info = tokinfo prev lexbuf in
      T_NUMBER (s, info)
    }

  (* ----------------------------------------------------------------------- *)
  (* Strings *)
  (* ----------------------------------------------------------------------- *)
  | ("'"|'"') as quote {
      let from = lexbuf.Lexing.lex_start_p.pos_cnum in
      let info = tokinfo prev lexbuf in
      let buf = Buffer.create 127 in
      string_quote quote buf lexbuf;
      let s = Buffer.contents buf in
      (* s does not contain the enclosing "'" but the info does *)
      let to_ = lexbuf.Lexing.lex_curr_p.pos_cnum in
      T_STRING (s, info, to_ - 1 - from)
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
      let info = tokinfo prev lexbuf in

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
          let buf = Buffer.create 127 in
          Buffer.add_string buf s;
          regexp buf lexbuf;
          T_REGEX (Buffer.contents buf, info)
    }

  (* ----------------------------------------------------------------------- *)
  (* eof *)
  (* ----------------------------------------------------------------------- *)

  | eof { EOF (tokinfo prev lexbuf) }

  | _ {
      TUnknown (tok lexbuf, tokinfo prev lexbuf)
    }
(*****************************************************************************)

and string_escape quote buf = parse
  | '\\'{ Buffer.add_string buf "\\\\" }
  | 'x' hexa hexa
  | 'u' hexa hexa hexa hexa {
      Buffer.add_char buf '\\';
      Buffer.add_string buf (tok lexbuf) }
  | (_ as c)
    { if Char.(c <> '\'') && Char.(c <> '\"') then Buffer.add_char buf '\\';
      Buffer.add_char buf c }
  | eof { Format.eprintf  "LEXER: WEIRD end of file in string_escape@."; ()}

and string_quote q buf = parse
  | ("'"|'"') as q' {
    if Char.(q = q')
    then ()
    else (Buffer.add_char buf q'; string_quote q buf lexbuf) }
  | "\\" NEWLINE {
    update_loc lexbuf ~line:1 ~absolute:false 0;
    string_quote q buf lexbuf }
  | NEWLINE {
    Format.eprintf  "LEXER: WEIRD newline in quoted string@.";
    update_loc lexbuf ~line:1 ~absolute:false 0;
    Buffer.add_string buf (tok lexbuf);
    string_quote q buf lexbuf }
  | '\\' {
      string_escape q buf lexbuf;
      string_quote q buf lexbuf
    }
  | (_ as x)       { Buffer.add_char buf x; string_quote q buf lexbuf }
  | eof { Format.eprintf  "LEXER: WEIRD end of file in quoted string@."; ()}

(*****************************************************************************)
and regexp buf = parse
  | '\\' (_ as x) { Buffer.add_char buf '\\';
                    Buffer.add_char buf x;
                    regexp buf lexbuf }
  | '/' { Buffer.add_char buf '/'; regexp_maybe_ident buf lexbuf }
  | '[' { Buffer.add_char buf '['; regexp_class buf lexbuf }
  | ([^ '\n'] as x)       { Buffer.add_char buf x; regexp buf lexbuf }
  | '\n' { Format.eprintf "LEXER: WEIRD newline in regexp@.";
           update_loc lexbuf ~line:1 ~absolute:false 0;
           ()}
  | eof { Format.eprintf "LEXER: WEIRD end of file in regexp@."; ()}

and regexp_class buf = parse
  | ']' { Buffer.add_char buf ']';
             regexp buf lexbuf }
  | '\\' (_ as x) { Buffer.add_char buf '\\';
                    Buffer.add_char buf x;
                    regexp_class buf lexbuf }
  | ([^ '\n'] as x) { Buffer.add_char buf x; regexp_class buf lexbuf }
  | '\n' { Format.eprintf "LEXER: WEIRD newline in regexp_class@.";
           update_loc lexbuf ~line:1 ~absolute:false 0;
           ()}
  | eof { Format.eprintf "LEXER: WEIRD end of file in regexp_class@."; ()}

and regexp_maybe_ident buf = parse
  | ['A'-'Z''a'-'z']* { Buffer.add_string buf (tok lexbuf) }

(*****************************************************************************)

and st_comment buf = parse
  | "*/" { Buffer.add_string buf (tok lexbuf) }
  | NEWLINE {
      update_loc lexbuf ~line:1 ~absolute:false 0;
      Buffer.add_string buf (tok lexbuf);
      st_comment buf lexbuf }
  | [^'*' '\n' '\r' ]+ { Buffer.add_string buf (tok lexbuf);st_comment buf lexbuf }
  | '*'     { Buffer.add_char buf '*';st_comment buf lexbuf }

  | eof { Format.eprintf "LEXER: end of file in comment@."; Buffer.add_string buf "*/"}
  | _  {
      let s = tok lexbuf in
      Format.eprintf "LEXER: unrecognised symbol in comment: %s@." s;
      Buffer.add_string buf s;
      st_comment buf lexbuf
    }
