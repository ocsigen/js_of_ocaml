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
    "break",      T_BREAK;
    "case",       T_CASE;
    "catch",      T_CATCH;
    "continue",   T_CONTINUE;
    "debugger",   T_DEBUGGER;
    "default",    T_DEFAULT;
    "delete",     T_DELETE;
    "do",         T_DO;
    "else",       T_ELSE;
    "false",      T_FALSE;
    "finally",    T_FINALLY;
    "for",        T_FOR;
    "function",   T_FUNCTION;
    "if",         T_IF;
    "in",         T_IN;
    "instanceof", T_INSTANCEOF;
    "new",        T_NEW;
    "null",       T_NULL;
    "return",     T_RETURN;
    "switch",     T_SWITCH;
    "this",       T_THIS;
    "throw",      T_THROW;
    "true",       T_TRUE;
    "try",        T_TRY;
    "typeof",     T_TYPEOF;
    "var",        T_VAR;
    "void",       T_VOID;
    "while",      T_WHILE;
    "while",      T_WHILE;
    "with",       T_WITH;
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

let with_pos lexbuf f =
  let p = lexbuf.Lexing.lex_start_p in
  let pos = lexbuf.Lexing.lex_start_pos in
  let r = f () in
  lexbuf.Lexing.lex_start_p <- p;
  lexbuf.Lexing.lex_start_pos <- pos;
  r
}

(*****************************************************************************)

let NEWLINE = ("\r"|"\n"|"\r\n")
let hexa = ['0'-'9''a'-'f''A'-'F']
let inputCharacter = [^ '\r' '\n' ]
(*****************************************************************************)

rule main = parse

  (* ----------------------------------------------------------------------- *)
  (* spacing/comments *)
  (* ----------------------------------------------------------------------- *)
  | "/*" {
      with_pos lexbuf (fun () ->
      let buf = Buffer.create 127 in
      Buffer.add_string buf (tok lexbuf);
      st_comment buf lexbuf;
      let content = Buffer.contents buf in
      TComment content)
    }
  | ("//#" [' ' '\t' ]*
     (['0'-'9']+ as line) [' ' '\t' ]*
     '"' ([^ '"' '\n']* as file) '"' [' ' '\t' ]*
    ) as raw NEWLINE {
      let line = int_of_string line in
      update_loc lexbuf ~file ~line ~absolute:true 0;
      TCommentLineDirective raw
    }
  (* don't keep the trailing \n; it will be handled later *)
  | ("//" inputCharacter*) as cmt { TComment cmt }

  | [' ' '\t' ]+ {
      main lexbuf
    }
  | NEWLINE {
      update_loc lexbuf ~line:1 ~absolute:false 0;
      main lexbuf
    }

  (* ----------------------------------------------------------------------- *)
  (* symbols *)
  (* ----------------------------------------------------------------------- *)

  | "{" { T_LCURLY; }
  | "}" { T_RCURLY; }

  | "(" { T_LPAREN; }
  | ")" { T_RPAREN; }

  | "[" { T_LBRACKET; }
  | "]" { T_RBRACKET; }
  | "." { T_PERIOD; }
  | ";" { T_SEMICOLON; }
  | "," { T_COMMA; }
  | ":" { T_COLON; }
  | "?" { T_PLING; }
  | "&&" { T_AND; }
  | "||" { T_OR; }
  | "===" { T_STRICT_EQUAL; }
  | "!==" { T_STRICT_NOT_EQUAL; }
  | "<=" { T_LESS_THAN_EQUAL; }
  | ">=" { T_GREATER_THAN_EQUAL; }
  | "==" { T_EQUAL; }
  | "!=" { T_NOT_EQUAL; }
  | "++" { T_INCR; }
  | "--" { T_DECR; }
  | "<<=" { T_LSHIFT_ASSIGN; }
  | "<<" { T_LSHIFT; }
  | ">>=" { T_RSHIFT_ASSIGN; }
  | ">>>=" { T_RSHIFT3_ASSIGN; }
  | "..." { T_SPREAD; }
  | ">>>" { T_RSHIFT3; }
  | ">>" { T_RSHIFT; }
  | "+=" { T_PLUS_ASSIGN; }
  | "-=" { T_MINUS_ASSIGN; }

  | "*=" { T_MULT_ASSIGN; }
  | "%=" { T_MOD_ASSIGN; }
  | "&=" { T_BIT_AND_ASSIGN; }
  | "|=" { T_BIT_OR_ASSIGN; }
  | "^=" { T_BIT_XOR_ASSIGN; }
  | "<" { T_LESS_THAN; }
  | ">" { T_GREATER_THAN; }
  | "+" { T_PLUS; }
  | "-" { T_MINUS; }
  | "*" { T_MULT; }
  (* for '/' see below the regexp handling *)
  | "%" { T_MOD; }
  | "|" { T_BIT_OR; }
  | "&" { T_BIT_AND; }
  | "^" { T_BIT_XOR; }
  | "!" { T_NOT; }
  | "~" { T_BIT_NOT; }
  | "=" { T_ASSIGN; }

  (* ----------------------------------------------------------------------- *)
  (* Keywords and ident *)
  (* ----------------------------------------------------------------------- *)
  | ['a'-'z''A'-'Z''$''_']['a'-'z''A'-'Z''$''_''0'-'9']* {
      let s = tok lexbuf in
      try
        Hashtbl.find keyword_table s
      with
        | Not_found -> T_IDENTIFIER (Utf8_string.of_string_exn s)
    }

  (* ----------------------------------------------------------------------- *)
  (* Constant *)
  (* ----------------------------------------------------------------------- *)

  | "0" ['X''x'] hexa+ {
      let s = tok lexbuf in
      T_NUMBER s
    }
  | '0'['0'-'7']+ {
      let s = tok lexbuf in
      T_NUMBER s
    }

  | ['0'-'9']*'.'?['0'-'9']+['e''E']['-''+']?['0'-'9']+ (* {1,3} *) {
      let s = tok lexbuf in
      T_NUMBER s
    }
  | ['0'-'9']+'.'? |
    ['0'-'9']*'.'['0'-'9']+ {
      let s = tok lexbuf in
      T_NUMBER s
    }

  (* ----------------------------------------------------------------------- *)
  (* Strings *)
  (* ----------------------------------------------------------------------- *)
  | ("'"|'"') as quote {
      with_pos lexbuf (fun () ->
      let from = lexbuf.Lexing.lex_start_p.pos_cnum in
      let buf = Buffer.create 127 in
      string_quote quote buf lexbuf;
      let s = Buffer.contents buf in
      (* s does not contain the enclosing "'" but the info does *)
      let to_ = lexbuf.Lexing.lex_curr_p.pos_cnum in
      let s =
          if String.is_valid_utf_8 s
          then Utf8_string.of_string_exn s
          else (
            Format.eprintf "LEXER: invalid utf8 string.@.";
            Utf8_string.of_string_exn (String.fix_utf_8 s))
      in
      T_STRING (s, to_ - 1 - from))
    }
  | "/" { T_DIV }
  | "/=" { T_DIV_ASSIGN }
  (* ----------------------------------------------------------------------- *)
  (* eof *)
  (* ----------------------------------------------------------------------- *)

  | eof { EOF }

  | _ {
      TUnknown (tok lexbuf)
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
and main_regexp = parse
  | '/' {
      with_pos lexbuf (fun () ->
      let buf = Buffer.create 127 in
      Buffer.add_string buf (Lexing.lexeme lexbuf);
      regexp buf lexbuf;
      T_REGEX (Buffer.contents buf)) }

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
