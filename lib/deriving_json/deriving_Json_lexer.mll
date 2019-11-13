(*
Copyright (c) 2010 Martin Jambon
Copyright (c) 2010 Grégoire Henry
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions
are met:
1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.
3. The name of the author may not be used to endorse or promote products
   derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

*)
{
open! Deriving_Json_import
module Lexing =
    (*
      We override Lexing.engine in order to avoid creating a new position
      record each time a rule is matched.
      This reduces total parsing time by about 31%.
    *)
struct
  include Lexing

  external c_engine : lex_tables -> int -> lexbuf -> int = "caml_lex_engine"

  let engine tbl state buf =
    let result = c_engine tbl state buf in
      (*
      if result >= 0 then begin
  buf.lex_start_p <- buf.lex_curr_p;
  buf.lex_curr_p <- {buf.lex_curr_p
         with pos_cnum = buf.lex_abs_pos + buf.lex_curr_pos};
      end;
      *)
    result
end

open Printf
open Lexing

type lexbuf = {
  buf : Buffer.t;
  (* Buffer used to accumulate substrings *)

  mutable lnum : int;
  (* Current line number (starting from 1) *)

  mutable bol : int;
  (* Absolute position of the first character of the current line
     (starting from 0) *)

  lexbuf : Lexing.lexbuf;

}

let dec c =
  Char.code c - 48

let hex c =
  match c with
    '0'..'9' -> int_of_char c - int_of_char '0'
  | 'a'..'f' -> int_of_char c - int_of_char 'a' + 10
  | 'A'..'F' -> int_of_char c - int_of_char 'A' + 10
  | _ -> assert false

let json_error msg = failwith ("Deriving.Json: " ^ msg)

let custom_error descr v lexbuf =
  let offs = lexbuf.lex_abs_pos in
  let bol = v.bol in
  let pos1 = offs + lexbuf.lex_start_pos - bol in
  let pos2 = max pos1 (offs + lexbuf.lex_curr_pos - bol - 1) in
  let bytes =
    if pos1 = pos2 then
      sprintf "byte %i" (pos1+1)
    else
      sprintf "bytes %i-%i" (pos1+1) (pos2+1)
  in
  let msg = sprintf "Line %i, %s:\n%s" v.lnum bytes descr in
  json_error msg

let eof_error v lexbuf = custom_error "Unexpected end of input" v lexbuf
let byte_error v lexbuf = custom_error "Unexpected byte in string" v lexbuf
let tag_error ~typename v =
  custom_error
    (Printf.sprintf "Unexpected constructor %s for Json_%s" (Lexing.lexeme v.lexbuf) typename)
    v v.lexbuf

let lexer_error descr v lexbuf =
  custom_error
    (sprintf "%s '%s'" descr (Lexing.lexeme lexbuf))
    v lexbuf

let min10 = min_int / 10 - (if min_int mod 10 = 0 then 0 else 1)
let max10 = max_int / 10 + (if max_int mod 10 = 0 then 0 else 1)

exception Int_overflow

let extract_positive_int lexbuf =
  let start = lexbuf.lex_start_pos in
  let stop = lexbuf.lex_curr_pos in
  let s = lexbuf.lex_buffer in
  let n = ref 0 in
  for i = start to stop - 1 do
    if !n >= max10 then
      raise Int_overflow
    else
      n := 10 * !n + dec (Bytes.get s i)
  done;
  if !n < 0 then
    raise Int_overflow
  else
    !n

let extract_negative_int lexbuf =
  let start = lexbuf.lex_start_pos + 1 in
  let stop = lexbuf.lex_curr_pos in
  let s = lexbuf.lex_buffer in
  let n = ref 0 in
  for i = start to stop - 1 do
    if !n <= min10 then
      raise Int_overflow
    else
      n := 10 * !n - dec (Bytes.get s i)
  done;
  if !n > 0 then
    raise Int_overflow
  else
    !n

let newline v lexbuf =
  v.lnum <- v.lnum + 1;
  v.bol <- lexbuf.lex_abs_pos + lexbuf.lex_curr_pos

}

let space = [' ' '\t' '\r']+

let digit = ['0'-'9']
let nonzero = ['1'-'9']
let digits = digit+
let frac = '.' digits
let e = ['e' 'E']['+' '-']?
let exp = e digits

let positive_int = (digit | nonzero digits)
let float = '-'? positive_int (frac | exp | frac exp)
let number = '-'? positive_int (frac | exp | frac exp)?

let hex = [ '0'-'9' 'a'-'f' 'A'-'F' ]

let ident = ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '_' '0'-'9']*

rule finish_string v = parse
    '"'    { Buffer.contents v.buf }
  | '\\'   { finish_escaped_char v lexbuf;
       finish_string v lexbuf }
  | _ as c { if Poly.(c < '\x80') then
               Buffer.add_char v.buf c
             else
               finish_utf8_encoded_byte v c lexbuf;
             finish_string v lexbuf }
  | eof    { eof_error v lexbuf }

and finish_utf8_encoded_byte v c1 = parse
  | _ as c2 { (* Even if encoded in UTF-8, a byte could not be greater than 255 ! *)
              if Poly.('\xC2' <= c1) && Poly.(c1 < '\xC4') && Poly.('\x80' <= c2) && Poly.(c2 < '\xC0') then
                let c = ((Char.code c1 lsl 6) lor Char.code c2) land 0xFF in
                Buffer.add_char v.buf (Char.chr c)
              else
                byte_error v lexbuf }
  | eof     { eof_error v lexbuf }

and finish_escaped_char v = parse
    '"'
  | '\\'
  | '/' as c { Buffer.add_char v.buf c }
  | 'b'  { Buffer.add_char v.buf '\b' }
  | 'f'  { Buffer.add_char v.buf '\012' }
  | 'n'  { Buffer.add_char v.buf '\n' }
  | 'r'  { Buffer.add_char v.buf '\r' }
  | 't'  { Buffer.add_char v.buf '\t' }
  | 'u' (hex as a) (hex as b) (hex as c) (hex as d)
         { (* Even if encoded in UTF-8, a byte could not be greater than 255 ! *)
            if hex a = 0 && hex b = 0 then
       let c = (hex c lsl 4) lor hex d in
             Buffer.add_char v.buf (Char.chr c)
           else
       byte_error v lexbuf
   }
  | _    { lexer_error "Invalid escape sequence" v lexbuf }
  | eof  { eof_error v lexbuf }

and read_comma v = parse
  | ','   { () }
  | _     { lexer_error "Expected ',' but found" v lexbuf }
  | eof   { eof_error v lexbuf }

and read_comma_or_rbracket v = parse
  | ','   { `Comma }
  | ']'   { `RBracket }
  | _     { lexer_error "Expected ',' or ']' but found" v lexbuf }
  | eof   { eof_error v lexbuf }

and finish_comment v = parse
  | "*/" { () }
  | eof  { lexer_error "Unterminated comment" v lexbuf }
  | '\n' { newline v lexbuf; finish_comment v lexbuf }
  | _    { finish_comment v lexbuf }

(* Readers expecting a particular JSON construct *)

and read_space v = parse
  | "//"[^'\n']* ('\n'|eof)  { newline v lexbuf; read_space v lexbuf }
  | "/*"                     { finish_comment v lexbuf; read_space v lexbuf }
  | '\n'                     { newline v lexbuf; read_space v lexbuf }
  | [' ' '\t' '\r']+         { read_space v lexbuf }
  | ""                       { () }

and read_int v = parse
    positive_int         { try extract_positive_int lexbuf
         with Int_overflow ->
           lexer_error "Int overflow" v lexbuf }
  | '-' positive_int     { try extract_negative_int lexbuf
         with Int_overflow ->
           lexer_error "Int overflow" v lexbuf }
  | _                    { lexer_error "Expected integer but found" v lexbuf }
  | eof                  { eof_error v lexbuf }

and read_positive_int v = parse
    positive_int         { try extract_positive_int lexbuf
         with Int_overflow ->
           lexer_error "Int overflow" v lexbuf }
  | _                    { lexer_error "Expected integer but found" v lexbuf }
  | eof                  { eof_error v lexbuf }

and read_int32 v = parse
    '-'? positive_int    { try Int32.of_string (Lexing.lexeme lexbuf)
         with _ ->
           lexer_error "Int32 overflow" v lexbuf }
  | _                    { lexer_error "Expected int32 but found" v lexbuf }
  | eof                  { eof_error v lexbuf }

and read_int64 v = parse
    '-'? positive_int    { try Int64.of_string (Lexing.lexeme lexbuf)
         with _ ->
           lexer_error "Int32 overflow" v lexbuf }
  | _                    { lexer_error "Expected int64 but found" v lexbuf }
  | eof                  { eof_error v lexbuf }

and read_number v = parse
  | "NaN"       { nan }
  | "Infinity"  { infinity }
  | "-Infinity" { neg_infinity }
  | number      { float_of_string (lexeme lexbuf) }
  | _           { lexer_error "Expected number but found" v lexbuf }
  | eof         { eof_error v lexbuf }

and read_string v = parse
    '"'      { Buffer.clear v.buf;
         finish_string v lexbuf }
  | _        { lexer_error "Expected '\"' but found" v lexbuf }
  | eof      { eof_error v lexbuf }

and read_lbracket v = parse
    '['      { () }
  | _        { lexer_error "Expected '[' but found" v lexbuf }
  | eof      { eof_error v lexbuf }

and read_rbracket v = parse
    ']'      { () }
  | _        { lexer_error "Expected ']' but found" v lexbuf }
  | eof      { eof_error v lexbuf }

and read_case v = parse
  | positive_int { try `Cst (extract_positive_int lexbuf)
                       with Int_overflow -> lexer_error "Int overflow" v lexbuf }
  | '['          { read_space v lexbuf;
       `NCst (read_positive_int v lexbuf) }
  | _            { lexer_error "Expected positive integer or '[' but found" v lexbuf }
  | eof          { eof_error v lexbuf }

and read_vcase v = parse
  | positive_int { try `Cst (extract_positive_int lexbuf)
                       with Int_overflow -> lexer_error "Int overflow" v lexbuf }
  | '-'? positive_int { try `Cst (extract_negative_int lexbuf)
                       with Int_overflow -> lexer_error "Int overflow" v lexbuf }
  | '['          { read_space v lexbuf;
       let zero = read_positive_int v lexbuf in
       if (zero <> 0) then
         lexer_error
           (Printf.sprintf "Expected 0 but found %d" zero) v lexbuf;
       read_space v lexbuf;
       read_comma v lexbuf;
       read_space v lexbuf;
       `NCst (read_int v lexbuf) }
  | _            { lexer_error "Expected positive integer or '[' but found" v lexbuf }
  | eof          { eof_error v lexbuf }

{

let init_lexer ?buf lexbuf =
  let buf =
    match buf with
      None -> Buffer.create 256
    | Some buf -> buf
  in
  {
    buf = buf;
    lnum = 1;
    bol = 0;
    lexbuf = lexbuf;
  }

let read_bounded_int min max v lexbuf =
  let n = read_int v lexbuf in
  if n < min || n > max then
    lexer_error (Printf.sprintf "Int outside of bounds [%d - %d]" min max) v lexbuf
  else
    n

let read_tag_1 n v lexbuf =
  if n = read_int v lexbuf
  then n
  else lexer_error (Printf.sprintf "Int expected to be %d" n) v lexbuf

let read_tag_2 n1 n2 v lexbuf =
  let n = read_int v lexbuf in
  if n = n1 || n = n2
  then n
  else lexer_error (Printf.sprintf "Int expected to be either %d or %d" n1 n2) v lexbuf

let read_int v = read_space v v.lexbuf; read_int v v.lexbuf
let read_bounded_int ?(min = 0) ~max v =
  read_space v v.lexbuf; read_bounded_int min max v v.lexbuf
let read_tag_1 n v =
  read_space v v.lexbuf; read_tag_1 n v v.lexbuf
let read_tag_2 n1 n2 v =
  read_space v v.lexbuf; read_tag_2 n1 n2 v v.lexbuf
let read_int32 v = read_space v v.lexbuf; read_int32 v v.lexbuf
let read_int64 v = read_space v v.lexbuf; read_int64 v v.lexbuf
let read_number v = read_space v v.lexbuf; read_number v v.lexbuf
let read_string v = read_space v v.lexbuf; read_string v v.lexbuf

let read_case v = read_space v v.lexbuf; read_case v v.lexbuf
let read_vcase v = read_space v v.lexbuf; read_vcase v v.lexbuf

let read_lbracket v = read_space v v.lexbuf; read_lbracket v v.lexbuf
let read_rbracket v = read_space v v.lexbuf; read_rbracket v v.lexbuf
let read_comma v = read_space v v.lexbuf; read_comma v v.lexbuf
let read_comma_or_rbracket v =
  read_space v v.lexbuf; read_comma_or_rbracket v v.lexbuf

}
