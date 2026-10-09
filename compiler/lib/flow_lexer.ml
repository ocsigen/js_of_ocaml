(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Js_token

module Lex_mode = struct
  type t =
    | NORMAL
    | BACKQUOTE
    | REGEXP
end

module Parse_error = struct
  type t =
    | Unexpected of string
    | IllegalUnicodeEscape
    | InvalidSciBigInt
    | InvalidFloatBigInt
    | UnterminatedRegExp

  let to_string = function
    | Unexpected unexpected -> Printf.sprintf "Unexpected %s" unexpected
    | IllegalUnicodeEscape -> "Illegal Unicode escape"
    | InvalidSciBigInt -> "A bigint literal cannot use exponential notation"
    | InvalidFloatBigInt -> "A bigint literal must be an integer"
    | UnterminatedRegExp -> "Invalid regular expression: missing /"
end

(* The lexer state is a single mutable record: the lexer is used
   sequentially, and allocating a new environment per token is costly. *)
module Lex_env = struct
  type t =
    { lex_lb : Sedlexing.lexbuf
    ; mutable lex_errors_acc : (Loc.t * Parse_error.t) list
          (* errors of the current token, most recent first *)
    ; mutable lex_mode_stack : Lex_mode.t list
    ; mutable lex_last_line : Loc.line
          (* line of the end of the last token, shared between the
             locations of the tokens on the same line *)
    ; mutable lex_first_token_on_line : bool
          (* true if no non-comment token has been seen on the current line *)
    }

  let create lex_lb =
    { lex_lb
    ; lex_errors_acc = []
    ; lex_mode_stack = [ Lex_mode.NORMAL ]
    ; lex_last_line = Loc.dummy_line
    ; lex_first_token_on_line = true
    }

  let take_errors env =
    match env.lex_errors_acc with
    | [] -> []
    | l ->
        env.lex_errors_acc <- [];
        List.rev l
end

let push_mode (env : Lex_env.t) mode = env.lex_mode_stack <- mode :: env.lex_mode_stack

let pop_mode (env : Lex_env.t) =
  match env.lex_mode_stack with
  | [] -> ()
  | _ :: xs -> env.lex_mode_stack <- xs

(* The lexeme as a string. Fast path for ASCII lexemes, which is the
   common case: [Sedlexing.Utf8.lexeme] goes through a [Buffer]. *)
let lexeme lexbuf =
  let n = Sedlexing.lexeme_length lexbuf in
  let b = Bytes.create n in
  let rec loop i =
    if i = n
    then Bytes.unsafe_to_string b
    else
      let c = Uchar.to_int (Sedlexing.lexeme_char lexbuf i) in
      if c < 128
      then (
        Bytes.unsafe_set b i (Char.unsafe_chr c);
        loop (i + 1))
      else Sedlexing.Utf8.lexeme lexbuf
  in
  loop 0

let lexeme_to_buffer lexbuf b = Buffer.add_string b (lexeme lexbuf)

let letter = [%sedlex.regexp? 'a' .. 'z' | 'A' .. 'Z' | '$']

let id_letter = [%sedlex.regexp? letter | '_']

let digit = [%sedlex.regexp? '0' .. '9']

let digit_non_zero = [%sedlex.regexp? '1' .. '9']

let decintlit = [%sedlex.regexp? '0' | '1' .. '9', Star digit]

(* DecimalIntegerLiteral *)

let alphanumeric = [%sedlex.regexp? digit | letter]

let word = [%sedlex.regexp? letter, Star alphanumeric]

let hex_digit = [%sedlex.regexp? digit | 'a' .. 'f' | 'A' .. 'F']

let non_hex_letter = [%sedlex.regexp? 'g' .. 'z' | 'G' .. 'Z' | '$']

let bin_digit = [%sedlex.regexp? '0' | '1']

let oct_digit = [%sedlex.regexp? '0' .. '7']

(* This regex could be simplified to (digit Star (digit OR '_' digit))
 * That makes the underscore and failure cases faster, and the base case take x2-3 the steps
 * As the codebase contains more base cases than underscored or errors, prefer this version *)
let underscored_bin =
  [%sedlex.regexp? Plus bin_digit | bin_digit, Star (bin_digit | '_', bin_digit)]

let underscored_oct =
  [%sedlex.regexp? Plus oct_digit | oct_digit, Star (oct_digit | '_', oct_digit)]

let underscored_hex =
  [%sedlex.regexp? Plus hex_digit | hex_digit, Star (hex_digit | '_', hex_digit)]

let underscored_digit =
  [%sedlex.regexp? Plus digit | digit_non_zero, Star (digit | '_', digit)]

let underscored_decimal = [%sedlex.regexp? Plus digit | digit, Star (digit | '_', digit)]

(* Different ways you can write a number *)
let binnumber = [%sedlex.regexp? '0', ('B' | 'b'), underscored_bin]

let octnumber = [%sedlex.regexp? '0', ('O' | 'o'), underscored_oct]

let legacyoctnumber = [%sedlex.regexp? '0', Plus oct_digit]

(* no underscores allowed *)

let legacynonoctnumber = [%sedlex.regexp? '0', Star oct_digit, '8' .. '9', Star digit]

let hexnumber = [%sedlex.regexp? '0', ('X' | 'x'), underscored_hex]

let scinumber =
  [%sedlex.regexp?
    ( (decintlit, Opt ('.', Opt underscored_decimal) | '.', underscored_decimal)
    , ('e' | 'E')
    , Opt ('-' | '+')
    , underscored_digit )]

let integer = [%sedlex.regexp? underscored_digit]

let floatnumber = [%sedlex.regexp? Opt underscored_digit, '.', underscored_decimal]

let binbigint = [%sedlex.regexp? binnumber, 'n']

let octbigint = [%sedlex.regexp? octnumber, 'n']

let hexbigint = [%sedlex.regexp? hexnumber, 'n']

let wholebigint = [%sedlex.regexp? underscored_digit, 'n']

(* https://tc39.github.io/ecma262/#sec-white-space *)
let whitespace =
  [%sedlex.regexp?
    ( 0x0009 | 0x000B | 0x000C | 0x0020 | 0x00A0 | 0xfeff | 0x1680
    | 0x2000 .. 0x200a
    | 0x202f | 0x205f | 0x3000 )]

(* minus sign in front of negative numbers
   (only for types! regular numbers use T_MINUS!) *)
let neg = [%sedlex.regexp? '-', Star whitespace]

let line_terminator_sequence = [%sedlex.regexp? '\n' | '\r' | "\r\n" | 0x2028 | 0x2029]

let line_terminator_sequence_start = [%sedlex.regexp? '\n' | '\r' | 0x2028 | 0x2029]

let hex_quad = [%sedlex.regexp? hex_digit, hex_digit, hex_digit, hex_digit]

let unicode_escape = [%sedlex.regexp? "\\u", hex_quad]

let codepoint_escape = [%sedlex.regexp? "\\u{", Plus hex_digit, '}']

let js_id_start = [%sedlex.regexp? '$' | '_' | id_start]

let js_id_continue = [%sedlex.regexp? '$' | '_' | id_continue | 0x200C | 0x200D]

let js_id_start_with_escape =
  [%sedlex.regexp? js_id_start | unicode_escape | codepoint_escape]

let js_id_continue_with_escape =
  [%sedlex.regexp? js_id_continue | unicode_escape | codepoint_escape]

exception Not_an_ident

let is_basic_ident =
  let l =
    Array.init 256 (fun i ->
        let c = Char.chr i in
        match c with
        | 'a' .. 'z' | 'A' .. 'Z' | '_' | '$' -> 1
        | '0' .. '9' -> 2
        | _ -> 0)
  in
  fun s ->
    try
      for i = 0 to String.length s - 1 do
        let code = l.(Char.code s.[i]) in
        if i = 0
        then (if code <> 1 then raise Not_an_ident)
        else if code < 1
        then raise Not_an_ident
      done;
      true
    with Not_an_ident -> false

let is_valid_identifier_name s =
  is_basic_ident s
  ||
  let lexbuf = Sedlexing.Utf8.from_string s in
  match%sedlex lexbuf with
  | js_id_start, Star js_id_continue, eof -> true
  | _ -> false

(* Location of the current lexeme. The line record of the previous token
   is reused when the lexeme is on the same line. *)
let loc_of_lexbuf (env : Lex_env.t) (lexbuf : Sedlexing.lexbuf) =
  let start = Sedlexing.lexing_position_start lexbuf in
  let stop = Sedlexing.lexing_position_curr lexbuf in
  Loc.create ~last_line:env.lex_last_line start stop

let lex_error (env : Lex_env.t) loc err =
  env.lex_errors_acc <- (loc, err) :: env.lex_errors_acc

let illegal (env : Lex_env.t) (loc : Loc.t) reason =
  let reason =
    match reason with
    | "" -> "token ILLEGAL"
    | s -> s
  in
  lex_error env loc (Parse_error.Unexpected reason)

let decode_identifier =
  let sub_lexeme lexbuf trim_start trim_end =
    Sedlexing.Utf8.sub_lexeme
      lexbuf
      trim_start
      (Sedlexing.lexeme_length lexbuf - trim_start - trim_end)
  in
  let unicode_escape_code lexbuf =
    let hex = sub_lexeme lexbuf 2 0 in
    let code = int_of_string ("0x" ^ hex) in
    code
  in
  let codepoint_escape_code lexbuf =
    let hex = sub_lexeme lexbuf 3 1 in
    let code = int_of_string ("0x" ^ hex) in
    code
  in
  let is_high_surrogate c = 0xD800 <= c && c <= 0xDBFF in
  let is_low_surrogate c = 0xDC00 <= c && c <= 0xDFFF in
  let combine_surrogate hi lo =
    (((hi land 0x3FF) lsl 10) lor (lo land 0x3FF)) + 0x10000
  in
  let low_surrogate env loc buf lexbuf lead =
    lex_error env loc Parse_error.IllegalUnicodeEscape;
    match%sedlex lexbuf with
    | unicode_escape ->
        let code = unicode_escape_code lexbuf in
        if is_low_surrogate code
        then
          let code = combine_surrogate lead code in
          Buffer.add_utf_8_uchar buf (Uchar.of_int code)
        else lex_error env loc Parse_error.IllegalUnicodeEscape
    | codepoint_escape ->
        let code = codepoint_escape_code lexbuf in
        if is_low_surrogate code
        then
          let code = combine_surrogate lead code in
          Buffer.add_utf_8_uchar buf (Uchar.of_int code)
        else lex_error env loc Parse_error.IllegalUnicodeEscape
    | _ -> lex_error env loc Parse_error.IllegalUnicodeEscape
  in
  let rec id_char env loc buf lexbuf =
    match%sedlex lexbuf with
    | unicode_escape ->
        let code = unicode_escape_code lexbuf in
        if is_high_surrogate code
        then low_surrogate env loc buf lexbuf code
        else (
          if not (Uchar.is_valid code)
          then lex_error env loc Parse_error.IllegalUnicodeEscape;
          Buffer.add_utf_8_uchar buf (Uchar.of_int code));
        id_char env loc buf lexbuf
    | codepoint_escape ->
        let code = codepoint_escape_code lexbuf in
        if is_high_surrogate code
        then low_surrogate env loc buf lexbuf code
        else (
          if not (Uchar.is_valid code)
          then lex_error env loc Parse_error.IllegalUnicodeEscape;
          Buffer.add_utf_8_uchar buf (Uchar.of_int code));
        id_char env loc buf lexbuf
    | eof -> Buffer.contents buf
    (* match multi-char substrings that don't contain the start chars of the above patterns *)
    | Plus (Compl (eof | "\\")) | any ->
        lexeme_to_buffer lexbuf buf;
        id_char env loc buf lexbuf
    | _ -> failwith "unreachable id_char"
  in
  fun env loc raw ->
    let lexbuf = Sedlexing.Utf8.from_string raw in
    let buf = Buffer.create (String.length raw) in
    id_char env loc buf lexbuf

let recover env lexbuf ~f =
  illegal env (loc_of_lexbuf env lexbuf) "recovery";
  Sedlexing.rollback lexbuf;
  f lexbuf

type result =
  | Token of Js_token.t
  | Comment of string
  | Continue

let newline lexbuf =
  let start = Sedlexing.lexeme_start lexbuf in
  let stop = Sedlexing.lexeme_end lexbuf in
  let len = stop - start in
  let pending = ref false in
  for i = 0 to len - 1 do
    match Uchar.to_int (Sedlexing.lexeme_char lexbuf i) with
    | 0x000d -> pending := true
    | 0x000a -> pending := false
    | 0x2028 | 0x2029 ->
        if !pending
        then (
          pending := false;
          Sedlexing.new_line lexbuf);
        Sedlexing.new_line lexbuf
    | _ ->
        if !pending
        then (
          pending := false;
          Sedlexing.new_line lexbuf)
  done;
  if !pending then Sedlexing.new_line lexbuf

let rec comment env buf lexbuf =
  match%sedlex lexbuf with
  | line_terminator_sequence ->
      newline lexbuf;
      lexeme_to_buffer lexbuf buf;
      comment env buf lexbuf
  | "*/" -> lexeme_to_buffer lexbuf buf
  | "*-/" ->
      Buffer.add_string buf "*-/";
      comment env buf lexbuf
  (* match multi-char substrings that don't contain the start chars of the above patterns *)
  | Plus (Compl (line_terminator_sequence_start | '*')) | any ->
      lexeme_to_buffer lexbuf buf;
      comment env buf lexbuf
  | _ -> illegal env (loc_of_lexbuf env lexbuf) ""

let drop_line env =
  let lexbuf = env.Lex_env.lex_lb in
  match%sedlex lexbuf with
  | Star (Compl (eof | line_terminator_sequence_start)) -> ()
  | _ -> assert false

let rec line_comment env buf lexbuf =
  match%sedlex lexbuf with
  | eof -> ()
  | line_terminator_sequence -> Sedlexing.rollback lexbuf
  (* match multi-char substrings that don't contain the start chars of the above patterns *)
  | Plus (Compl (eof | line_terminator_sequence_start)) | any ->
      lexeme_to_buffer lexbuf buf;
      line_comment env buf lexbuf
  | _ -> failwith "unreachable line_comment"

let string_escape ~accept_invalid env lexbuf =
  match%sedlex lexbuf with
  | eof | '\\' ->
      let str = lexeme lexbuf in
      str
  | 'x', hex_digit, hex_digit ->
      let str = lexeme lexbuf in
      (* 0xAB *)
      str
  | '0' .. '7', '0' .. '7', '0' .. '7' ->
      let str = lexeme lexbuf in
      str
  | '0' .. '7', '0' .. '7' ->
      let str = lexeme lexbuf in
      (* 0o01 *)
      str
  | '0' -> "0"
  | 'b' -> "b"
  | 'f' -> "f"
  | 'n' -> "n"
  | 'r' -> "r"
  | 't' -> "t"
  | 'v' -> "v"
  | '0' .. '7' ->
      let str = lexeme lexbuf in
      (* 0o1 *)
      str
  | 'u', hex_quad ->
      let str = lexeme lexbuf in
      str
  | "u{", Plus hex_digit, '}' ->
      let str = lexeme lexbuf in
      let hex = String.sub str 2 (String.length str - 3) in
      let code = int_of_string ("0x" ^ hex) in
      (* 11.8.4.1 *)
      if code > 0x10FFFF && not accept_invalid
      then illegal env (loc_of_lexbuf env lexbuf) "unicode escape out of range";
      str
  | 'u' | 'x' | '0' .. '7' ->
      let str = lexeme lexbuf in
      if not accept_invalid then illegal env (loc_of_lexbuf env lexbuf) "";
      str
  | line_terminator_sequence ->
      newline lexbuf;
      let str = lexeme lexbuf in
      str
  | any ->
      let str = lexeme lexbuf in
      str
  | _ -> failwith "unreachable string_escape"

(* Really simple version of string lexing. Just try to find beginning and end of
 * string. We can inspect the string later to find invalid escapes, etc *)
let rec string_quote env q buf lexbuf =
  match%sedlex lexbuf with
  | "'" | '"' ->
      let q' = lexeme lexbuf in
      if q = q'
      then ()
      else (
        Buffer.add_string buf q';
        string_quote env q buf lexbuf)
  | '\\', line_terminator_sequence ->
      newline lexbuf;
      string_quote env q buf lexbuf
  | '\\' ->
      let str = string_escape ~accept_invalid:false env lexbuf in
      (match str with
      | "'" | "\"" -> ()
      | _ -> Buffer.add_string buf "\\");
      Buffer.add_string buf str;
      string_quote env q buf lexbuf
  | '\n' ->
      let x = lexeme lexbuf in
      Buffer.add_string buf x;
      illegal env (loc_of_lexbuf env lexbuf) "";
      string_quote env q buf lexbuf
  | eof ->
      let x = lexeme lexbuf in
      Buffer.add_string buf x;
      illegal env (loc_of_lexbuf env lexbuf) ""
  (* match multi-char substrings that don't contain the start chars of the above patterns *)
  | Plus (Compl ("'" | '"' | '\\' | '\n' | eof)) | any ->
      lexeme_to_buffer lexbuf buf;
      string_quote env q buf lexbuf
  | _ -> failwith "unreachable string_quote"

let token (env : Lex_env.t) lexbuf : result =
  match%sedlex lexbuf with
  | line_terminator_sequence ->
      newline lexbuf;
      env.lex_first_token_on_line <- true;
      Continue
  | Plus whitespace -> Continue
  | "/*" ->
      let buf = Buffer.create 127 in
      lexeme_to_buffer lexbuf buf;
      comment env buf lexbuf;
      Comment (Buffer.contents buf)
  | "//" ->
      let buf = Buffer.create 127 in
      lexeme_to_buffer lexbuf buf;
      line_comment env buf lexbuf;
      Comment (Buffer.contents buf)
  (* HTML-like comments: <!-- is treated as a single-line comment *)
  | "<!--" ->
      let buf = Buffer.create 127 in
      lexeme_to_buffer lexbuf buf;
      line_comment env buf lexbuf;
      Comment (Buffer.contents buf)
  (* HTML-like comments: --> is treated as a single-line comment
   * Note: Per spec, --> is only a comment if it's the first non-comment token on the line *)
  | "-->" ->
      if env.Lex_env.lex_first_token_on_line
      then (
        let buf = Buffer.create 127 in
        lexeme_to_buffer lexbuf buf;
        line_comment env buf lexbuf;
        Comment (Buffer.contents buf))
      else (
        Sedlexing.rollback lexbuf;
        match%sedlex lexbuf with
        | "--" -> Token T_DECR_NB
        | _ -> failwith "unreachable, expected ?")
  (* Support for the shebang at the beginning of a file. It is treated like a
   * comment at the beginning or an error elsewhere *)
  | "#!" ->
      if Sedlexing.lexeme_start lexbuf = 0
      then (
        line_comment env (Buffer.create 127) lexbuf;
        Continue)
      else Token (T_ERROR "#!")
  (* Values *)
  | "'" | '"' ->
      let quote = lexeme lexbuf in
      let p1 = Sedlexing.lexeme_start lexbuf in
      let buf = Buffer.create 127 in
      string_quote env quote buf lexbuf;
      let p2 = Sedlexing.lexeme_end lexbuf in
      Token
        (T_STRING (Stdlib.Utf8_string.of_string_exn (Buffer.contents buf), p2 - p1 - 1))
  | '`' ->
      push_mode env BACKQUOTE;
      Token T_BACKQUOTE
  | binbigint, word ->
      (* Numbers cannot be immediately followed by words *)
      recover env lexbuf ~f:(fun lexbuf ->
          match%sedlex lexbuf with
          | binbigint -> Token (T_BIGINT (BIG_BINARY, lexeme lexbuf))
          | _ -> failwith "unreachable token bigint")
  | binbigint -> Token (T_BIGINT (BIG_BINARY, lexeme lexbuf))
  | binnumber, (letter | '2' .. '9'), Star alphanumeric ->
      (* Numbers cannot be immediately followed by words *)
      recover env lexbuf ~f:(fun lexbuf ->
          match%sedlex lexbuf with
          | binnumber -> Token (T_NUMBER (BINARY, lexeme lexbuf))
          | _ -> failwith "unreachable token bignumber")
  | binnumber -> Token (T_NUMBER (BINARY, lexeme lexbuf))
  | octbigint, word ->
      (* Numbers cannot be immediately followed by words *)
      recover env lexbuf ~f:(fun lexbuf ->
          match%sedlex lexbuf with
          | octbigint -> Token (T_BIGINT (BIG_OCTAL, lexeme lexbuf))
          | _ -> failwith "unreachable token octbigint")
  | octbigint -> Token (T_BIGINT (BIG_OCTAL, lexeme lexbuf))
  | octnumber, (letter | '8' .. '9'), Star alphanumeric ->
      (* Numbers cannot be immediately followed by words *)
      recover env lexbuf ~f:(fun lexbuf ->
          match%sedlex lexbuf with
          | octnumber -> Token (T_NUMBER (OCTAL, lexeme lexbuf))
          | _ -> failwith "unreachable token octnumber")
  | octnumber -> Token (T_NUMBER (OCTAL, lexeme lexbuf))
  | legacynonoctnumber, word ->
      (* Numbers cannot be immediately followed by words *)
      recover env lexbuf ~f:(fun lexbuf ->
          match%sedlex lexbuf with
          | legacynonoctnumber -> Token (T_NUMBER (LEGACY_NON_OCTAL, lexeme lexbuf))
          | _ -> failwith "unreachable token legacynonoctnumber")
  | legacynonoctnumber -> Token (T_NUMBER (LEGACY_NON_OCTAL, lexeme lexbuf))
  | legacyoctnumber, (letter | '8' .. '9'), Star alphanumeric ->
      (* Numbers cannot be immediately followed by words *)
      recover env lexbuf ~f:(fun lexbuf ->
          match%sedlex lexbuf with
          | legacyoctnumber -> Token (T_NUMBER (LEGACY_OCTAL, lexeme lexbuf))
          | _ -> failwith "unreachable token legacyoctnumber")
  | legacyoctnumber -> Token (T_NUMBER (LEGACY_OCTAL, lexeme lexbuf))
  | hexbigint, word ->
      (* Numbers cannot be immediately followed by words *)
      recover env lexbuf ~f:(fun lexbuf ->
          match%sedlex lexbuf with
          | hexbigint -> Token (T_BIGINT (BIG_NORMAL, lexeme lexbuf))
          | _ -> failwith "unreachable token hexbigint")
  | hexbigint -> Token (T_BIGINT (BIG_NORMAL, lexeme lexbuf))
  | hexnumber, non_hex_letter, Star alphanumeric ->
      (* Numbers cannot be immediately followed by words *)
      recover env lexbuf ~f:(fun lexbuf ->
          match%sedlex lexbuf with
          | hexnumber -> Token (T_NUMBER (NORMAL, lexeme lexbuf))
          | _ -> failwith "unreachable token hexnumber")
  | hexnumber -> Token (T_NUMBER (NORMAL, lexeme lexbuf))
  | scinumber, word ->
      (* Numbers cannot be immediately followed by words *)
      recover env lexbuf ~f:(fun lexbuf ->
          match%sedlex lexbuf with
          | scinumber -> Token (T_NUMBER (NORMAL, lexeme lexbuf))
          | _ -> failwith "unreachable token scinumber")
  | scinumber -> Token (T_NUMBER (NORMAL, lexeme lexbuf))
  | wholebigint, word ->
      (* Numbers cannot be immediately followed by words *)
      recover env lexbuf ~f:(fun lexbuf ->
          match%sedlex lexbuf with
          | wholebigint -> Token (T_BIGINT (BIG_NORMAL, lexeme lexbuf))
          | _ -> failwith "unreachable token wholebigint")
  | wholebigint -> Token (T_BIGINT (BIG_NORMAL, lexeme lexbuf))
  | integer, word ->
      (* Numbers cannot be immediately followed by words *)
      recover env lexbuf ~f:(fun lexbuf ->
          match%sedlex lexbuf with
          | integer -> Token (T_NUMBER (NORMAL, lexeme lexbuf))
          | _ -> failwith "unreachable token wholenumber")
  | integer, '.', word -> (
      Sedlexing.rollback lexbuf;
      match%sedlex lexbuf with
      | integer -> Token (T_NUMBER (NORMAL, lexeme lexbuf))
      | _ -> failwith "unreachable token wholenumber")
  | floatnumber, word ->
      (* Numbers cannot be immediately followed by words *)
      recover env lexbuf ~f:(fun lexbuf ->
          match%sedlex lexbuf with
          | floatnumber -> Token (T_NUMBER (NORMAL, lexeme lexbuf))
          | _ -> failwith "unreachable token wholenumber")
  | integer, Opt '.' | floatnumber -> Token (T_NUMBER (NORMAL, lexeme lexbuf))
  (* Syntax *)
  | "{" ->
      push_mode env NORMAL;
      Token T_LCURLY
  | "}" ->
      pop_mode env;
      Token T_RCURLY
  | "(" -> Token T_LPAREN
  | ")" -> Token T_RPAREN
  | "[" -> Token T_LBRACKET
  | "]" -> Token T_RBRACKET
  | "..." -> Token T_ELLIPSIS
  | "." -> Token T_PERIOD
  | ";" -> Token T_SEMICOLON
  | "," -> Token T_COMMA
  | ":" -> Token T_COLON
  | "?.", digit -> (
      Sedlexing.rollback lexbuf;
      match%sedlex lexbuf with
      | "?" -> Token T_PLING
      | _ -> failwith "unreachable, expected ?")
  | "?." -> Token T_PLING_PERIOD
  | "??" -> Token T_PLING_PLING
  | "?" -> Token T_PLING
  | "&&" -> Token T_AND
  | "||" -> Token T_OR
  | "===" -> Token T_STRICT_EQUAL
  | "!==" -> Token T_STRICT_NOT_EQUAL
  | "<=" -> Token T_LESS_THAN_EQUAL
  | ">=" -> Token T_GREATER_THAN_EQUAL
  | "==" -> Token T_EQUAL
  | "!=" -> Token T_NOT_EQUAL
  (* restricted productions rules have the following effect:
   * When a ++ or -- token is encountered where the parser would treat it
   * as a postfix operator, and at least one LineTerminator occurred between
   * the preceding token and the ++ or -- token, then a semicolon is automatically
   * inserted before the ++ or -- token. *)
  | "++" -> Token (if env.lex_first_token_on_line then T_INCR else T_INCR_NB)
  | "--" -> Token (if env.lex_first_token_on_line then T_DECR else T_DECR_NB)
  | "<<=" -> Token T_LSHIFT_ASSIGN
  | "<<" -> Token T_LSHIFT
  | ">>=" -> Token T_RSHIFT_ASSIGN
  | ">>>=" -> Token T_RSHIFT3_ASSIGN
  | ">>>" -> Token T_RSHIFT3
  | ">>" -> Token T_RSHIFT
  | "+=" -> Token T_PLUS_ASSIGN
  | "-=" -> Token T_MINUS_ASSIGN
  | "*=" -> Token T_MULT_ASSIGN
  | "**=" -> Token T_EXP_ASSIGN
  | "%=" -> Token T_MOD_ASSIGN
  | "&=" -> Token T_BIT_AND_ASSIGN
  | "|=" -> Token T_BIT_OR_ASSIGN
  | "^=" -> Token T_BIT_XOR_ASSIGN
  | "??=" -> Token T_NULLISH_ASSIGN
  | "&&=" -> Token T_AND_ASSIGN
  | "||=" -> Token T_OR_ASSIGN
  | "<" -> Token T_LESS_THAN
  | ">" -> Token T_GREATER_THAN
  | "+" -> Token T_PLUS
  | "-" -> Token T_MINUS
  | "*" -> Token T_MULT
  | "**" -> Token T_EXP
  | "%" -> Token T_MOD
  | "|" -> Token T_BIT_OR
  | "&" -> Token T_BIT_AND
  | "^" -> Token T_BIT_XOR
  | "!" -> Token T_NOT
  | "~" -> Token T_BIT_NOT
  | "=" -> Token T_ASSIGN
  | "=>" -> Token T_ARROW
  | "/=" -> Token T_DIV_ASSIGN
  | "/" -> Token T_DIV
  | "@" -> Token T_AT
  | "#" -> Token T_POUND
  (* To reason about its correctness:
     1. all tokens are still matched
     2. tokens like opaque, opaquex are matched correctly
       the most fragile case is `opaquex` (matched with `opaque,x` instead)
     3. \a is disallowed
     4. a世界 recognized
  *)
  | js_id_start_with_escape, Star js_id_continue_with_escape -> (
      let raw = lexeme lexbuf in
      match Js_token.is_keyword raw with
      | Some t -> Token t
      | None -> (
          if is_basic_ident raw
          then Token (T_IDENTIFIER (Stdlib.Utf8_string.of_string_exn raw, raw))
          else
            let decoded = decode_identifier env (loc_of_lexbuf env lexbuf) raw in
            match Js_token.is_keyword decoded with
            | None -> (
                match is_valid_identifier_name decoded with
                | true ->
                    Token (T_IDENTIFIER (Stdlib.Utf8_string.of_string_exn decoded, raw))
                | false ->
                    illegal
                      env
                      (loc_of_lexbuf env lexbuf)
                      (Printf.sprintf "%S (%s) is not a valid identifier" raw decoded);
                    Token (T_ERROR raw))
            | Some _ ->
                (* accept keyword as ident if escaped *)
                Token (T_IDENTIFIER (Stdlib.Utf8_string.of_string_exn decoded, raw))))
  | eof -> Token T_EOF
  | any ->
      illegal env (loc_of_lexbuf env lexbuf) "";
      Token (T_ERROR (lexeme lexbuf))
  | _ -> failwith "unreachable token"

let rec regexp_class env buf lexbuf =
  match%sedlex lexbuf with
  | eof -> ()
  | "\\\\" ->
      Buffer.add_string buf "\\\\";
      regexp_class env buf lexbuf
  | '\\', ']' ->
      Buffer.add_char buf '\\';
      Buffer.add_char buf ']';
      regexp_class env buf lexbuf
  | ']' -> Buffer.add_char buf ']'
  | line_terminator_sequence ->
      newline lexbuf;
      lex_error env (loc_of_lexbuf env lexbuf) Parse_error.UnterminatedRegExp
  (* match multi-char substrings that don't contain the start chars of the above patterns *)
  | Plus (Compl (eof | '\\' | ']' | line_terminator_sequence_start)) | any ->
      let str = lexeme lexbuf in
      Buffer.add_string buf str;
      regexp_class env buf lexbuf
  | _ -> failwith "unreachable regexp_class"

let rec regexp_body env buf lexbuf =
  match%sedlex lexbuf with
  | eof ->
      lex_error env (loc_of_lexbuf env lexbuf) Parse_error.UnterminatedRegExp;
      ""
  | '\\', line_terminator_sequence ->
      newline lexbuf;
      lex_error env (loc_of_lexbuf env lexbuf) Parse_error.UnterminatedRegExp;
      ""
  | '\\', any ->
      let s = lexeme lexbuf in
      Buffer.add_string buf s;
      regexp_body env buf lexbuf
  | '/', Plus id_letter ->
      let flags =
        let str = lexeme lexbuf in
        String.sub str 1 (String.length str - 1)
      in
      flags
  | '/' -> ""
  | '[' ->
      Buffer.add_char buf '[';
      regexp_class env buf lexbuf;
      regexp_body env buf lexbuf
  | line_terminator_sequence ->
      newline lexbuf;
      lex_error env (loc_of_lexbuf env lexbuf) Parse_error.UnterminatedRegExp;
      ""
  (* match multi-char substrings that don't contain the start chars of the above patterns *)
  | Plus (Compl (eof | '\\' | '/' | '[' | line_terminator_sequence_start)) | any ->
      let str = lexeme lexbuf in
      Buffer.add_string buf str;
      regexp_body env buf lexbuf
  | _ -> failwith "unreachable regexp_body"

let regexp (env : Lex_env.t) lexbuf =
  match%sedlex lexbuf with
  | eof -> Token T_EOF
  | line_terminator_sequence ->
      newline lexbuf;
      env.lex_first_token_on_line <- true;
      Continue
  | Plus whitespace -> Continue
  | "//" ->
      let buf = Buffer.create 127 in
      lexeme_to_buffer lexbuf buf;
      line_comment env buf lexbuf;
      Comment (Buffer.contents buf)
  | "/*" ->
      let buf = Buffer.create 127 in
      lexeme_to_buffer lexbuf buf;
      comment env buf lexbuf;
      Comment (Buffer.contents buf)
  | '/' ->
      let buf = Buffer.create 127 in
      let flags = regexp_body env buf lexbuf in
      Token (T_REGEXP (Stdlib.Utf8_string.of_string_exn (Buffer.contents buf), flags))
  | any ->
      illegal env (loc_of_lexbuf env lexbuf) "";
      Token (T_ERROR (lexeme lexbuf))
  | _ -> failwith "unreachable regexp"

(*****************************************************************************)
(* Rule backquote *)
(*****************************************************************************)

let backquote env lexbuf =
  match%sedlex lexbuf with
  | '`' ->
      pop_mode env;
      Token T_BACKQUOTE
  | "${" ->
      push_mode env NORMAL;
      Token T_DOLLARCURLY
  | Plus (Compl ('`' | '$' | '\\')) -> Token (T_ENCAPSED_STRING (lexeme lexbuf))
  | '$' -> Token (T_ENCAPSED_STRING (lexeme lexbuf))
  | '\\' ->
      let buf = Buffer.create 127 in
      Buffer.add_char buf '\\';
      let str = string_escape ~accept_invalid:true env lexbuf in
      Buffer.add_string buf str;
      Token (T_ENCAPSED_STRING (Buffer.contents buf))
  | eof -> Token T_EOF
  | _ ->
      illegal env (loc_of_lexbuf env lexbuf) "";
      Token (T_ERROR (lexeme lexbuf))

(* Lex one token, skipping whitespace. The location of the token, and of
   the line records it shares, is only computed for actual tokens. The
   start position is taken before lexing: the sub-lexers (strings,
   comments, ...) reset the lexeme start. *)
let wrap f =
  let rec helper (env : Lex_env.t) =
    let lexbuf = env.lex_lb in
    Sedlexing.start lexbuf;
    let start = Sedlexing.lexing_position_start lexbuf in
    let loc () =
      let stop = Sedlexing.lexing_position_curr lexbuf in
      let loc = Loc.create ~last_line:env.lex_last_line start stop in
      let line_end = Loc.line_end' loc in
      if line_end != env.lex_last_line then env.lex_last_line <- line_end;
      loc
    in
    match f env lexbuf with
    | Continue -> helper env
    | Token t ->
        let loc = loc () in
        env.lex_first_token_on_line <- false;
        t, loc
    | Comment comment ->
        let loc = loc () in
        TComment comment, loc
  in
  helper

let regexp = wrap regexp

let token = wrap token

let backquote = wrap backquote

let lex env =
  match env.Lex_env.lex_mode_stack with
  | Lex_mode.NORMAL :: _ | [] -> token env
  | Lex_mode.BACKQUOTE :: _ -> backquote env
  | Lex_mode.REGEXP :: _ -> regexp env
