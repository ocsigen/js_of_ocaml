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

module Lex_env = struct
  type lex_state = { lex_errors_acc : (Loc.t * Parse_error.t) list } [@@ocaml.unboxed]

  type t =
    { lex_lb : Sedlexing.lexbuf
    ; lex_state : lex_state
    ; lex_mode_stack : Lex_mode.t list
    ; lex_last_loc : Loc.t ref
    }
  [@@ocaml.warning "-69"]

  let empty_lex_state = { lex_errors_acc = [] }

  let create lex_lb =
    { lex_lb
    ; lex_state = empty_lex_state
    ; lex_mode_stack = [ Lex_mode.NORMAL ]
    ; lex_last_loc = ref (Loc.create Lexing.dummy_pos Lexing.dummy_pos)
    }
end

let push_mode env mode =
  { env with Lex_env.lex_mode_stack = mode :: env.Lex_env.lex_mode_stack }

let pop_mode env =
  { env with
    Lex_env.lex_mode_stack =
      (match env.Lex_env.lex_mode_stack with
      | [] -> []
      | _ :: xs -> xs)
  }

module Lex_result = struct
  type t =
    { lex_token : Js_token.t
    ; lex_loc : Loc.t
    ; lex_errors : (Loc.t * Parse_error.t) list
    }
  [@@ocaml.warning "-69"]

  let token result = result.lex_token

  let loc result = result.lex_loc

  let errors result = result.lex_errors
end

let lexeme = Sedlexing.Utf8.lexeme

let lexeme_to_buffer lexbuf b = Buffer.add_string b (Sedlexing.Utf8.lexeme lexbuf)

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

let loc_of_lexbuf _env (lexbuf : Sedlexing.lexbuf) =
  let start_offset, stop_offset = Sedlexing.lexing_positions lexbuf in
  Loc.create start_offset stop_offset

let lex_error (env : Lex_env.t) loc err : Lex_env.t =
  let lex_errors_acc = (loc, err) :: env.lex_state.lex_errors_acc in
  { env with lex_state = { lex_errors_acc } }

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
    let env = lex_error env loc Parse_error.IllegalUnicodeEscape in
    match%sedlex lexbuf with
    | unicode_escape ->
        let code = unicode_escape_code lexbuf in
        if is_low_surrogate code
        then (
          let code = combine_surrogate lead code in
          Buffer.add_utf_8_uchar buf (Uchar.of_int code);
          env)
        else lex_error env loc Parse_error.IllegalUnicodeEscape
    | codepoint_escape ->
        let code = codepoint_escape_code lexbuf in
        if is_low_surrogate code
        then (
          let code = combine_surrogate lead code in
          Buffer.add_utf_8_uchar buf (Uchar.of_int code);
          env)
        else lex_error env loc Parse_error.IllegalUnicodeEscape
    | _ -> lex_error env loc Parse_error.IllegalUnicodeEscape
  in
  let rec id_char env loc buf lexbuf =
    match%sedlex lexbuf with
    | unicode_escape ->
        let code = unicode_escape_code lexbuf in
        let env =
          if is_high_surrogate code
          then low_surrogate env loc buf lexbuf code
          else
            let env =
              if not (Uchar.is_valid code)
              then lex_error env loc Parse_error.IllegalUnicodeEscape
              else env
            in
            Buffer.add_utf_8_uchar buf (Uchar.of_int code);
            env
        in
        id_char env loc buf lexbuf
    | codepoint_escape ->
        let code = codepoint_escape_code lexbuf in
        let env =
          if is_high_surrogate code
          then low_surrogate env loc buf lexbuf code
          else
            let env =
              if not (Uchar.is_valid code)
              then lex_error env loc Parse_error.IllegalUnicodeEscape
              else env
            in
            Buffer.add_utf_8_uchar buf (Uchar.of_int code);
            env
        in
        id_char env loc buf lexbuf
    | eof -> env, Buffer.contents buf
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
  let env = illegal env (loc_of_lexbuf env lexbuf) "recovery" in
  Sedlexing.rollback lexbuf;
  f env lexbuf

type result =
  | Token of Lex_env.t * Js_token.t
  | Comment of Lex_env.t * string
  | Continue of Lex_env.t

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
  | "*/" ->
      lexeme_to_buffer lexbuf buf;
      env
  | "*-/" ->
      Buffer.add_string buf "*-/";
      comment env buf lexbuf
  (* match multi-char substrings that don't contain the start chars of the above patterns *)
  | Plus (Compl (line_terminator_sequence_start | '*')) | any ->
      lexeme_to_buffer lexbuf buf;
      comment env buf lexbuf
  | _ ->
      let env = illegal env (loc_of_lexbuf env lexbuf) "" in
      env

let drop_line env =
  let lexbuf = env.Lex_env.lex_lb in
  match%sedlex lexbuf with
  | Star (Compl (eof | line_terminator_sequence_start)) -> ()
  | _ -> assert false

let rec line_comment env buf lexbuf =
  match%sedlex lexbuf with
  | eof -> env
  | line_terminator_sequence ->
      Sedlexing.rollback lexbuf;
      env
  (* match multi-char substrings that don't contain the start chars of the above patterns *)
  | Plus (Compl (eof | line_terminator_sequence_start)) | any ->
      lexeme_to_buffer lexbuf buf;
      line_comment env buf lexbuf
  | _ -> failwith "unreachable line_comment"

let string_escape ~accept_invalid env lexbuf =
  match%sedlex lexbuf with
  | eof | '\\' ->
      let str = lexeme lexbuf in
      env, str
  | 'x', hex_digit, hex_digit ->
      let str = lexeme lexbuf in
      (* 0xAB *)
      env, str
  | '0' .. '7', '0' .. '7', '0' .. '7' ->
      let str = lexeme lexbuf in
      env, str
  | '0' .. '7', '0' .. '7' ->
      let str = lexeme lexbuf in
      (* 0o01 *)
      env, str
  | '0' -> env, "0"
  | 'b' -> env, "b"
  | 'f' -> env, "f"
  | 'n' -> env, "n"
  | 'r' -> env, "r"
  | 't' -> env, "t"
  | 'v' -> env, "v"
  | '0' .. '7' ->
      let str = lexeme lexbuf in
      (* 0o1 *)
      env, str
  | 'u', hex_quad ->
      let str = lexeme lexbuf in
      env, str
  | "u{", Plus hex_digit, '}' ->
      let str = lexeme lexbuf in
      let hex = String.sub str 2 (String.length str - 3) in
      let code = int_of_string ("0x" ^ hex) in
      (* 11.8.4.1 *)
      let env =
        if code > 0x10FFFF && not accept_invalid
        then illegal env (loc_of_lexbuf env lexbuf) "unicode escape out of range"
        else env
      in
      env, str
  | 'u' | 'x' | '0' .. '7' ->
      let str = lexeme lexbuf in
      let env =
        if accept_invalid then env else illegal env (loc_of_lexbuf env lexbuf) ""
      in
      env, str
  | line_terminator_sequence ->
      newline lexbuf;
      let str = lexeme lexbuf in
      env, str
  | any ->
      let str = lexeme lexbuf in
      env, str
  | _ -> failwith "unreachable string_escape"

(* Really simple version of string lexing. Just try to find beginning and end of
 * string. We can inspect the string later to find invalid escapes, etc *)
let rec string_quote env q buf lexbuf =
  match%sedlex lexbuf with
  | "'" | '"' ->
      let q' = lexeme lexbuf in
      if q = q'
      then env
      else (
        Buffer.add_string buf q';
        string_quote env q buf lexbuf)
  | '\\', line_terminator_sequence ->
      newline lexbuf;
      string_quote env q buf lexbuf
  | '\\' ->
      let env, str = string_escape ~accept_invalid:false env lexbuf in
      (match str with
      | "'" | "\"" -> ()
      | _ -> Buffer.add_string buf "\\");
      Buffer.add_string buf str;
      string_quote env q buf lexbuf
  | '\n' ->
      let x = lexeme lexbuf in
      Buffer.add_string buf x;
      let env = illegal env (loc_of_lexbuf env lexbuf) "" in
      string_quote env q buf lexbuf
  (* env, end_pos_of_lexbuf env lexbuf *)
  | eof ->
      let x = lexeme lexbuf in
      Buffer.add_string buf x;
      let env = illegal env (loc_of_lexbuf env lexbuf) "" in
      env
  (* match multi-char substrings that don't contain the start chars of the above patterns *)
  | Plus (Compl ("'" | '"' | '\\' | '\n' | eof)) | any ->
      lexeme_to_buffer lexbuf buf;
      string_quote env q buf lexbuf
  | _ -> failwith "unreachable string_quote"

let token (env : Lex_env.t) lexbuf : result =
  match%sedlex lexbuf with
  | line_terminator_sequence ->
      newline lexbuf;
      Continue env
  | Plus whitespace -> Continue env
  | "/*" ->
      let buf = Buffer.create 127 in
      lexeme_to_buffer lexbuf buf;
      let env = comment env buf lexbuf in
      Comment (env, Buffer.contents buf)
  | "//" ->
      let buf = Buffer.create 127 in
      lexeme_to_buffer lexbuf buf;
      let env = line_comment env buf lexbuf in
      Comment (env, Buffer.contents buf)
  (* Support for the shebang at the beginning of a file. It is treated like a
   * comment at the beginning or an error elsewhere *)
  | "#!" ->
      if Sedlexing.lexeme_start lexbuf = 0
      then
        let env = line_comment env (Buffer.create 127) lexbuf in
        Continue env
      else Token (env, T_ERROR "#!")
  (* Values *)
  | "'" | '"' ->
      let quote = lexeme lexbuf in
      let p1 = Sedlexing.lexeme_start lexbuf in
      let buf = Buffer.create 127 in
      let env = string_quote env quote buf lexbuf in
      let p2 = Sedlexing.lexeme_end lexbuf in
      Token
        ( env
        , T_STRING (Stdlib.Utf8_string.of_string_exn (Buffer.contents buf), p2 - p1 - 1)
        )
  | '`' ->
      let env = push_mode env BACKQUOTE in
      Token (env, T_BACKQUOTE)
  | binbigint, word ->
      (* Numbers cannot be immediately followed by words *)
      recover env lexbuf ~f:(fun env lexbuf ->
          match%sedlex lexbuf with
          | binbigint -> Token (env, T_BIGINT (BIG_BINARY, lexeme lexbuf))
          | _ -> failwith "unreachable token bigint")
  | binbigint -> Token (env, T_BIGINT (BIG_BINARY, lexeme lexbuf))
  | binnumber, (letter | '2' .. '9'), Star alphanumeric ->
      (* Numbers cannot be immediately followed by words *)
      recover env lexbuf ~f:(fun env lexbuf ->
          match%sedlex lexbuf with
          | binnumber -> Token (env, T_NUMBER (BINARY, lexeme lexbuf))
          | _ -> failwith "unreachable token bignumber")
  | binnumber -> Token (env, T_NUMBER (BINARY, lexeme lexbuf))
  | octbigint, word ->
      (* Numbers cannot be immediately followed by words *)
      recover env lexbuf ~f:(fun env lexbuf ->
          match%sedlex lexbuf with
          | octbigint -> Token (env, T_BIGINT (BIG_OCTAL, lexeme lexbuf))
          | _ -> failwith "unreachable token octbigint")
  | octbigint -> Token (env, T_BIGINT (BIG_OCTAL, lexeme lexbuf))
  | octnumber, (letter | '8' .. '9'), Star alphanumeric ->
      (* Numbers cannot be immediately followed by words *)
      recover env lexbuf ~f:(fun env lexbuf ->
          match%sedlex lexbuf with
          | octnumber -> Token (env, T_NUMBER (OCTAL, lexeme lexbuf))
          | _ -> failwith "unreachable token octnumber")
  | octnumber -> Token (env, T_NUMBER (OCTAL, lexeme lexbuf))
  | legacynonoctnumber, word ->
      (* Numbers cannot be immediately followed by words *)
      recover env lexbuf ~f:(fun env lexbuf ->
          match%sedlex lexbuf with
          | legacynonoctnumber -> Token (env, T_NUMBER (LEGACY_NON_OCTAL, lexeme lexbuf))
          | _ -> failwith "unreachable token legacynonoctnumber")
  | legacynonoctnumber -> Token (env, T_NUMBER (LEGACY_NON_OCTAL, lexeme lexbuf))
  | legacyoctnumber, (letter | '8' .. '9'), Star alphanumeric ->
      (* Numbers cannot be immediately followed by words *)
      recover env lexbuf ~f:(fun env lexbuf ->
          match%sedlex lexbuf with
          | legacyoctnumber -> Token (env, T_NUMBER (LEGACY_OCTAL, lexeme lexbuf))
          | _ -> failwith "unreachable token legacyoctnumber")
  | legacyoctnumber -> Token (env, T_NUMBER (LEGACY_OCTAL, lexeme lexbuf))
  | hexbigint, word ->
      (* Numbers cannot be immediately followed by words *)
      recover env lexbuf ~f:(fun env lexbuf ->
          match%sedlex lexbuf with
          | hexbigint -> Token (env, T_BIGINT (BIG_NORMAL, lexeme lexbuf))
          | _ -> failwith "unreachable token hexbigint")
  | hexbigint -> Token (env, T_BIGINT (BIG_NORMAL, lexeme lexbuf))
  | hexnumber, non_hex_letter, Star alphanumeric ->
      (* Numbers cannot be immediately followed by words *)
      recover env lexbuf ~f:(fun env lexbuf ->
          match%sedlex lexbuf with
          | hexnumber -> Token (env, T_NUMBER (NORMAL, lexeme lexbuf))
          | _ -> failwith "unreachable token hexnumber")
  | hexnumber -> Token (env, T_NUMBER (NORMAL, lexeme lexbuf))
  | scinumber, word ->
      (* Numbers cannot be immediately followed by words *)
      recover env lexbuf ~f:(fun env lexbuf ->
          match%sedlex lexbuf with
          | scinumber -> Token (env, T_NUMBER (NORMAL, lexeme lexbuf))
          | _ -> failwith "unreachable token scinumber")
  | scinumber -> Token (env, T_NUMBER (NORMAL, lexeme lexbuf))
  | wholebigint, word ->
      (* Numbers cannot be immediately followed by words *)
      recover env lexbuf ~f:(fun env lexbuf ->
          match%sedlex lexbuf with
          | wholebigint -> Token (env, T_BIGINT (BIG_NORMAL, lexeme lexbuf))
          | _ -> failwith "unreachable token wholebigint")
  | wholebigint -> Token (env, T_BIGINT (BIG_NORMAL, lexeme lexbuf))
  | integer, word ->
      (* Numbers cannot be immediately followed by words *)
      recover env lexbuf ~f:(fun env lexbuf ->
          match%sedlex lexbuf with
          | integer -> Token (env, T_NUMBER (NORMAL, lexeme lexbuf))
          | _ -> failwith "unreachable token wholenumber")
  | integer, '.', word -> (
      Sedlexing.rollback lexbuf;
      match%sedlex lexbuf with
      | integer -> Token (env, T_NUMBER (NORMAL, lexeme lexbuf))
      | _ -> failwith "unreachable token wholenumber")
  | floatnumber, word ->
      (* Numbers cannot be immediately followed by words *)
      recover env lexbuf ~f:(fun env lexbuf ->
          match%sedlex lexbuf with
          | floatnumber -> Token (env, T_NUMBER (NORMAL, lexeme lexbuf))
          | _ -> failwith "unreachable token wholenumber")
  | integer, Opt '.' | floatnumber -> Token (env, T_NUMBER (NORMAL, lexeme lexbuf))
  (* Syntax *)
  | "{" ->
      let env = push_mode env NORMAL in
      Token (env, T_LCURLY)
  | "}" ->
      let env = pop_mode env in
      Token (env, T_RCURLY)
  | "(" -> Token (env, T_LPAREN)
  | ")" -> Token (env, T_RPAREN)
  | "[" -> Token (env, T_LBRACKET)
  | "]" -> Token (env, T_RBRACKET)
  | "..." -> Token (env, T_ELLIPSIS)
  | "." -> Token (env, T_PERIOD)
  | ";" -> Token (env, T_SEMICOLON)
  | "," -> Token (env, T_COMMA)
  | ":" -> Token (env, T_COLON)
  | "?.", digit -> (
      Sedlexing.rollback lexbuf;
      match%sedlex lexbuf with
      | "?" -> Token (env, T_PLING)
      | _ -> failwith "unreachable, expected ?")
  | "?." -> Token (env, T_PLING_PERIOD)
  | "??" -> Token (env, T_PLING_PLING)
  | "?" -> Token (env, T_PLING)
  | "&&" -> Token (env, T_AND)
  | "||" -> Token (env, T_OR)
  | "===" -> Token (env, T_STRICT_EQUAL)
  | "!==" -> Token (env, T_STRICT_NOT_EQUAL)
  | "<=" -> Token (env, T_LESS_THAN_EQUAL)
  | ">=" -> Token (env, T_GREATER_THAN_EQUAL)
  | "==" -> Token (env, T_EQUAL)
  | "!=" -> Token (env, T_NOT_EQUAL)
  | "++" -> Token (env, T_INCR)
  | "--" -> Token (env, T_DECR)
  | "<<=" -> Token (env, T_LSHIFT_ASSIGN)
  | "<<" -> Token (env, T_LSHIFT)
  | ">>=" -> Token (env, T_RSHIFT_ASSIGN)
  | ">>>=" -> Token (env, T_RSHIFT3_ASSIGN)
  | ">>>" -> Token (env, T_RSHIFT3)
  | ">>" -> Token (env, T_RSHIFT)
  | "+=" -> Token (env, T_PLUS_ASSIGN)
  | "-=" -> Token (env, T_MINUS_ASSIGN)
  | "*=" -> Token (env, T_MULT_ASSIGN)
  | "**=" -> Token (env, T_EXP_ASSIGN)
  | "%=" -> Token (env, T_MOD_ASSIGN)
  | "&=" -> Token (env, T_BIT_AND_ASSIGN)
  | "|=" -> Token (env, T_BIT_OR_ASSIGN)
  | "^=" -> Token (env, T_BIT_XOR_ASSIGN)
  | "??=" -> Token (env, T_NULLISH_ASSIGN)
  | "&&=" -> Token (env, T_AND_ASSIGN)
  | "||=" -> Token (env, T_OR_ASSIGN)
  | "<" -> Token (env, T_LESS_THAN)
  | ">" -> Token (env, T_GREATER_THAN)
  | "+" -> Token (env, T_PLUS)
  | "-" -> Token (env, T_MINUS)
  | "*" -> Token (env, T_MULT)
  | "**" -> Token (env, T_EXP)
  | "%" -> Token (env, T_MOD)
  | "|" -> Token (env, T_BIT_OR)
  | "&" -> Token (env, T_BIT_AND)
  | "^" -> Token (env, T_BIT_XOR)
  | "!" -> Token (env, T_NOT)
  | "~" -> Token (env, T_BIT_NOT)
  | "=" -> Token (env, T_ASSIGN)
  | "=>" -> Token (env, T_ARROW)
  | "/=" -> Token (env, T_DIV_ASSIGN)
  | "/" -> Token (env, T_DIV)
  | "@" -> Token (env, T_AT)
  | "#" -> Token (env, T_POUND)
  (* To reason about its correctness:
     1. all tokens are still matched
     2. tokens like opaque, opaquex are matched correctly
       the most fragile case is `opaquex` (matched with `opaque,x` instead)
     3. \a is disallowed
     4. a世界 recognized
  *)
  | js_id_start_with_escape, Star js_id_continue_with_escape -> (
      let raw = Sedlexing.Utf8.lexeme lexbuf in
      match Js_token.is_keyword raw with
      | Some t -> Token (env, t)
      | None ->
          if is_basic_ident raw
          then Token (env, T_IDENTIFIER (Stdlib.Utf8_string.of_string_exn raw, raw))
          else
            let env, decoded = decode_identifier env (loc_of_lexbuf env lexbuf) raw in
            let env =
              match Js_token.is_keyword decoded with
              | None -> (
                  match is_valid_identifier_name decoded with
                  | true -> env
                  | false ->
                      illegal
                        env
                        (loc_of_lexbuf env lexbuf)
                        (Printf.sprintf "%S is not a valid identifier" decoded))
              | Some _ ->
                  (* accept keyword as ident if escaped *)
                  env
            in
            Token (env, T_IDENTIFIER (Stdlib.Utf8_string.of_string_exn decoded, raw)))
  | eof -> Token (env, T_EOF)
  | any ->
      let env = illegal env (loc_of_lexbuf env lexbuf) "" in
      Token (env, T_ERROR (lexeme lexbuf))
  | _ -> failwith "unreachable token"

let rec regexp_class env buf lexbuf =
  match%sedlex lexbuf with
  | eof -> env
  | "\\\\" ->
      Buffer.add_string buf "\\\\";
      regexp_class env buf lexbuf
  | '\\', ']' ->
      Buffer.add_char buf '\\';
      Buffer.add_char buf ']';
      regexp_class env buf lexbuf
  | ']' ->
      Buffer.add_char buf ']';
      env
  | line_terminator_sequence ->
      newline lexbuf;
      let loc = loc_of_lexbuf env lexbuf in
      let env = lex_error env loc Parse_error.UnterminatedRegExp in
      env
  (* match multi-char substrings that don't contain the start chars of the above patterns *)
  | Plus (Compl (eof | '\\' | ']' | line_terminator_sequence_start)) | any ->
      let str = lexeme lexbuf in
      Buffer.add_string buf str;
      regexp_class env buf lexbuf
  | _ -> failwith "unreachable regexp_class"

let rec regexp_body env buf lexbuf =
  match%sedlex lexbuf with
  | eof ->
      let loc = loc_of_lexbuf env lexbuf in
      let env = lex_error env loc Parse_error.UnterminatedRegExp in
      env, ""
  | '\\', line_terminator_sequence ->
      newline lexbuf;
      let loc = loc_of_lexbuf env lexbuf in
      let env = lex_error env loc Parse_error.UnterminatedRegExp in
      env, ""
  | '\\', any ->
      let s = lexeme lexbuf in
      Buffer.add_string buf s;
      regexp_body env buf lexbuf
  | '/', Plus id_letter ->
      let flags =
        let str = lexeme lexbuf in
        String.sub str 1 (String.length str - 1)
      in
      env, flags
  | '/' -> env, ""
  | '[' ->
      Buffer.add_char buf '[';
      let env = regexp_class env buf lexbuf in
      regexp_body env buf lexbuf
  | line_terminator_sequence ->
      newline lexbuf;
      let loc = loc_of_lexbuf env lexbuf in
      let env = lex_error env loc Parse_error.UnterminatedRegExp in
      env, ""
  (* match multi-char substrings that don't contain the start chars of the above patterns *)
  | Plus (Compl (eof | '\\' | '/' | '[' | line_terminator_sequence_start)) | any ->
      let str = lexeme lexbuf in
      Buffer.add_string buf str;
      regexp_body env buf lexbuf
  | _ -> failwith "unreachable regexp_body"

let regexp env lexbuf =
  match%sedlex lexbuf with
  | eof -> Token (env, T_EOF)
  | line_terminator_sequence ->
      newline lexbuf;
      Continue env
  | Plus whitespace -> Continue env
  | "//" ->
      let buf = Buffer.create 127 in
      lexeme_to_buffer lexbuf buf;
      let env = line_comment env buf lexbuf in
      Comment (env, Buffer.contents buf)
  | "/*" ->
      let buf = Buffer.create 127 in
      lexeme_to_buffer lexbuf buf;
      let env = comment env buf lexbuf in
      Comment (env, Buffer.contents buf)
  | '/' ->
      let buf = Buffer.create 127 in
      let env, flags = regexp_body env buf lexbuf in
      Token (env, T_REGEXP (Stdlib.Utf8_string.of_string_exn (Buffer.contents buf), flags))
  | any ->
      let env = illegal env (loc_of_lexbuf env lexbuf) "" in
      Token (env, T_ERROR (lexeme lexbuf))
  | _ -> failwith "unreachable regexp"

(*****************************************************************************)
(* Rule backquote *)
(*****************************************************************************)

let backquote env lexbuf =
  match%sedlex lexbuf with
  | '`' ->
      let env = pop_mode env in
      Token (env, T_BACKQUOTE)
  | "${" ->
      let env = push_mode env NORMAL in
      Token (env, T_DOLLARCURLY)
  | Plus (Compl ('`' | '$' | '\\')) -> Token (env, T_ENCAPSED_STRING (lexeme lexbuf))
  | '$' -> Token (env, T_ENCAPSED_STRING (lexeme lexbuf))
  | '\\' ->
      let buf = Buffer.create 127 in
      Buffer.add_char buf '\\';
      let env, str = string_escape ~accept_invalid:true env lexbuf in
      Buffer.add_string buf str;
      Token (env, T_ENCAPSED_STRING (Buffer.contents buf))
  | eof -> Token (env, T_EOF)
  | _ ->
      let env = illegal env (loc_of_lexbuf env lexbuf) "" in
      Token (env, T_ERROR (lexeme lexbuf))

let wrap f =
  let f env =
    let start = Sedlexing.lexing_position_start env.Lex_env.lex_lb in
    let t = f env env.Lex_env.lex_lb in
    let stop = Sedlexing.lexing_position_curr env.Lex_env.lex_lb in
    t, Loc.create ~last_line:(Loc.line_end' !(env.lex_last_loc)) start stop
  in
  let rec helper comments env =
    Sedlexing.start env.Lex_env.lex_lb;
    let res, lex_loc = f env in
    match res with
    | Token (env, t) ->
        env.lex_last_loc := lex_loc;
        let lex_token = t in
        let lex_errors_acc = env.lex_state.lex_errors_acc in
        if lex_errors_acc = []
        then env, { Lex_result.lex_token; lex_loc; lex_errors = [] }
        else
          ( { env with lex_state = Lex_env.empty_lex_state }
          , { Lex_result.lex_token; lex_loc; lex_errors = List.rev lex_errors_acc } )
    | Comment (env, comment) ->
        env.lex_last_loc := lex_loc;
        let lex_errors_acc = env.lex_state.lex_errors_acc in
        ( env
        , { Lex_result.lex_token = TComment comment
          ; lex_loc
          ; lex_errors = List.rev lex_errors_acc
          } )
    | Continue env -> helper comments env
  in
  fun env -> helper [] env

let regexp = wrap regexp

let token = wrap token

let backquote = wrap backquote

let lex env =
  match env.Lex_env.lex_mode_stack with
  | Lex_mode.NORMAL :: _ | [] -> token env
  | Lex_mode.BACKQUOTE :: _ -> backquote env
  | Lex_mode.REGEXP :: _ -> regexp env
