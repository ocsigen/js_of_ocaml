(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2013 Hugo Heuzard
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

(* A hand-written recursive-descent parser for JavaScript (ECMAScript 2024
   plus a few proposals: decorators, explicit resource management, deferred
   imports).

   Design notes:

   - The lexer is Flow's lexer ([Flow_lexer]). It has no knowledge of the
     parser state; the parser decides whether a [/] starts a regular
     expression (only ever at the start of a primary expression) and asks
     the lexer to re-lex the token in that case.

   - Every token, including comments, is recorded in an array. This gives
     the parser arbitrary lookahead, provides the token list returned by
     [parse'], and allows the arrow-function cover grammar to be handled by
     re-parsing: a parenthesized expression is first parsed as an
     expression, and if it turns out to be followed by [=>], the parser
     rewinds to the opening parenthesis and parses formal parameters
     instead.

   - Automatic semicolon insertion (ASI) follows ECMA-262 12.10: a
     semicolon is inserted before [}], at end of input, or when the
     offending token is on a new line; restricted productions ([return],
     [throw], [break], [continue], [yield], postfix [++]/[--], [async],
     [using]) check for a line terminator explicitly.

   - The [Yield]/[Await] grammar parameters are two mutable flags. When the
     flag is off, the corresponding keyword token is turned into an
     identifier; when on, an escaped identifier spelling the keyword is
     turned into the keyword. The [In] parameter is threaded as [~no_in]. *)

let debug = Debug.find "js-parser"

let debug () = debug ()

open! Stdlib

module Lexer : sig
  type t

  type error

  val of_file : string -> t

  val of_channel : in_channel -> t

  val of_string :
       ?report_error:(error -> unit)
    -> ?pos:Lexing.position
    -> ?filename:string
    -> string
    -> t

  val print_error : error -> unit

  val token : t -> Js_token.t * Loc.t

  val lex_as_regexp : t -> Js_token.t * Loc.t
end = struct
  type error = Loc.t * Flow_lexer.Parse_error.t

  type t =
    { l : Sedlexing.lexbuf
    ; report_error : error -> unit
    ; mutable env : Flow_lexer.Lex_env.t
    }

  let zero_pos = { Lexing.pos_fname = ""; pos_lnum = 1; pos_cnum = 0; pos_bol = 0 }

  let print_error (loc, e) =
    let pi = Parse_info.t_of_pos (Loc.p1 loc) in
    let msg = "lexer error: " ^ Flow_lexer.Parse_error.to_string e in
    Printf.eprintf "%s\n" (Parse_info.Diagnostic.with_excerpt pi msg)

  let create ?(report_error = print_error) l =
    { l; env = Flow_lexer.Lex_env.create l; report_error }

  let of_file file : t =
    let ic = open_in_bin file in
    let lexbuf = Sedlexing.Utf8.from_channel ic in
    Sedlexing.set_filename lexbuf file;
    create lexbuf

  let of_channel ci : t = create (Sedlexing.Utf8.from_channel ci)

  let of_string ?report_error ?(pos = zero_pos) ?filename s =
    let l = Sedlexing.Utf8.from_string s in
    let pos =
      match filename with
      | None -> pos
      | Some pos_fname -> { pos with pos_fname }
    in
    Sedlexing.set_position l pos;
    Option.iter filename ~f:(Sedlexing.set_filename l);
    create ?report_error l

  let report_errors t res =
    match Flow_lexer.Lex_result.errors res with
    | [] -> ()
    | l -> List.iter l ~f:t.report_error

  let token (t : t) =
    let env, res = Flow_lexer.lex t.env in
    t.env <- env;
    let tok = Flow_lexer.Lex_result.token res in
    let loc = Flow_lexer.Lex_result.loc res in
    report_errors t res;
    tok, loc

  let lex_as_regexp (t : t) =
    Sedlexing.rollback t.l;
    let env, res = Flow_lexer.regexp t.env in
    t.env <- env;
    let tok = Flow_lexer.Lex_result.token res in
    let loc = Flow_lexer.Lex_result.loc res in
    report_errors t res;
    tok, loc
end

exception Parsing_error of Parse_info.t

open Javascript

(****)

let parse_annot s =
  match String.drop_prefix ~prefix:"//" s with
  | None -> None
  | Some s -> (
      let buf = Lexing.from_string s in
      try Some (Annot_parser.annot Annot_lexer.main buf) with
      | Not_found -> None
      | _ -> None)

(* Tokens that are invisible to the parser *)
let is_comment = function
  | Js_token.TComment _ | TAnnot _ | TCommentLineDirective _ | T_VIRTUAL_SEMICOLON -> true
  | _ -> false

let utf8_s = Utf8_string.of_string_exn

let pi pos = Parse_info.t_of_pos pos

let p pos = Pi (pi pos)

let var pos name = ident_unsafe ~loc:(p pos) name

let vartok pos tok = EVar (var pos (utf8_s (Js_token.to_string tok)))

let token_to_ident t =
  let name = Js_token.to_string t in
  Js_token.T_IDENTIFIER (utf8_s name, name)

let dummy_pos = { Lexing.pos_fname = ""; pos_lnum = 0; pos_cnum = 0; pos_bol = 0 }

let dummy_loc = Loc.create dummy_pos dummy_pos

(****)

(* Token stream *)

type t =
  { lexbuf : Lexer.t
  ; mutable toks : (Js_token.t * Loc.t) array
  ; mutable len : int
  ; mutable pos : int
        (* Index of the current (lookahead) token. Comments are skipped by [cur_raw]. *)
  ; mutable prev_end : Lexing.position (* end of the last consumed token *)
  ; mutable prev_line_end : int (* line of the end of the last consumed token *)
  ; mutable prev_real : int (* index of the last consumed token *)
  ; mutable yield : bool (* [yield] is a keyword *)
  ; mutable await : bool (* [await] is a keyword *)
  }

let create lexbuf ~yield ~await =
  { lexbuf
  ; toks = Array.make 64 (Js_token.T_EOF, dummy_loc)
  ; len = 0
  ; pos = 0
  ; prev_end = dummy_pos
  ; prev_line_end = -1
  ; prev_real = -1
  ; yield
  ; await
  }

let push t tok =
  if t.len = Array.length t.toks
  then (
    let toks = Array.make (2 * t.len) (Js_token.T_EOF, dummy_loc) in
    Array.blit ~src:t.toks ~src_pos:0 ~dst:toks ~dst_pos:0 ~len:t.len;
    t.toks <- toks);
  t.toks.(t.len) <- tok;
  t.len <- t.len + 1

let lex_one t =
  let tok, loc = Lexer.token t.lexbuf in
  let tok =
    match tok with
    | Js_token.TComment s -> (
        match parse_annot s with
        | None -> tok
        | Some a -> TAnnot (s, a))
    | _ -> tok
  in
  push t (tok, loc)

let rec cur_raw t =
  if t.pos >= t.len then lex_one t;
  let ((tok, _) as x) = t.toks.(t.pos) in
  if is_comment tok
  then (
    t.pos <- t.pos + 1;
    cur_raw t)
  else x

let normalize t (tok : Js_token.t) =
  match tok with
  | T_IDENTIFIER (_, "yield") when t.yield -> Js_token.T_YIELD
  | T_IDENTIFIER (_, "await") when t.await -> T_AWAIT
  | T_YIELD when not t.yield -> token_to_ident T_YIELD
  | T_AWAIT when not t.await -> token_to_ident T_AWAIT
  | tok -> tok

let cur t = normalize t (fst (cur_raw t))

let cur_loc t = snd (cur_raw t)

let start_pos t = Loc.p1 (cur_loc t)

(* The [n]th token after the current one, and its location *)
let peek t n =
  ignore (cur_raw t);
  let rec find i n =
    if i >= t.len then lex_one t;
    let tok, loc = t.toks.(i) in
    if is_comment tok
    then find (i + 1) n
    else if n = 1
    then normalize t tok, loc
    else find (i + 1) (n - 1)
  in
  find (t.pos + 1) n

let peek_tok t n = fst (peek t n)

let advance t =
  let _, loc = cur_raw t in
  t.prev_end <- Loc.p2 loc;
  t.prev_line_end <- Loc.line_end loc;
  t.prev_real <- t.pos;
  t.pos <- t.pos + 1

(* Whether there is a line terminator between the previous token and the current one *)
let newline_before t = Loc.line (cur_loc t) <> t.prev_line_end

let same_line loc1 loc2 = Loc.line_end loc1 = Loc.line loc2

(* Whether the next token is on the same line as the current one *)
let next_on_same_line t = same_line (cur_loc t) (snd (peek t 1))

let error t = raise (Parsing_error (pi (start_pos t)))

let expect t (tok : Js_token.t) = if Poly.equal (cur t) tok then advance t else error t

(* Record a virtual semicolon in the token stream, right after the last
   consumed token. Only used for the token list returned by [parse']. *)
let insert_virtual_semicolon t =
  let i = t.prev_real + 1 in
  match t.toks.(i) with
  | T_VIRTUAL_SEMICOLON, _ when i < t.len -> () (* already there (re-parse) *)
  | _ ->
      push t (T_EOF, dummy_loc);
      Array.blit ~src:t.toks ~src_pos:i ~dst:t.toks ~dst_pos:(i + 1) ~len:(t.len - 1 - i);
      t.toks.(i) <- Js_token.T_VIRTUAL_SEMICOLON, dummy_loc;
      t.pos <- t.pos + 1

let consume_semicolon t =
  match cur t with
  | T_SEMICOLON -> advance t
  | T_RCURLY | T_EOF -> insert_virtual_semicolon t
  | _ when newline_before t -> insert_virtual_semicolon t
  | _ -> error t

(* Semicolon optional even on the same line: after [do ... while (...)]
   and [export default function/class] *)
let consume_semicolon_opt t =
  match cur t with
  | T_SEMICOLON -> advance t
  | _ -> insert_virtual_semicolon t

let relex_regexp t =
  ignore (cur_raw t);
  assert (t.pos = t.len - 1);
  t.toks.(t.pos) <- Lexer.lex_as_regexp t.lexbuf

type mark =
  { m_pos : int
  ; m_prev_end : Lexing.position
  ; m_prev_line_end : int
  ; m_prev_real : int
  }

let mark t =
  ignore (cur_raw t);
  { m_pos = t.pos
  ; m_prev_end = t.prev_end
  ; m_prev_line_end = t.prev_line_end
  ; m_prev_real = t.prev_real
  }

let reset t m =
  t.pos <- m.m_pos;
  t.prev_end <- m.m_prev_end;
  t.prev_line_end <- m.m_prev_line_end;
  t.prev_real <- m.m_prev_real

let with_context t ~yield ~await f =
  let yield' = t.yield and await' = t.await in
  t.yield <- yield;
  t.await <- await;
  match f () with
  | x ->
      t.yield <- yield';
      t.await <- await';
      x
  | exception e ->
      t.yield <- yield';
      t.await <- await';
      raise e

let all_tokens t = Array.to_list (Array.sub t.toks ~pos:0 ~len:t.len)

(****)

(* Identifiers *)

(* Identifier: identifiers and contextual keywords. [yield] and [await]
   have already been turned into identifiers by [normalize] when they are
   not keywords in the current context. *)
let ident_of_token (tok : Js_token.t) =
  match tok with
  | T_IDENTIFIER (name, _) -> Some name
  | T_ACCESSOR
  | T_AS
  | T_ASYNC
  | T_FROM
  | T_GET
  | T_META
  | T_OF
  | T_SET
  | T_TARGET
  | T_USING
  | T_DEFER -> Some (utf8_s (Js_token.to_string tok))
  | _ -> None

let is_identifier tok = Option.is_some (ident_of_token tok)

(* IdentifierName: identifiers and reserved words *)
let identifier_name_of_token (tok : Js_token.t) =
  match tok with
  | T_IDENTIFIER (name, _) -> Some name
  | T_ACCESSOR
  | T_AS
  | T_ASYNC
  | T_FROM
  | T_GET
  | T_META
  | T_OF
  | T_SET
  | T_TARGET
  | T_USING
  | T_DEFER
  | T_BREAK
  | T_CASE
  | T_CATCH
  | T_CLASS
  | T_CONST
  | T_CONTINUE
  | T_DEBUGGER
  | T_DEFAULT
  | T_DELETE
  | T_DO
  | T_ELSE
  | T_ENUM
  | T_EXPORT
  | T_EXTENDS
  | T_FALSE
  | T_FINALLY
  | T_FOR
  | T_FUNCTION
  | T_IF
  | T_IMPORT
  | T_IN
  | T_INSTANCEOF
  | T_NEW
  | T_NULL
  | T_RETURN
  | T_SUPER
  | T_SWITCH
  | T_THIS
  | T_THROW
  | T_TRUE
  | T_TRY
  | T_TYPEOF
  | T_VAR
  | T_VOID
  | T_WHILE
  | T_WITH
  | T_AWAIT
  | T_YIELD
  | T_LET
  | T_STATIC
  | T_IMPLEMENTS
  | T_INTERFACE
  | T_PACKAGE
  | T_PRIVATE
  | T_PROTECTED
  | T_PUBLIC -> Some (utf8_s (Js_token.to_string tok))
  | _ -> None

let parse_identifier t =
  match ident_of_token (cur t) with
  | Some name ->
      let pos = start_pos t in
      advance t;
      var pos name
  | None -> error t

(* BindingIdentifier also accepts [yield] and [await] *)
let parse_binding_identifier t =
  match cur t with
  | (T_YIELD | T_AWAIT) as tok ->
      let pos = start_pos t in
      advance t;
      var pos (utf8_s (Js_token.to_string tok))
  | _ -> parse_identifier t

let parse_identifier_name t =
  match identifier_name_of_token (cur t) with
  | Some name ->
      advance t;
      name
  | None -> error t

(* Tokens that can start an expression *)
let starts_expression (tok : Js_token.t) =
  match tok with
  | T_IDENTIFIER _
  | T_ACCESSOR
  | T_AS
  | T_ASYNC
  | T_FROM
  | T_GET
  | T_META
  | T_OF
  | T_SET
  | T_TARGET
  | T_USING
  | T_DEFER
  | T_YIELD
  | T_AWAIT
  | T_THIS
  | T_SUPER
  | T_NULL
  | T_TRUE
  | T_FALSE
  | T_NUMBER _
  | T_BIGINT _
  | T_STRING _
  | T_BACKQUOTE
  | T_REGEXP _
  | T_DIV
  | T_DIV_ASSIGN
  | T_LBRACKET
  | T_LCURLY
  | T_LPAREN
  | T_FUNCTION
  | T_CLASS
  | T_AT
  | T_NEW
  | T_IMPORT
  | T_POUND
  | T_PLUS
  | T_MINUS
  | T_NOT
  | T_BIT_NOT
  | T_INCR
  | T_DECR
  | T_INCR_NB
  | T_DECR_NB
  | T_TYPEOF
  | T_VOID
  | T_DELETE -> true
  | _ -> false

let assignment_op (tok : Js_token.t) =
  match tok with
  | T_ASSIGN -> Some Eq
  | T_MULT_ASSIGN -> Some StarEq
  | T_EXP_ASSIGN -> Some ExpEq
  | T_DIV_ASSIGN -> Some SlashEq
  | T_MOD_ASSIGN -> Some ModEq
  | T_PLUS_ASSIGN -> Some PlusEq
  | T_MINUS_ASSIGN -> Some MinusEq
  | T_LSHIFT_ASSIGN -> Some LslEq
  | T_RSHIFT_ASSIGN -> Some AsrEq
  | T_RSHIFT3_ASSIGN -> Some LsrEq
  | T_BIT_AND_ASSIGN -> Some BandEq
  | T_BIT_XOR_ASSIGN -> Some BxorEq
  | T_BIT_OR_ASSIGN -> Some BorEq
  | T_AND_ASSIGN -> Some AndEq
  | T_OR_ASSIGN -> Some OrEq
  | T_NULLISH_ASSIGN -> Some CoalesceEq
  | _ -> None

(* Binary operators, with their precedence. [??] is handled separately. *)
let binary_op ~no_in (tok : Js_token.t) =
  match tok with
  | T_OR -> Some (Or, 1)
  | T_AND -> Some (And, 2)
  | T_BIT_OR -> Some (Bor, 3)
  | T_BIT_XOR -> Some (Bxor, 4)
  | T_BIT_AND -> Some (Band, 5)
  | T_EQUAL -> Some (EqEq, 6)
  | T_NOT_EQUAL -> Some (NotEq, 6)
  | T_STRICT_EQUAL -> Some (EqEqEq, 6)
  | T_STRICT_NOT_EQUAL -> Some (NotEqEq, 6)
  | T_LESS_THAN -> Some (Lt, 7)
  | T_GREATER_THAN -> Some (Gt, 7)
  | T_LESS_THAN_EQUAL -> Some (Le, 7)
  | T_GREATER_THAN_EQUAL -> Some (Ge, 7)
  | T_INSTANCEOF -> Some (InstanceOf, 7)
  | T_IN when not no_in -> Some (In, 7)
  | T_LSHIFT -> Some (Lsl, 8)
  | T_RSHIFT -> Some (Asr, 8)
  | T_RSHIFT3 -> Some (Lsr, 8)
  | T_PLUS -> Some (Plus, 9)
  | T_MINUS -> Some (Minus, 9)
  | T_MULT -> Some (Mul, 10)
  | T_DIV -> Some (Div, 10)
  | T_MOD -> Some (Mod, 10)
  | _ -> None

let no_fun = { async = false; generator = false }

(* After [get], [set], [async], [static] or [accessor] in an object
   literal or a class body: is the keyword itself the property name? *)
let keyword_is_name t =
  match peek_tok t 1 with
  | T_LPAREN | T_COLON | T_COMMA | T_ASSIGN | T_SEMICOLON | T_RCURLY -> true
  | _ -> false

(* [async function] and [async x =>]: no line terminator after [async] *)
let async_function_ahead t =
  match peek t 1 with
  | T_FUNCTION, loc -> same_line (cur_loc t) loc
  | _ -> false

(****)

(* Expressions *)

let rec parse_expression t ~no_in =
  let e = parse_assignment t ~no_in in
  let rec loop e =
    match cur t with
    | T_COMMA ->
        advance t;
        let e2 = parse_assignment t ~no_in in
        loop (ESeq (e, e2))
    | _ -> e
  in
  loop e

and parse_assignment t ~no_in =
  match cur t with
  | T_YIELD -> parse_yield t ~no_in
  | tok when is_identifier tok && Poly.equal (peek_tok t 1) Js_token.T_ARROW ->
      (* x => body *)
      let pos = start_pos t in
      let i = parse_identifier t in
      expect t T_ARROW;
      let body, concise = parse_arrow_body t ~no_in ~async:false in
      EArrow ((no_fun, list [ param' i ], body, p pos), concise, AUnknown)
  | T_ASYNC -> (
      match peek t 1 with
      | T_OF, _ -> parse_assignment_rest t ~no_in
      | tok, loc when is_identifier tok && same_line (cur_loc t) loc ->
          (* async x => body *)
          let pos = start_pos t in
          advance t;
          with_context t ~yield:false ~await:true (fun () ->
              let i = parse_identifier t in
              expect t T_ARROW;
              let body, concise = parse_arrow_body t ~no_in ~async:true in
              EArrow
                ( ({ async = true; generator = false }, list [ param' i ], body, p pos)
                , concise
                , AUnknown ))
      | _ -> parse_assignment_rest t ~no_in)
  | _ -> parse_assignment_rest t ~no_in

and parse_assignment_rest t ~no_in =
  let lhs = parse_conditional t ~no_in in
  match assignment_op (cur t) with
  | Some op ->
      advance t;
      let rhs = parse_assignment t ~no_in in
      EBin (op, assignment_target_of_expr (Some op) lhs, rhs)
  | None -> lhs

and parse_arrow_body t ~no_in ~async =
  with_context t ~yield:false ~await:async (fun () ->
      match cur t with
      | T_LCURLY ->
          advance t;
          let body = parse_function_body t in
          expect t T_RCURLY;
          body, false
      | _ ->
          let pos = start_pos t in
          let e = parse_assignment t ~no_in in
          let stop = t.prev_end in
          [ Return_statement (Some e, p stop), p pos ], true)

and parse_yield t ~no_in =
  advance t;
  if newline_before t
  then EYield { delegate = false; expr = None }
  else
    match cur t with
    | T_MULT ->
        advance t;
        let e = parse_assignment t ~no_in in
        EYield { delegate = true; expr = Some e }
    | tok when starts_expression tok ->
        let e = parse_assignment t ~no_in in
        EYield { delegate = false; expr = Some e }
    | _ -> EYield { delegate = false; expr = None }

and parse_conditional t ~no_in =
  let c = parse_short_circuit t ~no_in in
  match cur t with
  | T_PLING ->
      advance t;
      let e1 = parse_assignment t ~no_in:false in
      expect t T_COLON;
      let e2 = parse_assignment t ~no_in in
      ECond (c, e1, e2)
  | _ -> c

(* ShortCircuitExpression: a [??] chain or a [||]/[&&] chain, not both *)
and parse_short_circuit t ~no_in =
  let e = parse_binary t ~no_in ~min_prec:3 in
  match cur t with
  | T_PLING_PLING ->
      let rec loop e =
        match cur t with
        | T_PLING_PLING ->
            advance t;
            let e2 = parse_binary t ~no_in ~min_prec:3 in
            loop (EBin (Coalesce, e, e2))
        | T_OR | T_AND -> error t
        | _ -> e
      in
      loop e
  | T_OR | T_AND -> (
      let e = parse_binary_rest t ~no_in ~min_prec:1 e in
      match cur t with
      | T_PLING_PLING -> error t
      | _ -> e)
  | _ -> e

and parse_binary t ~no_in ~min_prec =
  let e = parse_exponentiation t in
  parse_binary_rest t ~no_in ~min_prec e

and parse_binary_rest t ~no_in ~min_prec e =
  match binary_op ~no_in (cur t) with
  | Some (op, prec) when prec >= min_prec ->
      advance t;
      let e2 = parse_binary t ~no_in ~min_prec:(prec + 1) in
      parse_binary_rest t ~no_in ~min_prec (EBin (op, e, e2))
  | _ -> e

and parse_exponentiation t =
  let e, is_unary = parse_unary t in
  match cur t with
  | T_EXP ->
      (* [-a ** b] is a syntax error *)
      if is_unary then error t;
      advance t;
      let e2 = parse_exponentiation t in
      EBin (Exp, e, e2)
  | _ -> e

(* Also returns whether the expression is a unary operator application
   (which cannot be the base of [**]) *)
and parse_unary t =
  let unop op =
    advance t;
    let e, _ = parse_unary t in
    EUn (op, e), true
  in
  match cur t with
  | T_DELETE -> unop Delete
  | T_VOID -> unop Void
  | T_TYPEOF -> unop Typeof
  | T_PLUS -> unop Pl
  | T_MINUS -> unop Neg
  | T_BIT_NOT -> unop Bnot
  | T_NOT -> unop Not
  | T_AWAIT -> unop Await
  | T_INCR | T_INCR_NB ->
      advance t;
      let e, _ = parse_unary t in
      EUn (IncrB, e), false
  | T_DECR | T_DECR_NB ->
      advance t;
      let e, _ = parse_unary t in
      EUn (DecrB, e), false
  | _ -> (
      let e = parse_lhs t in
      (* Postfix operators: the lexer produces [T_INCR_NB] when there is no
         line terminator before [++] *)
      match cur t with
      | T_INCR_NB ->
          advance t;
          EUn (IncrA, e), false
      | T_DECR_NB ->
          advance t;
          EUn (DecrA, e), false
      | _ -> e, false)

(* LeftHandSideExpression *)
and parse_lhs t =
  let start = start_pos t in
  match cur t with
  | T_ASYNC when Poly.equal (peek_tok t 1) Js_token.T_LPAREN && next_on_same_line t -> (
      (* [async (...)] is either a call or an async arrow function head. *)
      let async = parse_identifier t in
      let m = mark t in
      let args = parse_arguments t in
      match cur t with
      | T_ARROW when not (newline_before t) ->
          reset t m;
          with_context t ~yield:false ~await:true (fun () ->
              expect t T_LPAREN;
              let params = parse_formal_parameters t in
              expect t T_RPAREN;
              expect t T_ARROW;
              let body, concise = parse_arrow_body t ~no_in:false ~async:true in
              EArrow
                ( ({ async = true; generator = false }, params, body, p start)
                , concise
                , AUnknown ))
      | _ ->
          parse_suffixes
            t
            ~start
            ~allow_call:true
            (ECall (EVar async, ANormal, args, p start)))
  | _ ->
      let e = parse_primary t in
      parse_suffixes t ~start ~allow_call:true e

(* Member accesses, calls, optional chains and tagged templates *)
and parse_suffixes t ~start ~allow_call e =
  (* Location of calls: the start of the expression or, inside an optional
     chain, the position of the last [?.] *)
  let loc_start = ref start in
  let rec loop e =
    match cur t with
    | T_PERIOD -> (
        advance t;
        match cur t with
        | T_POUND ->
            advance t;
            let n = parse_identifier_name t in
            loop (EDotPrivate (e, ANormal, n))
        | _ ->
            let n = parse_identifier_name t in
            loop (EDot (e, ANormal, n)))
    | T_LBRACKET ->
        advance t;
        let i = parse_expression t ~no_in:false in
        expect t T_RBRACKET;
        loop (EAccess (e, ANormal, i))
    | T_BACKQUOTE ->
        let tpl = parse_template t in
        loop (ECallTemplate (e, tpl, p !loc_start))
    | T_LPAREN when allow_call ->
        let args = parse_arguments t in
        loop (ECall (e, ANormal, args, p !loc_start))
    | T_PLING_PERIOD when allow_call -> (
        loc_start := start_pos t;
        advance t;
        match cur t with
        | T_LPAREN ->
            let args = parse_arguments t in
            loop (ECall (e, ANullish, args, p !loc_start))
        | T_LBRACKET ->
            advance t;
            let i = parse_expression t ~no_in:false in
            expect t T_RBRACKET;
            loop (EAccess (e, ANullish, i))
        | T_POUND ->
            advance t;
            let n = parse_identifier_name t in
            loop (EDotPrivate (e, ANullish, n))
        | T_BACKQUOTE -> error t
        | _ ->
            let n = parse_identifier_name t in
            loop (EDot (e, ANullish, n)))
    | _ -> e
  in
  loop e

and parse_new t =
  let pos = start_pos t in
  expect t T_NEW;
  match cur t with
  | T_PERIOD ->
      advance t;
      expect t T_TARGET;
      EDot (vartok pos T_NEW, ANormal, utf8_s "target")
  | _ -> (
      let callee_pos = start_pos t in
      let callee =
        match cur t with
        | T_NEW -> parse_new t
        | _ -> parse_primary t
      in
      let callee = parse_suffixes t ~start:callee_pos ~allow_call:false callee in
      match cur t with
      | T_LPAREN ->
          let args = parse_arguments t in
          ENew (callee, Some args, p pos)
      | _ -> ENew (callee, None, p pos))

and parse_primary t =
  let pos = start_pos t in
  match cur t with
  | (T_THIS | T_NULL | T_SUPER) as tok ->
      advance t;
      vartok pos tok
  | T_IMPORT -> (
      advance t;
      match cur t with
      | T_PERIOD | T_LPAREN -> vartok pos T_IMPORT
      | _ -> error t)
  | T_TRUE ->
      advance t;
      EBool true
  | T_FALSE ->
      advance t;
      EBool false
  | T_NUMBER (_, raw) | T_BIGINT (_, raw) ->
      advance t;
      ENum (Num.of_string_unsafe raw)
  | T_STRING (s, _) ->
      advance t;
      EStr s
  | T_BACKQUOTE -> ETemplate (parse_template t)
  | T_DIV | T_DIV_ASSIGN ->
      relex_regexp t;
      parse_primary t
  | T_REGEXP (Utf8 s, flags) ->
      advance t;
      ERegexp (s, if String.equal flags "" then None else Some flags)
  | T_LBRACKET -> parse_array_literal t
  | T_LCURLY -> parse_object_literal t
  | T_LPAREN -> parse_parenthesized t
  | T_FUNCTION -> parse_function_expression t ~pos ~async:false
  | T_ASYNC when async_function_ahead t ->
      advance t;
      parse_function_expression t ~pos ~async:true
  | T_CLASS | T_AT ->
      let decorators = parse_decorators t in
      let name, decl = parse_class t ~decorators ~name:`Optional in
      EClass (name, decl)
  | T_POUND ->
      advance t;
      let n = parse_identifier_name t in
      EPrivName n
  | T_NEW -> parse_new t
  | tok when is_identifier tok -> EVar (parse_identifier t)
  | _ -> error t

(* CoverParenthesizedExpressionAndArrowParameterList *)
and parse_parenthesized t =
  let pos = start_pos t in
  let m = mark t in
  expect t T_LPAREN;
  let cover_rest () =
    (* [...] binding, only valid as arrow parameters *)
    let ellipsis_pos = start_pos t in
    expect t T_ELLIPSIS;
    ignore (parse_binding_element t);
    expect t T_RPAREN;
    `Cover (early_error (pi ellipsis_pos))
  in
  let res =
    match cur t with
    | T_RPAREN ->
        let rparen_pos = start_pos t in
        advance t;
        `Cover (early_error (pi rparen_pos))
    | T_ELLIPSIS -> cover_rest ()
    | _ ->
        let rec loop e =
          match cur t with
          | T_COMMA -> (
              advance t;
              match cur t with
              | T_RPAREN ->
                  advance t;
                  `Expr e
              | T_ELLIPSIS -> cover_rest ()
              | _ ->
                  let e2 = parse_assignment t ~no_in:false in
                  loop (ESeq (e, e2)))
          | T_RPAREN ->
              advance t;
              `Expr e
          | _ -> error t
        in
        loop (parse_assignment t ~no_in:false)
  in
  match cur t with
  | T_ARROW when not (newline_before t) ->
      (* Arrow function: re-parse the parenthesized tokens as parameters *)
      reset t m;
      expect t T_LPAREN;
      let params = parse_formal_parameters t in
      expect t T_RPAREN;
      expect t T_ARROW;
      let body, concise = parse_arrow_body t ~no_in:false ~async:false in
      EArrow ((no_fun, params, body, p pos), concise, AUnknown)
  | _ -> (
      match res with
      | `Expr e -> e
      | `Cover e -> CoverParenthesizedExpressionAndArrowParameterList e)

and parse_arguments t =
  expect t T_LPAREN;
  let rec loop acc =
    match cur t with
    | T_RPAREN ->
        advance t;
        List.rev acc
    | _ -> (
        let arg =
          match cur t with
          | T_ELLIPSIS ->
              advance t;
              ArgSpread (parse_assignment t ~no_in:false)
          | _ -> Arg (parse_assignment t ~no_in:false)
        in
        match cur t with
        | T_COMMA ->
            advance t;
            loop (arg :: acc)
        | T_RPAREN ->
            advance t;
            List.rev (arg :: acc)
        | _ -> error t)
  in
  loop []

and parse_template t =
  expect t T_BACKQUOTE;
  let rec loop acc =
    match cur t with
    | T_ENCAPSED_STRING s ->
        advance t;
        loop (TStr (utf8_s s) :: acc)
    | T_DOLLARCURLY ->
        advance t;
        let e = parse_expression t ~no_in:false in
        expect t T_RCURLY;
        loop (TExp e :: acc)
    | T_BACKQUOTE ->
        advance t;
        List.rev acc
    | _ -> error t
  in
  loop []

and parse_array_literal t =
  expect t T_LBRACKET;
  let rec loop acc =
    match cur t with
    | T_RBRACKET ->
        advance t;
        List.rev acc
    | T_COMMA ->
        advance t;
        loop (ElementHole :: acc)
    | _ -> (
        let e =
          match cur t with
          | T_ELLIPSIS ->
              advance t;
              ElementSpread (parse_assignment t ~no_in:false)
          | _ -> Element (parse_assignment t ~no_in:false)
        in
        match cur t with
        | T_COMMA ->
            advance t;
            loop (e :: acc)
        | T_RBRACKET ->
            advance t;
            List.rev (e :: acc)
        | _ -> error t)
  in
  EArr (loop [])

and parse_property_name t =
  match cur t with
  | T_STRING (s, _) ->
      advance t;
      PNS s
  | T_NUMBER (_, raw) | T_BIGINT (_, raw) ->
      advance t;
      PNN (Num.of_string_unsafe raw)
  | T_LBRACKET ->
      advance t;
      let e = parse_assignment t ~no_in:false in
      expect t T_RBRACKET;
      PComputed e
  | _ -> PNI (parse_identifier_name t)

and parse_object_literal t =
  expect t T_LCURLY;
  let rec loop acc =
    match cur t with
    | T_RCURLY ->
        advance t;
        List.rev acc
    | _ -> (
        let prop = parse_property_definition t in
        match cur t with
        | T_COMMA ->
            advance t;
            loop (prop :: acc)
        | T_RCURLY ->
            advance t;
            List.rev (prop :: acc)
        | _ -> error t)
  in
  EObj (loop [])

and parse_property_definition t =
  let pos = start_pos t in
  match cur t with
  | T_ELLIPSIS ->
      advance t;
      PropertySpread (parse_assignment t ~no_in:false)
  | T_GET when not (keyword_is_name t) ->
      advance t;
      let name = parse_property_name t in
      PropertyMethod
        (name, MethodGet (parse_method_rest t ~pos ~async:false ~generator:false))
  | T_SET when not (keyword_is_name t) ->
      advance t;
      let name = parse_property_name t in
      PropertyMethod
        (name, MethodSet (parse_method_rest t ~pos ~async:false ~generator:false))
  | T_ASYNC when (not (keyword_is_name t)) && next_on_same_line t ->
      advance t;
      let generator =
        match cur t with
        | T_MULT ->
            advance t;
            true
        | _ -> false
      in
      let name = parse_property_name t in
      PropertyMethod (name, Method (parse_method_rest t ~pos ~async:true ~generator))
  | T_MULT ->
      advance t;
      let name = parse_property_name t in
      PropertyMethod (name, Method (parse_method_rest t ~pos ~async:false ~generator:true))
  | tok -> (
      let ident = ident_of_token tok in
      let name = parse_property_name t in
      match cur t, ident with
      | T_COLON, _ ->
          advance t;
          Property (name, parse_assignment t ~no_in:false)
      | T_LPAREN, _ ->
          PropertyMethod
            (name, Method (parse_method_rest t ~pos ~async:false ~generator:false))
      | (T_COMMA | T_RCURLY), Some i ->
          (* shorthand property *)
          Property (PNI i, EVar (ident_unsafe i))
      | T_ASSIGN, Some i ->
          let eq_pos = start_pos t in
          advance t;
          let e = parse_assignment t ~no_in:false in
          CoverInitializedName (early_error (pi eq_pos), var pos i, (e, p eq_pos))
      | _ -> error t)

(* Parameters and body of a method, after its name *)
and parse_method_rest t ~pos ~async ~generator =
  with_context t ~yield:generator ~await:async (fun () ->
      expect t T_LPAREN;
      let params = parse_formal_parameters t in
      expect t T_RPAREN;
      expect t T_LCURLY;
      let body = parse_function_body t in
      expect t T_RCURLY;
      { async; generator }, params, body, p pos)

(****)

(* Functions *)

and parse_function_expression t ~pos ~async =
  expect t T_FUNCTION;
  let generator =
    match cur t with
    | T_MULT ->
        advance t;
        true
    | _ -> false
  in
  with_context t ~yield:generator ~await:async (fun () ->
      let name =
        match cur t with
        | T_LPAREN -> None
        | _ -> Some (parse_identifier t)
      in
      expect t T_LPAREN;
      let params = parse_formal_parameters t in
      expect t T_RPAREN;
      expect t T_LCURLY;
      let body = parse_function_body t in
      expect t T_RCURLY;
      EFun (name, ({ async; generator }, params, body, p pos)))

and parse_function_declaration t ~pos ~async =
  expect t T_FUNCTION;
  let generator =
    match cur t with
    | T_MULT ->
        advance t;
        true
    | _ -> false
  in
  let name = parse_identifier t in
  with_context t ~yield:generator ~await:async (fun () ->
      expect t T_LPAREN;
      let params = parse_formal_parameters t in
      expect t T_RPAREN;
      expect t T_LCURLY;
      let body = parse_function_body t in
      (* For compatibility with the previous parser, plain function
         declarations are located at their closing brace. *)
      let pos = if async || generator then pos else start_pos t in
      expect t T_RCURLY;
      name, ({ async; generator }, params, body, p pos))

and parse_function_body t =
  let rec loop acc =
    match cur t with
    | T_RCURLY | T_EOF -> List.rev acc
    | _ ->
        let s = parse_statement_list_item t ~module_:false in
        loop (s :: acc)
  in
  loop []

(* Parses up to, but not including, the closing parenthesis *)
and parse_formal_parameters t =
  let rec loop acc =
    match cur t with
    | T_RPAREN -> { list = List.rev acc; rest = None }
    | T_ELLIPSIS ->
        advance t;
        let rest = parse_single_name_binding t in
        { list = List.rev acc; rest = Some rest }
    | _ -> (
        let param = parse_binding_element t in
        match cur t with
        | T_COMMA ->
            advance t;
            loop (param :: acc)
        | T_RPAREN -> { list = List.rev (param :: acc); rest = None }
        | _ -> error t)
  in
  loop []

and parse_binding_element t =
  let b = parse_single_name_binding t in
  let init = parse_initializer_opt t ~no_in:false in
  b, init

and parse_single_name_binding t =
  match cur t with
  | T_LBRACKET | T_LCURLY -> BindingPattern (parse_binding_pattern t)
  | _ -> BindingIdent (parse_identifier t)

and parse_initializer_opt t ~no_in =
  match cur t with
  | T_ASSIGN -> Some (parse_initializer t ~no_in)
  | _ -> None

and parse_initializer t ~no_in =
  let pos = start_pos t in
  expect t T_ASSIGN;
  let e = parse_assignment t ~no_in in
  e, p pos

and parse_binding_pattern t =
  match cur t with
  | T_LCURLY -> parse_object_binding_pattern t
  | T_LBRACKET -> parse_array_binding_pattern t
  | _ -> error t

and parse_object_binding_pattern t =
  expect t T_LCURLY;
  let rec loop acc =
    match cur t with
    | T_RCURLY ->
        advance t;
        { list = List.rev acc; rest = None }
    | T_ELLIPSIS ->
        advance t;
        let rest = parse_identifier t in
        expect t T_RCURLY;
        { list = List.rev acc; rest = Some rest }
    | _ -> (
        let prop =
          match cur t with
          | tok when is_identifier tok && not (Poly.equal (peek_tok t 1) Js_token.T_COLON)
            ->
              let i = parse_identifier t in
              let init = parse_initializer_opt t ~no_in:false in
              Prop_ident (Prop_and_ident i, init)
          | _ ->
              let name = parse_property_name t in
              expect t T_COLON;
              let e = parse_binding_element t in
              Prop_binding (name, e)
        in
        match cur t with
        | T_COMMA ->
            advance t;
            loop (prop :: acc)
        | T_RCURLY ->
            advance t;
            { list = List.rev (prop :: acc); rest = None }
        | _ -> error t)
  in
  ObjectBinding (loop [])

and parse_array_binding_pattern t =
  expect t T_LBRACKET;
  let rec loop acc =
    match cur t with
    | T_RBRACKET ->
        advance t;
        { list = List.rev acc; rest = None }
    | T_COMMA ->
        advance t;
        loop (None :: acc)
    | T_ELLIPSIS ->
        advance t;
        let rest = parse_single_name_binding t in
        expect t T_RBRACKET;
        { list = List.rev acc; rest = Some rest }
    | _ -> (
        let e = parse_binding_element t in
        match cur t with
        | T_COMMA ->
            advance t;
            loop (Some e :: acc)
        | T_RBRACKET ->
            advance t;
            { list = List.rev (Some e :: acc); rest = None }
        | _ -> error t)
  in
  ArrayBinding (loop [])

(****)

(* Classes *)

and parse_decorators t =
  let rec loop acc =
    match cur t with
    | T_AT ->
        let pos = start_pos t in
        advance t;
        let d =
          match cur t with
          | T_LPAREN ->
              advance t;
              let e = parse_expression t ~no_in:false in
              expect t T_RPAREN;
              e
          | _ -> (
              let rec member e =
                match cur t with
                | T_PERIOD -> (
                    advance t;
                    match cur t with
                    | T_POUND ->
                        advance t;
                        let n = parse_identifier_name t in
                        member (EDotPrivate (e, ANormal, n))
                    | _ ->
                        let n = parse_identifier_name t in
                        member (EDot (e, ANormal, n)))
                | _ -> e
              in
              let e = member (EVar (parse_identifier t)) in
              match cur t with
              | T_LPAREN ->
                  let args = parse_arguments t in
                  ECall (e, ANormal, args, p pos)
              | _ -> e)
        in
        loop (d :: acc)
    | _ -> List.rev acc
  in
  loop []

and parse_class t ~decorators ~name =
  expect t T_CLASS;
  let name =
    match name, cur t with
    | `Optional, (T_EXTENDS | T_LCURLY) -> None
    | _ -> Some (parse_binding_identifier t)
  in
  let extends =
    match cur t with
    | T_EXTENDS ->
        advance t;
        Some (parse_lhs t)
    | _ -> None
  in
  expect t T_LCURLY;
  let body = parse_class_body t in
  expect t T_RCURLY;
  name, { decorators; extends; body }

and parse_class_element_name t =
  match cur t with
  | T_POUND ->
      advance t;
      PrivName (parse_identifier_name t)
  | _ -> PropName (parse_property_name t)

and parse_class_body t =
  let rec loop acc =
    match cur t with
    | T_RCURLY -> List.rev acc
    | T_SEMICOLON ->
        advance t;
        loop acc
    | T_STATIC when Poly.equal (peek_tok t 1) Js_token.T_LCURLY ->
        advance t;
        advance t;
        let body =
          with_context t ~yield:false ~await:true (fun () -> parse_function_body t)
        in
        expect t T_RCURLY;
        loop (CEStaticBLock body :: acc)
    | _ ->
        let decorators = parse_decorators t in
        let static =
          match cur t with
          | T_STATIC when not (keyword_is_name t) ->
              advance t;
              true
          | _ -> false
        in
        let elt =
          match cur t with
          | T_ACCESSOR when not (keyword_is_name t) ->
              advance t;
              let name = parse_class_element_name t in
              let init = parse_initializer_opt t ~no_in:false in
              consume_semicolon t;
              CEAccessor (decorators, static, name, init)
          | _ -> (
              let pos = start_pos t in
              let meth ~async ~generator kind =
                let name = parse_class_element_name t in
                let m = parse_method_rest t ~pos ~async ~generator in
                CEMethod (decorators, static, name, kind m)
              in
              match cur t with
              | T_GET when not (keyword_is_name t) ->
                  advance t;
                  meth ~async:false ~generator:false (fun m -> MethodGet m)
              | T_SET when not (keyword_is_name t) ->
                  advance t;
                  meth ~async:false ~generator:false (fun m -> MethodSet m)
              | T_ASYNC when (not (keyword_is_name t)) && next_on_same_line t ->
                  (* [async] on its own line is a field named [async] *)
                  advance t;
                  let generator =
                    match cur t with
                    | T_MULT ->
                        advance t;
                        true
                    | _ -> false
                  in
                  meth ~async:true ~generator (fun m -> Method m)
              | T_MULT ->
                  advance t;
                  meth ~async:false ~generator:true (fun m -> Method m)
              | _ -> (
                  let name = parse_class_element_name t in
                  match cur t with
                  | T_LPAREN ->
                      let m = parse_method_rest t ~pos ~async:false ~generator:false in
                      CEMethod (decorators, static, name, Method m)
                  | _ ->
                      let init = parse_initializer_opt t ~no_in:false in
                      consume_semicolon t;
                      CEField (decorators, static, name, init)))
        in
        loop (elt :: acc)
  in
  loop []

(****)

(* Statements *)

and parse_block t =
  expect t T_LCURLY;
  let body = parse_function_body t in
  expect t T_RCURLY;
  body

(* A statement in a position where declarations are not allowed
   (e.g. the body of an [if]) *)
and parse_statement t =
  match cur t with
  | T_FUNCTION | T_CLASS | T_LET | T_CONST | T_AT -> error t
  | _ -> parse_statement_list_item t ~module_:false

and parse_variable_declaration_list t ~no_in =
  let rec loop acc =
    let d =
      match cur t with
      | T_LBRACKET | T_LCURLY ->
          let pat = parse_binding_pattern t in
          let init = parse_initializer t ~no_in in
          DeclPattern (pat, init)
      | _ ->
          let i = parse_identifier t in
          let init = parse_initializer_opt t ~no_in in
          DeclIdent (i, init)
    in
    match cur t with
    | T_COMMA ->
        advance t;
        loop (d :: acc)
    | _ -> List.rev (d :: acc)
  in
  loop []

(* [using] / [await using] declarations only bind identifiers (never
   [of]), and only when the first binding is on the same line as [using]. *)
and using_declaration_ahead t ~await =
  let n = if await then 1 else 0 in
  (match cur t with
    | T_AWAIT -> await && Poly.equal (peek_tok t 1) Js_token.T_USING
    | T_USING -> not await
    | _ -> false)
  &&
  let using_loc = if await then snd (peek t 1) else cur_loc t in
  match peek t (n + 1) with
  | T_OF, _ -> false
  | tok, loc -> is_identifier tok && same_line using_loc loc

and parse_using_declaration t ~no_in =
  let kind =
    match cur t with
    | T_AWAIT ->
        advance t;
        expect t T_USING;
        AwaitUsing
    | _ ->
        expect t T_USING;
        Using
  in
  let rec loop acc =
    let i = parse_identifier t in
    let init = parse_initializer_opt t ~no_in in
    let d = DeclIdent (i, init) in
    match cur t with
    | T_COMMA ->
        advance t;
        loop (d :: acc)
    | _ -> kind, List.rev (d :: acc)
  in
  loop []

and parse_statement_list_item t ~module_ =
  let pos = start_pos t in
  let stmt s = s, p pos in
  match cur t with
  | T_LCURLY -> stmt (Block (parse_block t))
  | T_VAR ->
      advance t;
      let l = parse_variable_declaration_list t ~no_in:false in
      consume_semicolon t;
      stmt (Variable_statement (Var, l))
  | (T_LET | T_CONST) as tok ->
      advance t;
      let l = parse_variable_declaration_list t ~no_in:false in
      consume_semicolon t;
      stmt
        (Variable_statement
           ( (match tok with
             | T_LET -> Let
             | _ -> Const)
           , l ))
  | T_USING when using_declaration_ahead t ~await:false ->
      let kind, l = parse_using_declaration t ~no_in:false in
      consume_semicolon t;
      stmt (Variable_statement (kind, l))
  | T_AWAIT when using_declaration_ahead t ~await:true ->
      let kind, l = parse_using_declaration t ~no_in:false in
      consume_semicolon t;
      stmt (Variable_statement (kind, l))
  | T_SEMICOLON ->
      advance t;
      stmt Empty_statement
  | T_IF ->
      advance t;
      expect t T_LPAREN;
      let c = parse_expression t ~no_in:false in
      expect t T_RPAREN;
      let s1 = parse_statement t in
      let s2 =
        match cur t with
        | T_ELSE ->
            advance t;
            Some (parse_statement t)
        | _ -> None
      in
      stmt (If_statement (c, s1, s2))
  | T_DO ->
      advance t;
      let body = parse_statement t in
      expect t T_WHILE;
      expect t T_LPAREN;
      let c = parse_expression t ~no_in:false in
      expect t T_RPAREN;
      consume_semicolon_opt t;
      stmt (Do_while_statement (body, c))
  | T_WHILE ->
      advance t;
      expect t T_LPAREN;
      let c = parse_expression t ~no_in:false in
      expect t T_RPAREN;
      let body = parse_statement t in
      stmt (While_statement (c, body))
  | T_FOR -> stmt (parse_for t)
  | T_CONTINUE ->
      advance t;
      let label = parse_label_opt t in
      consume_semicolon t;
      stmt (Continue_statement label)
  | T_BREAK ->
      advance t;
      let label = parse_label_opt t in
      consume_semicolon t;
      stmt (Break_statement label)
  | T_RETURN ->
      advance t;
      let e =
        match cur t with
        | T_SEMICOLON | T_RCURLY | T_EOF -> None
        | _ when newline_before t -> None
        | _ -> Some (parse_expression t ~no_in:false)
      in
      let stop = t.prev_end in
      consume_semicolon t;
      stmt (Return_statement (e, p stop))
  | T_WITH ->
      advance t;
      expect t T_LPAREN;
      let e = parse_expression t ~no_in:false in
      expect t T_RPAREN;
      let body = parse_statement t in
      stmt (With_statement (e, body))
  | T_SWITCH -> stmt (parse_switch t)
  | T_THROW ->
      advance t;
      if newline_before t then error t;
      let e = parse_expression t ~no_in:false in
      consume_semicolon t;
      stmt (Throw_statement e)
  | T_TRY -> stmt (parse_try t)
  | T_DEBUGGER ->
      advance t;
      consume_semicolon t;
      stmt Debugger_statement
  | T_FUNCTION ->
      let name, decl = parse_function_declaration t ~pos ~async:false in
      stmt (Function_declaration (name, decl))
  | T_ASYNC when async_function_ahead t ->
      advance t;
      let name, decl = parse_function_declaration t ~pos ~async:true in
      stmt (Function_declaration (name, decl))
  | T_CLASS ->
      let name, decl = parse_class t ~decorators:[] ~name:`Required in
      stmt (Class_declaration (Option.get name, decl))
  | T_AT -> (
      let decorators = parse_decorators t in
      match cur t with
      | T_EXPORT when module_ -> parse_export t ~pos ~decorators
      | _ ->
          let name, decl = parse_class t ~decorators ~name:`Required in
          stmt (Class_declaration (Option.get name, decl)))
  | T_IMPORT
    when module_
         &&
         match peek_tok t 1 with
         | T_LPAREN | T_PERIOD -> false
         | _ -> true -> parse_import t ~pos
  | T_EXPORT when module_ -> parse_export t ~pos ~decorators:[]
  | tok when is_identifier tok && Poly.equal (peek_tok t 1) Js_token.T_COLON ->
      let label = Label.of_string (Option.get (ident_of_token tok)) in
      advance t;
      advance t;
      let body = parse_statement t in
      stmt (Labelled_statement (label, body))
  | _ ->
      let e = parse_expression t ~no_in:false in
      consume_semicolon t;
      stmt (Expression_statement e)

and parse_label_opt t =
  match cur t with
  | tok when is_identifier tok && not (newline_before t) ->
      let label = Label.of_string (Option.get (ident_of_token tok)) in
      advance t;
      Some label
  | _ -> None

and parse_for t =
  expect t T_FOR;
  let await =
    match cur t with
    | T_AWAIT ->
        advance t;
        true
    | _ -> false
  in
  expect t T_LPAREN;
  let for_in_of left =
    match cur t with
    | T_IN when not await ->
        advance t;
        let e = parse_expression t ~no_in:false in
        expect t T_RPAREN;
        let body = parse_statement t in
        ForIn_statement (left, e, body)
    | T_OF ->
        advance t;
        let e = parse_assignment t ~no_in:false in
        expect t T_RPAREN;
        let body = parse_statement t in
        if await
        then ForAwaitOf_statement (left, e, body)
        else ForOf_statement (left, e, body)
    | _ -> error t
  in
  let for_rest init =
    expect t T_SEMICOLON;
    let c =
      match cur t with
      | T_SEMICOLON -> None
      | _ -> Some (parse_expression t ~no_in:false)
    in
    expect t T_SEMICOLON;
    let incr =
      match cur t with
      | T_RPAREN -> None
      | _ -> Some (parse_expression t ~no_in:false)
    in
    expect t T_RPAREN;
    let body = parse_statement t in
    For_statement (init, c, incr, body)
  in
  (* [for (kind binding in/of ...)] or [for (kind declarations; ...)] *)
  let declaration kind =
    let binding =
      match cur t with
      | T_LBRACKET | T_LCURLY -> BindingPattern (parse_binding_pattern t)
      | _ -> BindingIdent (parse_identifier t)
    in
    match cur t with
    | T_IN | T_OF -> for_in_of (Right (kind, binding))
    | _ ->
        let first =
          match binding with
          | BindingIdent i -> DeclIdent (i, parse_initializer_opt t ~no_in:true)
          | BindingPattern pat -> DeclPattern (pat, parse_initializer t ~no_in:true)
        in
        let l =
          match cur t with
          | T_COMMA ->
              advance t;
              first :: parse_variable_declaration_list t ~no_in:true
          | _ -> [ first ]
        in
        for_rest (Right (kind, l))
  in
  let using_declaration () =
    (* Note that [for (await using of of x)] declares [of] *)
    let kind =
      match cur t with
      | T_AWAIT ->
          advance t;
          expect t T_USING;
          AwaitUsing
      | _ ->
          expect t T_USING;
          Using
    in
    (match cur t with
    | T_OF ->
        (* Record it as an identifier in the token list *)
        let _, loc = cur_raw t in
        t.toks.(t.pos) <- token_to_ident T_OF, loc
    | _ -> ());
    let i = parse_identifier t in
    match cur t with
    | T_IN | T_OF -> for_in_of (Right (kind, BindingIdent i))
    | _ ->
        let init = parse_initializer_opt t ~no_in:true in
        let l =
          match cur t with
          | T_COMMA ->
              advance t;
              let _, l = parse_using_declaration_rest t ~kind in
              DeclIdent (i, init) :: l
          | _ -> [ DeclIdent (i, init) ]
        in
        for_rest (Right (kind, l))
  in
  match cur t with
  | T_SEMICOLON -> for_rest (Left None)
  | T_VAR ->
      advance t;
      declaration Var
  | T_LET ->
      advance t;
      declaration Let
  | T_CONST ->
      advance t;
      declaration Const
  | T_USING when using_declaration_ahead t ~await:false -> using_declaration ()
  | T_AWAIT
    when using_declaration_ahead t ~await:true
         || Poly.equal (peek_tok t 1) Js_token.T_USING
            && Poly.equal (peek_tok t 2) Js_token.T_OF
            && Poly.equal (peek_tok t 3) Js_token.T_OF -> using_declaration ()
  | _ -> (
      let e = parse_expression t ~no_in:true in
      match cur t with
      | T_IN | T_OF -> for_in_of (Left (assignment_target_of_expr None e))
      | _ -> for_rest (Left (Some e)))

and parse_using_declaration_rest t ~kind =
  let rec loop acc =
    let i = parse_identifier t in
    let init = parse_initializer_opt t ~no_in:true in
    let d = DeclIdent (i, init) in
    match cur t with
    | T_COMMA ->
        advance t;
        loop (d :: acc)
    | _ -> kind, List.rev (d :: acc)
  in
  loop []

and parse_switch t =
  expect t T_SWITCH;
  expect t T_LPAREN;
  let e = parse_expression t ~no_in:false in
  expect t T_RPAREN;
  expect t T_LCURLY;
  let rec statements acc =
    match cur t with
    | T_CASE | T_DEFAULT | T_RCURLY | T_EOF -> List.rev acc
    | _ -> statements (parse_statement_list_item t ~module_:false :: acc)
  in
  let rec clauses before default after =
    match cur t with
    | T_CASE -> (
        advance t;
        let e = parse_expression t ~no_in:false in
        expect t T_COLON;
        let l = statements [] in
        match default with
        | None -> clauses ((e, l) :: before) default after
        | Some _ -> clauses before default ((e, l) :: after))
    | T_DEFAULT -> (
        match default with
        | Some _ -> error t
        | None ->
            advance t;
            expect t T_COLON;
            let l = statements [] in
            clauses before (Some l) after)
    | T_RCURLY ->
        advance t;
        Switch_statement (e, List.rev before, default, List.rev after)
    | _ -> error t
  in
  clauses [] None []

and parse_try t =
  expect t T_TRY;
  let b = parse_block t in
  let c =
    match cur t with
    | T_CATCH -> (
        advance t;
        match cur t with
        | T_LPAREN ->
            advance t;
            let param = parse_binding_element t in
            expect t T_RPAREN;
            let b = parse_block t in
            Some (Some param, b)
        | _ ->
            let b = parse_block t in
            Some (None, b))
    | _ -> None
  in
  let f =
    match cur t with
    | T_FINALLY ->
        advance t;
        Some (parse_block t)
    | _ -> None
  in
  (match c, f with
  | None, None -> error t
  | _ -> ());
  Try_statement (b, c, f)

(****)

(* Modules *)

and parse_module_specifier t =
  match cur t with
  | T_STRING (s, _) ->
      advance t;
      s
  | _ -> error t

and parse_from_clause t =
  expect t T_FROM;
  parse_module_specifier t

and parse_with_clause_opt t =
  match cur t with
  | T_WITH ->
      advance t;
      expect t T_LCURLY;
      let rec loop acc =
        match cur t with
        | T_RCURLY ->
            advance t;
            List.rev acc
        | _ -> (
            let key =
              match cur t with
              | T_STRING (s, _) ->
                  advance t;
                  s
              | _ -> parse_identifier_name t
            in
            expect t T_COLON;
            let value = parse_module_specifier t in
            match cur t with
            | T_COMMA ->
                advance t;
                loop ((key, value) :: acc)
            | T_RCURLY ->
                advance t;
                List.rev ((key, value) :: acc)
            | _ -> error t)
      in
      Some (loop [])
  | _ -> None

(* ModuleExportName: a string or an identifier name *)
and parse_module_export_name t =
  let pos = start_pos t in
  match cur t with
  | T_STRING (s, _) ->
      advance t;
      `String, s, pos
  | tok ->
      let is_ident =
        is_identifier tok
        || Poly.equal tok Js_token.T_YIELD
        || Poly.equal tok Js_token.T_AWAIT
      in
      let name = parse_identifier_name t in
      (if is_ident then `Ident else `Reserved), name, pos

and parse_namespace_import t =
  expect t T_MULT;
  expect t T_AS;
  parse_binding_identifier t

and parse_named_imports t =
  expect t T_LCURLY;
  let rec loop acc =
    match cur t with
    | T_RCURLY ->
        advance t;
        List.rev acc
    | _ -> (
        let spec =
          let kind, name, pos = parse_module_export_name t in
          match cur t, kind with
          | T_AS, _ ->
              advance t;
              let id = parse_binding_identifier t in
              name, id
          | _, `Ident -> name, var pos name
          | _ -> error t
        in
        match cur t with
        | T_COMMA ->
            advance t;
            loop (spec :: acc)
        | T_RCURLY ->
            advance t;
            List.rev (spec :: acc)
        | _ -> error t)
  in
  loop []

and parse_import t ~pos =
  expect t T_IMPORT;
  let kind, from =
    match cur t with
    | T_STRING _ ->
        let from = parse_module_specifier t in
        SideEffect, from
    | T_DEFER when Poly.equal (peek_tok t 1) Js_token.T_MULT ->
        advance t;
        let id = parse_namespace_import t in
        DeferNamespace id, parse_from_clause t
    | T_MULT ->
        let id = parse_namespace_import t in
        Namespace (None, id), parse_from_clause t
    | T_LCURLY ->
        let l = parse_named_imports t in
        Named (None, l), parse_from_clause t
    | _ -> (
        let default = parse_binding_identifier t in
        match cur t with
        | T_COMMA -> (
            advance t;
            match cur t with
            | T_MULT ->
                let id = parse_namespace_import t in
                Namespace (Some default, id), parse_from_clause t
            | _ ->
                let l = parse_named_imports t in
                Named (Some default, l), parse_from_clause t)
        | _ -> Default default, parse_from_clause t)
  in
  let withClause = parse_with_clause_opt t in
  consume_semicolon t;
  Import ({ from; kind; withClause }, pi pos), p pos

and parse_export_clause t =
  expect t T_LCURLY;
  let rec loop acc =
    match cur t with
    | T_RCURLY ->
        advance t;
        List.rev acc
    | _ -> (
        let local = parse_module_export_name t in
        let exported =
          match cur t with
          | T_AS ->
              advance t;
              parse_module_export_name t
          | _ -> local
        in
        match cur t with
        | T_COMMA ->
            advance t;
            loop ((local, exported) :: acc)
        | T_RCURLY ->
            advance t;
            List.rev ((local, exported) :: acc)
        | _ -> error t)
  in
  loop []

and parse_export t ~pos ~decorators =
  expect t T_EXPORT;
  let export k = Export (k, pi pos), p pos in
  let export_from kind =
    let from = parse_from_clause t in
    let withClause = parse_with_clause_opt t in
    consume_semicolon t;
    export (ExportFrom { kind; from; withClause })
  in
  match cur t with
  | T_DEFAULT -> (
      advance t;
      let dpos = start_pos t in
      let default_fun ~async =
        match parse_function_expression t ~pos:dpos ~async with
        | EFun (name, decl) ->
            consume_semicolon_opt t;
            export (ExportDefaultFun (name, decl))
        | _ -> assert false
      in
      match cur t with
      | T_CLASS | T_AT ->
          let decorators = parse_decorators t in
          let name, decl = parse_class t ~decorators ~name:`Optional in
          consume_semicolon_opt t;
          export (ExportDefaultClass (name, decl))
      | T_FUNCTION -> default_fun ~async:false
      | T_ASYNC when async_function_ahead t ->
          advance t;
          default_fun ~async:true
      | _ ->
          let e = parse_assignment t ~no_in:false in
          consume_semicolon t;
          export (ExportDefaultExpression e))
  | T_MULT -> (
      advance t;
      match cur t with
      | T_AS ->
          advance t;
          let _, name, _ = parse_module_export_name t in
          export_from (Export_all (Some name))
      | _ -> export_from (Export_all None))
  | T_LCURLY -> (
      let names = parse_export_clause t in
      match cur t with
      | T_FROM ->
          export_from
            (Export_names (List.map names ~f:(fun ((_, a, _), (_, b, _)) -> a, b)))
      | _ ->
          consume_semicolon t;
          let exception Invalid of Lexing.position in
          let k =
            try
              ExportNames
                (List.map names ~f:(fun ((k, id, pos), (_, s, _)) ->
                     match k with
                     | `Ident | `Reserved -> var pos id, s
                     | `String -> raise (Invalid pos)))
            with Invalid pos -> CoverExportFrom (early_error (pi pos))
          in
          export k)
  | T_VAR ->
      advance t;
      let l = parse_variable_declaration_list t ~no_in:false in
      consume_semicolon t;
      export (ExportVar (Var, l))
  | T_LET | T_CONST | T_USING | T_AWAIT | T_FUNCTION | T_ASYNC | T_CLASS | T_AT -> (
      let s, _ =
        match cur t, decorators with
        | T_CLASS, _ :: _ ->
            let name, decl = parse_class t ~decorators ~name:`Required in
            Class_declaration (Option.get name, decl), N
        | _ -> parse_statement_list_item t ~module_:false
      in
      match s with
      | Variable_statement (k, l) -> export (ExportVar (k, l))
      | Function_declaration (id, decl) -> export (ExportFun (id, decl))
      | Class_declaration (id, decl) -> export (ExportClass (id, decl))
      | _ -> error t)
  | _ -> error t

(****)

let parse_program t ~module_ =
  let rec loop acc =
    match cur t with
    | T_EOF -> List.rev acc
    | _ ->
        let pos = start_pos t in
        let s = parse_statement_list_item t ~module_ in
        loop ((pos, s) :: acc)
  in
  loop []

let fail_early =
  object (m)
    inherit Js_traverse.iter as super

    method early_error p = raise (Parsing_error p.loc)

    method statement s =
      match s with
      | Import (_, loc) -> raise (Parsing_error loc)
      | Export (_, loc) -> raise (Parsing_error loc)
      | _ -> super#statement s

    method program p =
      List.iter p ~f:(fun ((p : Javascript.statement), _loc) ->
          match p with
          | Import _ -> super#statement p
          | Export (e, _) -> (
              match e with
              | CoverExportFrom e -> m#early_error e
              | _ -> super#statement p)
          | _ -> super#statement p)
  end

let check_program p = List.iter p ~f:(function _, p -> fail_early#program [ p ])

let parse_aux script_or_module lex =
  let module_, await =
    match script_or_module with
    | `Script -> false, false
    | `Module -> true, true
  in
  let t = create lex ~yield:false ~await in
  let p =
    try parse_program t ~module_
    with Parsing_error _ as e ->
      if debug ()
      then (
        let toks = all_tokens t in
        let n = List.length toks in
        List.iteri toks ~f:(fun i (tok, _) ->
            if i >= n - 10 then Printf.eprintf "%s " (Js_token.to_string_extra tok));
        Printf.eprintf "\n");
      raise e
  in
  check_program p;
  p, t

let parse' script_or_module lex =
  let p, t = parse_aux script_or_module lex in
  let toks = all_tokens t in
  let take_annot_before =
    let toks_r = ref toks in
    let rec loop start_pos acc (toks : (Js_token.t * _) list) =
      match toks with
      | [] -> assert false
      | (TAnnot a, loc) :: xs ->
          loop start_pos ((a, Parse_info.t_of_pos (Loc.p1 loc)) :: acc) xs
      | ((TComment _ | TCommentLineDirective _), _) :: xs -> loop start_pos acc xs
      | (_, loc) :: xs ->
          if Loc.cnum loc = start_pos.Lexing.pos_cnum
          then (
            toks_r := toks;
            List.rev acc)
          else loop start_pos [] xs
    in
    fun start_pos -> loop start_pos [] !toks_r
  in
  let p = List.map p ~f:(fun (start_pos, s) -> take_annot_before start_pos, s) in
  let groups =
    List.group p ~f:(fun a _pred ->
        match a with
        | [], _ -> true
        | _ :: _, _ -> false)
  in
  let p =
    List.map groups ~f:(function
      | [] -> assert false
      | (annot, _) :: _ as l -> annot, List.map l ~f:snd)
  in
  p, toks

let parse script_or_module lex =
  let p, _ = parse_aux script_or_module lex in
  List.map p ~f:(fun (_, x) -> x)

let parse_expr lex =
  let t = create lex ~yield:false ~await:false in
  let e = parse_expression t ~no_in:false in
  expect t T_EOF;
  fail_early#expression e;
  e
