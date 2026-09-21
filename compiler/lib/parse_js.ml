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
     [parse'], and allows the arrow-function cover grammars
     (CoverParenthesizedExpressionAndArrowParameterList and
     CoverCallExpressionAndAsyncArrowHead) to be handled by re-parsing: a
     parenthesized expression or an [async (...)] call is first parsed as
     an expression, recording the span of tokens it covers. If, at the
     AssignmentExpression level, the expression is exactly that cover and
     is followed by [=>], the parser rewinds to the start of the cover and
     parses formal parameters instead.

   - Automatic semicolon insertion (ASI) follows ECMA-262 12.10: a
     semicolon is inserted before [}], at end of input, or when the
     offending token is on a new line; restricted productions ([return],
     [throw], [break], [continue], [yield], postfix [++]/[--], [async],
     [using]) check for a line terminator explicitly.

   - The [Yield], [Await] and [In] grammar parameters of ECMA-262 are
     threaded through the parsing functions, following the spec: a
     function parameterized by [Yield, Await] in the grammar takes a
     [ctx] argument, a record with fields [yield] and [await]; one
     parameterized by [In] takes the labelled argument [~no_in]. A new
     [ctx] is built where the spec sets the parameters ([+Await],
     [~Yield]): function bodies, class static blocks, module items.
     The lexer always produces the keyword tokens [T_YIELD] and [T_AWAIT];
     when the corresponding parameter is off, they are accepted as
     identifiers. (An escaped spelling such as [yi\u0065ld] is lexed as a
     plain identifier.) *)

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
    ; env : Flow_lexer.Lex_env.t
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

  let report_errors t =
    match Flow_lexer.Lex_env.take_errors t.env with
    | [] -> ()
    | l -> List.iter l ~f:t.report_error

  let token (t : t) =
    let res = Flow_lexer.lex t.env in
    report_errors t;
    res

  let lex_as_regexp (t : t) =
    Sedlexing.rollback t.l;
    let res = Flow_lexer.regexp t.env in
    report_errors t;
    res
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
  ; mutable cover_start : int
  ; mutable cover_end : int
        (* Indices of the first and last tokens of the last
           parenthesized expression or [async (...)] call *)
  }

let create lexbuf =
  { lexbuf
  ; toks = Array.make 64 (Js_token.T_EOF, dummy_loc)
  ; len = 0
  ; pos = 0
  ; prev_end = dummy_pos
  ; prev_line_end = -1
  ; prev_real = -1
  ; cover_start = -1
  ; cover_end = -1
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

let cur t = fst (cur_raw t)

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
    then tok, loc
    else find (i + 1) (n - 1)
  in
  find (t.pos + 1) n

let peek_tok t n = fst (peek t n)

(* Index of the current token *)
let cur_index t =
  ignore (cur_raw t);
  t.pos

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

(* Combinators for the recurring grammar shapes. The sub-parsers are thunks
   as they usually need the [ctx] and [~no_in] parameters. *)

(* Consume the current token if it is [tok] *)
let accept t (tok : Js_token.t) =
  if Poly.equal (cur t) tok
  then (
    advance t;
    true)
  else false

(* [(tok x)?] *)
let opt t tok f = if accept t tok then Some (f ()) else None

(* [left x right] *)
let between t left right f =
  expect t left;
  let x = f () in
  expect t right;
  x

(* [x (, x)*] *)
let comma_list1 t f =
  let rec loop acc =
    let x = f () in
    if accept t T_COMMA then loop (x :: acc) else List.rev (x :: acc)
  in
  loop []

(* [(x ,)* (x | ... rest)? close]: a possibly empty comma-separated list up
   to the token [close], which is consumed. A trailing comma is allowed,
   except after the rest element. Without [rest], [...] is left to [f]. *)
let comma_list_gen t ~close ~rest f =
  let rec loop acc =
    if accept t close
    then { list = List.rev acc; rest = None }
    else
      match cur t, rest with
      | T_ELLIPSIS, Some rest ->
          advance t;
          let r = rest () in
          expect t close;
          { list = List.rev acc; rest = Some r }
      | _ ->
          let x = f () in
          if accept t close
          then { list = List.rev (x :: acc); rest = None }
          else (
            expect t T_COMMA;
            loop (x :: acc))
  in
  loop []

let comma_list t ~close f = (comma_list_gen t ~close ~rest:None f).list

let comma_list_rest t ~close ~rest f = comma_list_gen t ~close ~rest:(Some rest) f

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

let all_tokens t = Array.to_list (Array.sub t.toks ~pos:0 ~len:t.len)

(****)

(* The [Yield] and [Await] grammar parameters: whether [yield] and [await]
   are keywords rather than identifiers *)
type ctx =
  { yield : bool
  ; await : bool
  }

let no_ctx = { yield = false; await = false }

(* Inside the parameters and the body of a function *)
let function_ctx { async; generator } = { yield = generator; await = async }

(* Identifiers *)

(* IdentifierReference[Yield, Await] / LabelIdentifier[Yield, Await]:
   identifiers and contextual keywords, plus [yield] and [await] when they
   are not keywords in the current context. *)
let ident_of_token ctx (tok : Js_token.t) =
  match tok with
  | T_IDENTIFIER (name, _) -> Some name
  | T_YIELD when not ctx.yield -> Some (utf8_s "yield")
  | T_AWAIT when not ctx.await -> Some (utf8_s "await")
  | _ when Js_token.is_contextual_keyword tok -> Some (utf8_s (Js_token.to_string tok))
  | _ -> None

let is_identifier ctx tok = Option.is_some (ident_of_token ctx tok)

(* IdentifierName: identifiers and reserved words *)
let identifier_name_of_token (tok : Js_token.t) =
  match tok with
  | T_IDENTIFIER (name, _) -> Some name
  | _ when Js_token.is_contextual_keyword tok || Js_token.is_reserved_word tok ->
      Some (utf8_s (Js_token.to_string tok))
  | _ -> None

let parse_identifier t ctx =
  match ident_of_token ctx (cur t) with
  | Some name ->
      let pos = start_pos t in
      advance t;
      var pos name
  | None -> error t

(* BindingIdentifier[Yield, Await] accepts [yield] and [await] whatever the
   context; that they are not allowed in some contexts is an early error,
   which is not checked. *)
let parse_binding_identifier t = parse_identifier t no_ctx

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
  | _ -> Js_token.is_contextual_keyword tok

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

(* The prefix of a MethodDefinition, in an object literal or a class body:
   [get], [set], [async], [*] or [async *]. Returns the kind of function
   and the constructor of the method. *)
let parse_method_modifier t =
  match cur t with
  | T_GET when not (keyword_is_name t) ->
      advance t;
      Some (no_fun, fun m -> MethodGet m)
  | T_SET when not (keyword_is_name t) ->
      advance t;
      Some (no_fun, fun m -> MethodSet m)
  | T_ASYNC when (not (keyword_is_name t)) && next_on_same_line t ->
      (* [async] on its own line is a property named [async] *)
      advance t;
      let generator = accept t T_MULT in
      Some ({ async = true; generator }, fun m -> Method m)
  | T_MULT ->
      advance t;
      Some ({ async = false; generator = true }, fun m -> Method m)
  | _ -> None

(* After [.] or [?.]: [name] or [#name] *)
let parse_member_access t e access =
  if accept t T_POUND
  then EDotPrivate (e, access, parse_identifier_name t)
  else EDot (e, access, parse_identifier_name t)

let variable_kind (tok : Js_token.t) =
  match tok with
  | T_VAR -> Var
  | T_LET -> Let
  | T_CONST -> Const
  | _ -> assert false

(****)

(* Expressions *)

let rec parse_expression t ctx ~no_in =
  let e = parse_assignment t ctx ~no_in in
  let rec loop e =
    if accept t T_COMMA then loop (ESeq (e, parse_assignment t ctx ~no_in)) else e
  in
  loop e

(* [( Expression )] *)
and parse_paren_expression t ctx =
  between t T_LPAREN T_RPAREN (fun () -> parse_expression t ctx ~no_in:false)

and parse_assignment t ctx ~no_in =
  match cur t with
  | T_YIELD when ctx.yield -> parse_yield t ctx ~no_in
  | tok
    when is_identifier ctx tok
         && Poly.equal (peek_tok t 1) Js_token.T_ARROW
         && next_on_same_line t ->
      (* x => body; no line terminator before [=>] *)
      let pos = start_pos t in
      let i = parse_identifier t ctx in
      expect t T_ARROW;
      let body, concise = parse_arrow_body t ~no_in ~async:false in
      EArrow ((no_fun, list [ param' i ], body, p pos), concise, AUnknown)
  | T_ASYNC -> (
      match peek t 1 with
      | T_OF, _ -> parse_assignment_rest t ctx ~no_in
      | tok, loc when is_identifier ctx tok && same_line (cur_loc t) loc ->
          (* async x => body *)
          let pos = start_pos t in
          advance t;
          let i = parse_identifier t { ctx with await = true } in
          if newline_before t then error t;
          expect t T_ARROW;
          let body, concise = parse_arrow_body t ~no_in ~async:true in
          EArrow
            ( ({ async = true; generator = false }, list [ param' i ], body, p pos)
            , concise
            , AUnknown )
      | T_LPAREN, loc when same_line (cur_loc t) loc -> parse_cover_or_arrow t ctx ~no_in
      | _ -> parse_assignment_rest t ctx ~no_in)
  | T_LPAREN -> parse_cover_or_arrow t ctx ~no_in
  | _ -> parse_assignment_rest t ctx ~no_in

and parse_assignment_rest t ctx ~no_in =
  let lhs = parse_conditional t ctx ~no_in in
  parse_assignment_operator t ctx ~no_in lhs

and parse_assignment_operator t ctx ~no_in lhs =
  match assignment_op (cur t) with
  | Some op ->
      advance t;
      let rhs = parse_assignment t ctx ~no_in in
      EBin (op, assignment_target_of_expr (Some op) lhs, rhs)
  | None -> lhs

(* An expression starting with [(] or [async (]: either an arrow function
   or an expression starting with a parenthesized expression or a call. The
   expression is parsed first; if it turns out to be exactly a cover
   followed by [=>], it is re-parsed as arrow parameters. *)
and parse_cover_or_arrow t ctx ~no_in =
  let pos = start_pos t in
  let m = mark t in
  let e = parse_conditional t ctx ~no_in in
  match cur t with
  | T_ARROW
    when (not (newline_before t)) && t.cover_start = m.m_pos && t.cover_end = t.prev_real
    ->
      reset t m;
      let async = accept t T_ASYNC in
      (* ArrowFormalParameters[?Yield, ?Await], or [~Yield, +Await] after
         [async] *)
      let params =
        if async
        then parse_formal_parameters t { yield = false; await = true }
        else parse_formal_parameters t ctx
      in
      expect t T_ARROW;
      let body, concise = parse_arrow_body t ~no_in ~async in
      EArrow (({ async; generator = false }, params, body, p pos), concise, AUnknown)
  | _ -> parse_assignment_operator t ctx ~no_in e

and parse_arrow_body t ~no_in ~async =
  let ctx = { yield = false; await = async } in
  match cur t with
  | T_LCURLY -> parse_block t ctx, false
  | _ ->
      let pos = start_pos t in
      let e = parse_assignment t ctx ~no_in in
      let stop = t.prev_end in
      [ Return_statement (Some e, p stop), p pos ], true

and parse_yield t ctx ~no_in =
  advance t;
  if newline_before t
  then EYield { delegate = false; expr = None }
  else
    match cur t with
    | T_MULT ->
        advance t;
        let e = parse_assignment t ctx ~no_in in
        EYield { delegate = true; expr = Some e }
    | tok when starts_expression tok ->
        let e = parse_assignment t ctx ~no_in in
        EYield { delegate = false; expr = Some e }
    | _ -> EYield { delegate = false; expr = None }

and parse_conditional t ctx ~no_in =
  let c = parse_short_circuit t ctx ~no_in in
  match cur t with
  | T_PLING ->
      advance t;
      let e1 = parse_assignment t ctx ~no_in:false in
      expect t T_COLON;
      let e2 = parse_assignment t ctx ~no_in in
      ECond (c, e1, e2)
  | _ -> c

(* ShortCircuitExpression: a [??] chain or a [||]/[&&] chain, not both *)
and parse_short_circuit t ctx ~no_in =
  let e = parse_binary t ctx ~no_in ~min_prec:3 in
  match cur t with
  | T_PLING_PLING ->
      let rec loop e =
        match cur t with
        | T_PLING_PLING ->
            advance t;
            let e2 = parse_binary t ctx ~no_in ~min_prec:3 in
            loop (EBin (Coalesce, e, e2))
        | T_OR | T_AND -> error t
        | _ -> e
      in
      loop e
  | T_OR | T_AND -> (
      let e = parse_binary_rest t ctx ~no_in ~min_prec:1 e in
      match cur t with
      | T_PLING_PLING -> error t
      | _ -> e)
  | _ -> e

and parse_binary t ctx ~no_in ~min_prec =
  let e = parse_exponentiation t ctx in
  parse_binary_rest t ctx ~no_in ~min_prec e

and parse_binary_rest t ctx ~no_in ~min_prec e =
  match binary_op ~no_in (cur t) with
  | Some (op, prec) when prec >= min_prec ->
      advance t;
      let e2 = parse_binary t ctx ~no_in ~min_prec:(prec + 1) in
      parse_binary_rest t ctx ~no_in ~min_prec (EBin (op, e, e2))
  | _ -> e

and parse_exponentiation t ctx =
  let e, is_unary = parse_unary t ctx in
  match cur t with
  | T_EXP ->
      (* [-a ** b] is a syntax error *)
      if is_unary then error t;
      advance t;
      let e2 = parse_exponentiation t ctx in
      EBin (Exp, e, e2)
  | _ -> e

(* Also returns whether the expression is a unary operator application
   (which cannot be the base of [**]) *)
and parse_unary t ctx =
  let unop ?(unary = true) op =
    advance t;
    let e, _ = parse_unary t ctx in
    EUn (op, e), unary
  in
  match cur t with
  | T_DELETE -> unop Delete
  | T_VOID -> unop Void
  | T_TYPEOF -> unop Typeof
  | T_PLUS -> unop Pl
  | T_MINUS -> unop Neg
  | T_BIT_NOT -> unop Bnot
  | T_NOT -> unop Not
  | T_AWAIT when ctx.await -> unop Await
  | T_INCR | T_INCR_NB -> unop ~unary:false IncrB
  | T_DECR | T_DECR_NB -> unop ~unary:false DecrB
  | _ -> (
      let e = parse_lhs t ctx in
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
and parse_lhs t ctx =
  let start = start_pos t in
  match cur t with
  | T_ASYNC when Poly.equal (peek_tok t 1) Js_token.T_LPAREN && next_on_same_line t ->
      (* CoverCallExpressionAndAsyncArrowHead: parsed as a call; see
         [parse_cover_or_arrow] *)
      let cover_start = cur_index t in
      let async = parse_identifier t ctx in
      let args = parse_arguments t ctx in
      t.cover_start <- cover_start;
      t.cover_end <- t.prev_real;
      parse_suffixes
        t
        ctx
        ~start
        ~allow_call:true
        (ECall (EVar async, ANormal, args, p start))
  | _ ->
      let e = parse_primary t ctx in
      parse_suffixes t ctx ~start ~allow_call:true e

(* Member accesses, calls, optional chains and tagged templates *)
and parse_suffixes t ctx ~start ~allow_call e =
  (* Location of calls: the start of the expression or, inside an optional
     chain, the position of the last [?.] *)
  let loc_start = ref start in
  let rec loop e =
    match cur t with
    | T_PERIOD ->
        advance t;
        loop (parse_member_access t e ANormal)
    | T_LBRACKET -> loop (EAccess (e, ANormal, parse_index t ctx))
    | T_BACKQUOTE ->
        let tpl = parse_template t ctx in
        loop (ECallTemplate (e, tpl, p !loc_start))
    | T_LPAREN when allow_call ->
        let args = parse_arguments t ctx in
        loop (ECall (e, ANormal, args, p !loc_start))
    | T_PLING_PERIOD when allow_call -> (
        loc_start := start_pos t;
        advance t;
        match cur t with
        | T_LPAREN ->
            let args = parse_arguments t ctx in
            loop (ECall (e, ANullish, args, p !loc_start))
        | T_LBRACKET -> loop (EAccess (e, ANullish, parse_index t ctx))
        | _ -> loop (parse_member_access t e ANullish))
    | _ -> e
  in
  loop e

(* [[ Expression ]] *)
and parse_index t ctx =
  between t T_LBRACKET T_RBRACKET (fun () -> parse_expression t ctx ~no_in:false)

and parse_new t ctx =
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
        | T_NEW -> parse_new t ctx
        | _ -> parse_primary t ctx
      in
      let callee = parse_suffixes t ctx ~start:callee_pos ~allow_call:false callee in
      match cur t with
      | T_LPAREN ->
          let args = parse_arguments t ctx in
          ENew (callee, Some args, p pos)
      | _ -> ENew (callee, None, p pos))

and parse_primary t ctx =
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
  | T_BACKQUOTE -> ETemplate (parse_template t ctx)
  | T_DIV | T_DIV_ASSIGN ->
      relex_regexp t;
      parse_primary t ctx
  | T_REGEXP (Utf8 s, flags) ->
      advance t;
      ERegexp (s, if String.equal flags "" then None else Some flags)
  | T_LBRACKET -> parse_array_literal t ctx
  | T_LCURLY -> parse_object_literal t ctx
  | T_LPAREN -> parse_parenthesized t ctx
  | T_FUNCTION -> parse_function_expression t ~pos ~async:false
  | T_ASYNC when async_function_ahead t ->
      advance t;
      parse_function_expression t ~pos ~async:true
  | T_CLASS | T_AT ->
      let decorators = parse_decorators t ctx in
      let name, decl = parse_class t ctx ~decorators ~name:`Optional in
      EClass (name, decl)
  | T_POUND ->
      advance t;
      let n = parse_identifier_name t in
      EPrivName n
  | T_NEW -> parse_new t ctx
  | tok when is_identifier ctx tok -> EVar (parse_identifier t ctx)
  | _ -> error t

(* CoverParenthesizedExpressionAndArrowParameterList, parsed as an
   expression; see [parse_cover_or_arrow] *)
and parse_parenthesized t ctx =
  let cover_start = cur_index t in
  expect t T_LPAREN;
  let cover_rest () =
    (* [...] binding, only valid as arrow parameters *)
    let ellipsis_pos = start_pos t in
    expect t T_ELLIPSIS;
    ignore (parse_binding_element t ctx);
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
                  let e2 = parse_assignment t ctx ~no_in:false in
                  loop (ESeq (e, e2)))
          | T_RPAREN ->
              advance t;
              `Expr e
          | _ -> error t
        in
        loop (parse_assignment t ctx ~no_in:false)
  in
  t.cover_start <- cover_start;
  t.cover_end <- t.prev_real;
  match res with
  | `Expr e -> e
  | `Cover e -> CoverParenthesizedExpressionAndArrowParameterList e

and parse_arguments t ctx =
  expect t T_LPAREN;
  comma_list t ~close:T_RPAREN (fun () ->
      if accept t T_ELLIPSIS
      then ArgSpread (parse_assignment t ctx ~no_in:false)
      else Arg (parse_assignment t ctx ~no_in:false))

and parse_template t ctx =
  expect t T_BACKQUOTE;
  let rec loop acc =
    match cur t with
    | T_ENCAPSED_STRING s ->
        advance t;
        loop (TStr (utf8_s s) :: acc)
    | T_DOLLARCURLY ->
        advance t;
        let e = parse_expression t ctx ~no_in:false in
        expect t T_RCURLY;
        loop (TExp e :: acc)
    | T_BACKQUOTE ->
        advance t;
        List.rev acc
    | _ -> error t
  in
  loop []

and parse_array_literal t ctx =
  expect t T_LBRACKET;
  EArr
    (comma_list t ~close:T_RBRACKET (fun () ->
         match cur t with
         | T_COMMA -> ElementHole (* the comma is consumed as the separator *)
         | T_ELLIPSIS ->
             advance t;
             ElementSpread (parse_assignment t ctx ~no_in:false)
         | _ -> Element (parse_assignment t ctx ~no_in:false)))

and parse_property_name t ctx =
  match cur t with
  | T_STRING (s, _) ->
      advance t;
      PNS s
  | T_NUMBER (_, raw) | T_BIGINT (_, raw) ->
      advance t;
      PNN (Num.of_string_unsafe raw)
  | T_LBRACKET ->
      PComputed
        (between t T_LBRACKET T_RBRACKET (fun () -> parse_assignment t ctx ~no_in:false))
  | _ -> PNI (parse_identifier_name t)

and parse_object_literal t ctx =
  expect t T_LCURLY;
  EObj (comma_list t ~close:T_RCURLY (fun () -> parse_property_definition t ctx))

and parse_property_definition t ctx =
  let pos = start_pos t in
  if accept t T_ELLIPSIS
  then PropertySpread (parse_assignment t ctx ~no_in:false)
  else
    match parse_method_modifier t with
    | Some (kind, meth) ->
        let name = parse_property_name t ctx in
        PropertyMethod (name, meth (parse_function_rest t ~pos kind))
    | None -> (
        let ident = ident_of_token ctx (cur t) in
        let name = parse_property_name t ctx in
        match cur t, ident with
        | T_COLON, _ ->
            advance t;
            Property (name, parse_assignment t ctx ~no_in:false)
        | T_LPAREN, _ -> PropertyMethod (name, Method (parse_function_rest t ~pos no_fun))
        | (T_COMMA | T_RCURLY), Some i ->
            (* shorthand property *)
            Property (PNI i, EVar (ident_unsafe i))
        | T_ASSIGN, Some i ->
            let eq_pos = start_pos t in
            advance t;
            let e = parse_assignment t ctx ~no_in:false in
            CoverInitializedName (early_error (pi eq_pos), var pos i, (e, p eq_pos))
        | _ -> error t)

(* Parameters and body of a function or method, after its name *)
and parse_function_rest t ~pos kind =
  let ctx = function_ctx kind in
  let params = parse_formal_parameters t ctx in
  let body = parse_block t ctx in
  kind, params, body, p pos

(****)

(* Functions *)

and parse_function_expression t ~pos ~async =
  expect t T_FUNCTION;
  let kind = { async; generator = accept t T_MULT } in
  (* Unlike a declaration, the name of a function expression is in the
     scope of the function itself: BindingIdentifier[+Yield] for a
     generator, BindingIdentifier[+Await] for an async function. *)
  let name =
    match cur t with
    | T_LPAREN -> None
    | _ -> Some (parse_identifier t (function_ctx kind))
  in
  EFun (name, parse_function_rest t ~pos kind)

and parse_function_declaration t ctx ~pos ~async =
  expect t T_FUNCTION;
  let kind = { async; generator = accept t T_MULT } in
  let name = parse_identifier t ctx in
  let body_ctx = function_ctx kind in
  let params = parse_formal_parameters t body_ctx in
  expect t T_LCURLY;
  let body = parse_function_body t body_ctx in
  (* For compatibility with the previous parser, plain function
     declarations are located at their closing brace. *)
  let pos = if async || kind.generator then pos else start_pos t in
  expect t T_RCURLY;
  name, (kind, params, body, p pos)

and parse_function_body t ctx =
  let rec loop acc =
    match cur t with
    | T_RCURLY | T_EOF -> List.rev acc
    | _ ->
        let s = parse_statement_list_item t ctx in
        loop (s :: acc)
  in
  loop []

(* [( FormalParameters )] *)
and parse_formal_parameters t ctx =
  expect t T_LPAREN;
  comma_list_rest
    t
    ~close:T_RPAREN
    ~rest:(fun () -> parse_single_name_binding t ctx)
    (fun () -> parse_binding_element t ctx)

and parse_binding_element t ctx =
  let b = parse_single_name_binding t ctx in
  let init = parse_initializer_opt t ctx ~no_in:false in
  b, init

and parse_single_name_binding t ctx =
  match cur t with
  | T_LBRACKET | T_LCURLY -> BindingPattern (parse_binding_pattern t ctx)
  | _ -> BindingIdent (parse_identifier t ctx)

and parse_initializer_opt t ctx ~no_in =
  match cur t with
  | T_ASSIGN -> Some (parse_initializer t ctx ~no_in)
  | _ -> None

and parse_initializer t ctx ~no_in =
  let pos = start_pos t in
  expect t T_ASSIGN;
  let e = parse_assignment t ctx ~no_in in
  e, p pos

and parse_binding_pattern t ctx =
  match cur t with
  | T_LCURLY -> parse_object_binding_pattern t ctx
  | T_LBRACKET -> parse_array_binding_pattern t ctx
  | _ -> error t

and parse_object_binding_pattern t ctx =
  expect t T_LCURLY;
  ObjectBinding
    (comma_list_rest
       t
       ~close:T_RCURLY
       ~rest:(fun () -> parse_identifier t ctx)
       (fun () ->
         match cur t with
         | tok
           when is_identifier ctx tok && not (Poly.equal (peek_tok t 1) Js_token.T_COLON)
           ->
             let i = parse_identifier t ctx in
             let init = parse_initializer_opt t ctx ~no_in:false in
             Prop_ident (Prop_and_ident i, init)
         | _ ->
             let name = parse_property_name t ctx in
             expect t T_COLON;
             let e = parse_binding_element t ctx in
             Prop_binding (name, e)))

and parse_array_binding_pattern t ctx =
  expect t T_LBRACKET;
  ArrayBinding
    (comma_list_rest
       t
       ~close:T_RBRACKET
       ~rest:(fun () -> parse_single_name_binding t ctx)
       (fun () ->
         match cur t with
         | T_COMMA -> None (* elision; the comma is consumed as the separator *)
         | _ -> Some (parse_binding_element t ctx)))

(****)

(* Classes *)

and parse_decorators t ctx =
  let rec loop acc =
    match cur t with
    | T_AT ->
        let pos = start_pos t in
        advance t;
        let d =
          match cur t with
          | T_LPAREN -> parse_paren_expression t ctx
          | _ -> (
              let rec member e =
                if accept t T_PERIOD then member (parse_member_access t e ANormal) else e
              in
              let e = member (EVar (parse_identifier t ctx)) in
              match cur t with
              | T_LPAREN ->
                  let args = parse_arguments t ctx in
                  ECall (e, ANormal, args, p pos)
              | _ -> e)
        in
        loop (d :: acc)
    | _ -> List.rev acc
  in
  loop []

and parse_class t ctx ~decorators ~name =
  expect t T_CLASS;
  let name =
    match name, cur t with
    | `Optional, (T_EXTENDS | T_LCURLY) -> None
    | _ -> Some (parse_binding_identifier t)
  in
  let extends = opt t T_EXTENDS (fun () -> parse_lhs t ctx) in
  let body = between t T_LCURLY T_RCURLY (fun () -> parse_class_body t ctx) in
  name, { decorators; extends; body }

and parse_class_element_name t ctx =
  match cur t with
  | T_POUND ->
      advance t;
      PrivName (parse_identifier_name t)
  | _ -> PropName (parse_property_name t ctx)

and parse_class_body t ctx =
  let rec loop acc =
    match cur t with
    | T_RCURLY -> List.rev acc
    | T_SEMICOLON ->
        advance t;
        loop acc
    | T_STATIC when Poly.equal (peek_tok t 1) Js_token.T_LCURLY ->
        advance t;
        loop (CEStaticBLock (parse_block t { yield = false; await = true }) :: acc)
    | _ ->
        let decorators = parse_decorators t ctx in
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
              let name = parse_class_element_name t ctx in
              let init = parse_initializer_opt t ctx ~no_in:false in
              consume_semicolon t;
              CEAccessor (decorators, static, name, init)
          | _ -> (
              let pos = start_pos t in
              match parse_method_modifier t with
              | Some (kind, meth) ->
                  let name = parse_class_element_name t ctx in
                  let m = parse_function_rest t ~pos kind in
                  CEMethod (decorators, static, name, meth m)
              | None -> (
                  let name = parse_class_element_name t ctx in
                  match cur t with
                  | T_LPAREN ->
                      let m = parse_function_rest t ~pos no_fun in
                      CEMethod (decorators, static, name, Method m)
                  | _ ->
                      let init = parse_initializer_opt t ctx ~no_in:false in
                      consume_semicolon t;
                      CEField (decorators, static, name, init)))
        in
        loop (elt :: acc)
  in
  loop []

(****)

(* Statements *)

and parse_block t ctx =
  expect t T_LCURLY;
  let body = parse_function_body t ctx in
  expect t T_RCURLY;
  body

and parse_variable_declaration_list t ctx ~no_in =
  comma_list1 t (fun () ->
      match cur t with
      | T_LBRACKET | T_LCURLY ->
          let pat = parse_binding_pattern t ctx in
          let init = parse_initializer t ctx ~no_in in
          DeclPattern (pat, init)
      | _ ->
          let i = parse_identifier t ctx in
          let init = parse_initializer_opt t ctx ~no_in in
          DeclIdent (i, init))

(* Whether the current token starts a [using] or [await using]
   declaration. They only bind identifiers (never [of]), and only when the
   first binding is on the same line as [using]. *)
and using_declaration_ahead t ctx =
  let binding_follows using_loc (tok, loc) =
    match (tok : Js_token.t) with
    | T_OF -> false
    | _ -> is_identifier ctx tok && same_line using_loc loc
  in
  match cur t with
  | T_USING -> binding_follows (cur_loc t) (peek t 1)
  | T_AWAIT when ctx.await -> (
      match peek t 1 with
      | T_USING, loc -> binding_follows loc (peek t 2)
      | _ -> false)
  | _ -> false

(* [using] or [await using] *)
and parse_using_kind t =
  let kind = if accept t T_AWAIT then AwaitUsing else Using in
  expect t T_USING;
  kind

and parse_using_bindings t ctx ~no_in =
  comma_list1 t (fun () ->
      let i = parse_identifier t ctx in
      let init = parse_initializer_opt t ctx ~no_in in
      DeclIdent (i, init))

(* ModuleItem : ImportDeclaration | ExportDeclaration | StatementListItem *)
and parse_module_item t ctx =
  let pos = start_pos t in
  match cur t with
  | T_IMPORT
    when match peek_tok t 1 with
         | T_LPAREN | T_PERIOD -> false (* [import(...)] and [import.meta] *)
         | _ -> true -> parse_import t ~pos
  | T_EXPORT -> parse_export t ~pos ~decorators:[]
  | T_AT -> (
      (* Decorators come before [export] *)
      let decorators = parse_decorators t ctx in
      match cur t with
      | T_EXPORT -> parse_export t ~pos ~decorators
      | _ -> parse_class_declaration t ctx ~decorators, p pos)
  | _ -> parse_statement_list_item t ctx

(* StatementListItem : Statement | Declaration *)
and parse_statement_list_item t ctx =
  if declaration_ahead t ctx then parse_declaration t ctx else parse_statement t ctx

and declaration_ahead t ctx =
  match cur t with
  | T_FUNCTION | T_CLASS | T_AT | T_LET | T_CONST -> true
  | T_ASYNC -> async_function_ahead t
  | T_USING | T_AWAIT -> using_declaration_ahead t ctx
  | _ -> false

(* Declaration : HoistableDeclaration | ClassDeclaration | LexicalDeclaration *)
and parse_declaration t ctx =
  let pos = start_pos t in
  let stmt s = s, p pos in
  match cur t with
  | (T_LET | T_CONST) as tok -> stmt (parse_variable_statement t ctx tok)
  | (T_USING | T_AWAIT) when using_declaration_ahead t ctx ->
      let kind = parse_using_kind t in
      let l = parse_using_bindings t ctx ~no_in:false in
      consume_semicolon t;
      stmt (Variable_statement (kind, l))
  | T_FUNCTION ->
      let name, decl = parse_function_declaration t ctx ~pos ~async:false in
      stmt (Function_declaration (name, decl))
  | T_ASYNC when async_function_ahead t ->
      advance t;
      let name, decl = parse_function_declaration t ctx ~pos ~async:true in
      stmt (Function_declaration (name, decl))
  | T_CLASS | T_AT ->
      let decorators = parse_decorators t ctx in
      stmt (parse_class_declaration t ctx ~decorators)
  | _ -> error t

and parse_class_declaration t ctx ~decorators =
  match parse_class t ctx ~decorators ~name:`Required with
  | Some name, decl -> Class_declaration (name, decl)
  | None, _ -> assert false

(* [var], [let] or [const] declarations *)
and parse_variable_statement t ctx tok =
  advance t;
  let l = parse_variable_declaration_list t ctx ~no_in:false in
  consume_semicolon t;
  Variable_statement (variable_kind tok, l)

(* Statement, which excludes declarations: they are not allowed as the
   body of an [if], a loop, [with] or a labelled statement. *)
and parse_statement t ctx =
  let pos = start_pos t in
  let stmt s = s, p pos in
  match cur t with
  | T_LCURLY -> stmt (Block (parse_block t ctx))
  | T_VAR -> stmt (parse_variable_statement t ctx T_VAR)
  | T_SEMICOLON ->
      advance t;
      stmt Empty_statement
  | T_IF ->
      advance t;
      let c = parse_paren_expression t ctx in
      let s1 = parse_statement t ctx in
      let s2 = opt t T_ELSE (fun () -> parse_statement t ctx) in
      stmt (If_statement (c, s1, s2))
  | T_DO ->
      advance t;
      let body = parse_statement t ctx in
      expect t T_WHILE;
      let c = parse_paren_expression t ctx in
      consume_semicolon_opt t;
      stmt (Do_while_statement (body, c))
  | T_WHILE ->
      advance t;
      let c = parse_paren_expression t ctx in
      let body = parse_statement t ctx in
      stmt (While_statement (c, body))
  | T_FOR -> stmt (parse_for t ctx)
  | T_CONTINUE ->
      advance t;
      let label = parse_label_opt t ctx in
      consume_semicolon t;
      stmt (Continue_statement label)
  | T_BREAK ->
      advance t;
      let label = parse_label_opt t ctx in
      consume_semicolon t;
      stmt (Break_statement label)
  | T_RETURN ->
      advance t;
      let e =
        match cur t with
        | T_SEMICOLON | T_RCURLY | T_EOF -> None
        | _ when newline_before t -> None
        | _ -> Some (parse_expression t ctx ~no_in:false)
      in
      let stop = t.prev_end in
      consume_semicolon t;
      stmt (Return_statement (e, p stop))
  | T_WITH ->
      advance t;
      let e = parse_paren_expression t ctx in
      let body = parse_statement t ctx in
      stmt (With_statement (e, body))
  | T_SWITCH -> stmt (parse_switch t ctx)
  | T_THROW ->
      advance t;
      if newline_before t then error t;
      let e = parse_expression t ctx ~no_in:false in
      consume_semicolon t;
      stmt (Throw_statement e)
  | T_TRY -> stmt (parse_try t ctx)
  | T_DEBUGGER ->
      advance t;
      consume_semicolon t;
      stmt Debugger_statement
  | tok when is_identifier ctx tok && Poly.equal (peek_tok t 1) Js_token.T_COLON ->
      let label = Label.of_string (Option.get (ident_of_token ctx tok)) in
      advance t;
      advance t;
      let body = parse_statement t ctx in
      stmt (Labelled_statement (label, body))
  | T_FUNCTION | T_CLASS | T_AT -> error t
  | T_ASYNC when async_function_ahead t -> error t
  | _ ->
      (* ExpressionStatement, which cannot start with [function], [async
         function] or [class] (above) *)
      let e = parse_expression t ctx ~no_in:false in
      consume_semicolon t;
      stmt (Expression_statement e)

and parse_label_opt t ctx =
  match cur t with
  | tok when is_identifier ctx tok && not (newline_before t) ->
      let label = Label.of_string (Option.get (ident_of_token ctx tok)) in
      advance t;
      Some label
  | _ -> None

and parse_for t ctx =
  expect t T_FOR;
  let for_await = ctx.await && accept t T_AWAIT in
  expect t T_LPAREN;
  let for_in_of left =
    match cur t with
    | T_IN when not for_await ->
        advance t;
        let e = parse_expression t ctx ~no_in:false in
        expect t T_RPAREN;
        let body = parse_statement t ctx in
        ForIn_statement (left, e, body)
    | T_OF ->
        advance t;
        let e = parse_assignment t ctx ~no_in:false in
        expect t T_RPAREN;
        let body = parse_statement t ctx in
        if for_await
        then ForAwaitOf_statement (left, e, body)
        else ForOf_statement (left, e, body)
    | _ -> error t
  in
  let for_rest init =
    (* [Expression? tok] *)
    let expression_opt tok =
      let e =
        if Poly.equal (cur t) tok
        then None
        else Some (parse_expression t ctx ~no_in:false)
      in
      expect t tok;
      e
    in
    expect t T_SEMICOLON;
    let c = expression_opt T_SEMICOLON in
    let incr = expression_opt T_RPAREN in
    let body = parse_statement t ctx in
    For_statement (init, c, incr, body)
  in
  (* [for (kind binding in/of ...)] or [for (kind declarations; ...)] *)
  let declaration kind =
    let binding = parse_single_name_binding t ctx in
    match cur t with
    | T_IN | T_OF -> for_in_of (Right (kind, binding))
    | _ ->
        let first =
          match binding with
          | BindingIdent i -> DeclIdent (i, parse_initializer_opt t ctx ~no_in:true)
          | BindingPattern pat -> DeclPattern (pat, parse_initializer t ctx ~no_in:true)
        in
        let l =
          if accept t T_COMMA
          then first :: parse_variable_declaration_list t ctx ~no_in:true
          else [ first ]
        in
        for_rest (Right (kind, l))
  in
  let using_declaration () =
    (* Note that [for (await using of of x)] declares [of] *)
    let kind = parse_using_kind t in
    (match cur t with
    | T_OF ->
        (* Record it as an identifier in the token list *)
        let _, loc = cur_raw t in
        t.toks.(t.pos) <- token_to_ident T_OF, loc
    | _ -> ());
    let i = parse_identifier t ctx in
    match cur t with
    | T_IN | T_OF -> for_in_of (Right (kind, BindingIdent i))
    | _ ->
        let init = parse_initializer_opt t ctx ~no_in:true in
        let first = DeclIdent (i, init) in
        let l =
          if accept t T_COMMA
          then first :: parse_using_bindings t ctx ~no_in:true
          else [ first ]
        in
        for_rest (Right (kind, l))
  in
  match cur t with
  | T_SEMICOLON -> for_rest (Left None)
  | (T_VAR | T_LET | T_CONST) as tok ->
      advance t;
      declaration (variable_kind tok)
  | T_USING when using_declaration_ahead t ctx -> using_declaration ()
  | T_AWAIT
    when ctx.await
         && (using_declaration_ahead t ctx
            || Poly.equal (peek_tok t 1) Js_token.T_USING
               && Poly.equal (peek_tok t 2) Js_token.T_OF
               && Poly.equal (peek_tok t 3) Js_token.T_OF) -> using_declaration ()
  | _ -> (
      let e = parse_expression t ctx ~no_in:true in
      match cur t with
      | T_IN | T_OF -> for_in_of (Left (assignment_target_of_expr None e))
      | _ -> for_rest (Left (Some e)))

and parse_switch t ctx =
  expect t T_SWITCH;
  let e = parse_paren_expression t ctx in
  expect t T_LCURLY;
  let rec statements acc =
    match cur t with
    | T_CASE | T_DEFAULT | T_RCURLY | T_EOF -> List.rev acc
    | _ -> statements (parse_statement_list_item t ctx :: acc)
  in
  let rec clauses before default after =
    match cur t with
    | T_CASE -> (
        advance t;
        let e = parse_expression t ctx ~no_in:false in
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

and parse_try t ctx =
  expect t T_TRY;
  let b = parse_block t ctx in
  let c =
    opt t T_CATCH (fun () ->
        let param =
          opt t T_LPAREN (fun () ->
              let param = parse_binding_element t ctx in
              expect t T_RPAREN;
              param)
        in
        let b = parse_block t ctx in
        param, b)
  in
  let f = opt t T_FINALLY (fun () -> parse_block t ctx) in
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
  opt t T_WITH (fun () ->
      expect t T_LCURLY;
      comma_list t ~close:T_RCURLY (fun () ->
          let key =
            match cur t with
            | T_STRING (s, _) ->
                advance t;
                s
            | _ -> parse_identifier_name t
          in
          expect t T_COLON;
          let value = parse_module_specifier t in
          key, value))

(* ModuleExportName: a string or an identifier name *)
and parse_module_export_name t =
  let pos = start_pos t in
  match cur t with
  | T_STRING (s, _) ->
      advance t;
      `String, s, pos
  | tok ->
      (* Whether it could be an ImportedBinding (a BindingIdentifier, which
         accepts [yield] and [await]) *)
      let is_ident = is_identifier no_ctx tok in
      let name = parse_identifier_name t in
      (if is_ident then `Ident else `Reserved), name, pos

and parse_namespace_import t =
  expect t T_MULT;
  expect t T_AS;
  parse_binding_identifier t

and parse_named_imports t =
  expect t T_LCURLY;
  comma_list t ~close:T_RCURLY (fun () ->
      let kind, name, pos = parse_module_export_name t in
      match cur t, kind with
      | T_AS, _ ->
          advance t;
          let id = parse_binding_identifier t in
          name, id
      | _, `Ident -> name, var pos name
      | _ -> error t)

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
  comma_list t ~close:T_RCURLY (fun () ->
      let local = parse_module_export_name t in
      let exported = if accept t T_AS then parse_module_export_name t else local in
      local, exported)

and parse_export t ~pos ~decorators =
  (* Exports are module top-level items: [~Yield, +Await] *)
  let ctx = { yield = false; await = true } in
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
          let decorators = parse_decorators t ctx in
          let name, decl = parse_class t ctx ~decorators ~name:`Optional in
          consume_semicolon_opt t;
          export (ExportDefaultClass (name, decl))
      | T_FUNCTION -> default_fun ~async:false
      | T_ASYNC when async_function_ahead t ->
          advance t;
          default_fun ~async:true
      | _ ->
          let e = parse_assignment t ctx ~no_in:false in
          consume_semicolon t;
          export (ExportDefaultExpression e))
  | T_MULT ->
      advance t;
      let name =
        opt t T_AS (fun () ->
            let _, name, _ = parse_module_export_name t in
            name)
      in
      export_from (Export_all name)
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
  | tok -> (
      (* [export] VariableStatement or [export] Declaration *)
      let s =
        match tok, decorators with
        | T_CLASS, _ :: _ -> parse_class_declaration t ctx ~decorators
        | T_VAR, _ -> parse_variable_statement t ctx T_VAR
        | _ -> fst (parse_declaration t ctx)
      in
      match s with
      | Variable_statement (k, l) -> export (ExportVar (k, l))
      | Function_declaration (id, decl) -> export (ExportFun (id, decl))
      | Class_declaration (id, decl) -> export (ExportClass (id, decl))
      | _ -> assert false)

(****)

(* Script : StatementList[~Yield, ~Await, ~Return]
   Module : ModuleItemList, whose items are [~Yield, +Await] *)
let parse_program t ~module_ =
  let ctx = { yield = false; await = module_ } in
  let rec loop acc =
    match cur t with
    | T_EOF -> List.rev acc
    | _ ->
        let pos = start_pos t in
        let s =
          if module_ then parse_module_item t ctx else parse_statement_list_item t ctx
        in
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
  let module_ =
    match script_or_module with
    | `Script -> false
    | `Module -> true
  in
  let t = create lex in
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
  let t = create lex in
  let e = parse_expression t no_ctx ~no_in:false in
  expect t T_EOF;
  fail_early#expression e;
  e
