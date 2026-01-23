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

  val curr_pos : t -> Lexing.position

  val token : t -> Js_token.t * Loc.t

  val lex_as_regexp : t -> Js_token.t * Loc.t

  val rollback : t -> unit

  val dummy_pos : Lexing.position
end = struct
  type error = Loc.t * Flow_lexer.Parse_error.t

  type t =
    { l : Sedlexing.lexbuf
    ; report_error : error -> unit
    ; mutable env : Flow_lexer.Lex_env.t
    }

  let dummy_pos = { Lexing.pos_fname = ""; pos_lnum = 0; pos_cnum = 0; pos_bol = 0 }

  let zero_pos = { Lexing.pos_fname = ""; pos_lnum = 1; pos_cnum = 0; pos_bol = 0 }

  let print_error (loc, e) =
    let f = Loc.filename loc in
    let loc = Printf.sprintf "%s:%d:%d" f (Loc.line loc) (Loc.column loc) in
    Printf.eprintf "Lexer error: %s: %s\n" loc (Flow_lexer.Parse_error.to_string e)

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

  let curr_pos lexbuf = Sedlexing.lexing_position_curr lexbuf.l

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

  let rollback t = Sedlexing.rollback t.l

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

let is_comment = function
  | Js_token.TComment _ | TAnnot _ | TCommentLineDirective _ -> true
  | _ -> false

let token_to_ident t =
  let name = Js_token.to_string t in
  Js_token.T_IDENTIFIER (Stdlib.Utf8_string.of_string_exn name, name)

(* Handling contextual keywords [yield] and [await]:

   In JavaScript, [yield] and [await] are contextual keywords - they act as
   keywords inside generator/async functions but as regular identifiers outside.
   Instead of parametrizing the parser with [yield] and [await] parameters (which
   would significantly increase its size due to state explosion), we use virtual
   tokens to feed information back to the lexer.

   The parser emits virtual tokens (T_YIELDON_AWAITON, T_YIELDOFF_AWAITOFF, etc.)
   when entering/exiting contexts where [yield] or [await] should be keywords.
   These tokens update a [yield_await_state] stack that [normalize_token] uses
   to convert tokens: when [yield=true], an identifier "yield" becomes T_YIELD;
   when [yield=false], T_YIELD becomes an identifier. Same logic applies to [await].

   The T_YIELD_AWAIT_POP token pops the state stack when exiting a scope. *)
module State : sig
  type token = Js_token.t * Loc.t

  type yield_await_state =
    { yield : bool
    ; await : bool
    }

  type 'a checkpoint = 'a Js_parser.MenhirInterpreter.checkpoint * yield_await_state list

  val normalize_token : 'a checkpoint -> Js_token.t -> Js_token.t

  module Cursor : sig
    type 'a t

    val insert_token : 'a t -> Js_token.t -> Loc.t -> 'a t

    val replace_token : 'a t -> Js_token.t -> Loc.t -> 'a t

    val last_token : 'a t -> (Js_token.t * Loc.t * 'a t) option

    val rewind_block : 'a t -> (Js_token.t * Loc.t * 'a t) option
  end

  type 'a t

  val save_checkpoint : 'a t -> 'a t

  val pending : 'a t -> (token * 'a t) option

  val cursor : 'a t -> 'a Cursor.t

  val checkpoint : 'a t -> 'a checkpoint

  val offer : 'a t -> Js_token.t -> Loc.t -> 'a t

  val finalize_error : 'a t -> 'a t

  val try_recover : 'a Cursor.t -> 'a t

  val create : 'a Js_parser.MenhirInterpreter.checkpoint -> yield_await_state -> 'a t

  val all_tokens : 'a t -> token list
end = struct
  type token = Js_token.t * Loc.t

  type yield_await_state =
    { yield : bool
    ; await : bool
    }

  type 'a checkpoint = 'a Js_parser.MenhirInterpreter.checkpoint * yield_await_state list

  type 'a w =
    | Start of 'a checkpoint
    | Checkpoint of 'a checkpoint * 'a w
    | Token of Js_token.t * Loc.t * 'a w

  module Cursor = struct
    type 'a t = 'a w * token list

    let last_token ((h, next) : _ t) : (_ * _ * _ t) option =
      let rec find next = function
        | Start _ -> None
        | Checkpoint (_, t) -> find next t
        | Token (tok, loc, t) ->
            if is_comment tok
            then find ((tok, loc) :: next) t
            else Some (tok, loc, (t, (tok, loc) :: next))
      in
      find next h

    let replace_token ((h, next) : _ t) tok loc : _ t =
      match next with
      | [] -> assert false
      | _ :: next -> h, (tok, loc) :: next

    let insert_token ((h, next) : _ t) tok loc : _ t = h, (tok, loc) :: next

    let rewind_block : 'a t -> (Js_token.t * Loc.t * 'a t) option =
     fun h ->
      let rec rewind (stack : Js_token.t list) (h : _ t) =
        match last_token h with
        | None -> None
        | Some (tok, loc, h) -> (
            match tok, stack with
            | (T_RPAREN | T_RCURLY | T_RBRACKET), _ ->
                let stack = tok :: stack in
                rewind stack h
            | (T_LPAREN | T_LPAREN_ARROW), [ T_RPAREN ]
            | T_LBRACKET, [ T_RBRACKET ]
            | T_LCURLY, [ T_RCURLY ] -> Some (tok, loc, h)
            | (T_LPAREN | T_LPAREN_ARROW), T_RPAREN :: stack
            | T_LBRACKET, T_RBRACKET :: stack
            | T_LCURLY, T_RCURLY :: stack -> rewind stack h
            | T_LPAREN, _ -> assert false
            | T_LBRACKET, _ -> assert false
            | T_LCURLY, _ -> assert false
            | _, [] -> None
            | _, (_ :: _ as stack) -> rewind stack h)
      in
      rewind [] h
  end

  type 'a t =
    { checkpoint : 'a checkpoint
    ; history : 'a w
    ; next : token list
    }

  let pending t =
    match t.next with
    | [] -> None
    | x :: next -> Some (x, { t with next })

  let save_checkpoint { checkpoint; history; next } =
    { checkpoint; history = Checkpoint (checkpoint, history); next }

  let cursor { history; next; _ } = history, next

  let rec advance (t, ya) =
    match (t : _ Js_parser.MenhirInterpreter.checkpoint) with
    | Shifting _ -> advance (Js_parser.MenhirInterpreter.resume t, ya)
    | AboutToReduce _ -> advance (Js_parser.MenhirInterpreter.resume t, ya)
    | InputNeeded _ -> t, ya
    | Accepted _ -> t, ya
    | HandlingError _ -> t, ya
    | Rejected -> t, ya

  let advance' (checkpoint, yield_await) token =
    let checkpoint, yield_await =
      advance (Js_parser.MenhirInterpreter.offer checkpoint token, yield_await)
    in
    let yield_await =
      match token, yield_await with
      | ((T_YIELDOFF | T_YIELDON | T_AWAITOFF | T_AWAITON | T_YIELD_AWAIT_POP), _, _), []
        -> assert false
      | (T_YIELDOFF, _, _), ({ await; _ } :: _ as ya) -> { yield = false; await } :: ya
      | (T_YIELDON, _, _), ({ await; _ } :: _ as ya) -> { yield = true; await } :: ya
      | (T_AWAITOFF, _, _), ({ yield; _ } :: _ as ya) -> { yield; await = false } :: ya
      | (T_AWAITON, _, _), ({ yield; _ } :: _ as ya) -> { yield; await = true } :: ya
      | (T_YIELD_AWAIT_POP, _, _), _ :: xs -> xs
      | _ -> yield_await
    in
    checkpoint, yield_await

  let create checkpoint yield_await_state =
    let checkpoint = checkpoint, [ yield_await_state ] in
    { checkpoint; history = Start checkpoint; next = [] }

  let checkpoint { checkpoint; _ } = checkpoint

  let normalize_token (_checkpoint, yield_await_state) (tok : Js_token.t) =
    match yield_await_state with
    | [] -> assert false
    | yield_await :: _ -> (
        match tok, yield_await with
        | T_IDENTIFIER (_, "yield"), { yield = false; _ } -> tok
        | T_YIELD, { yield = true; _ } -> tok
        | T_IDENTIFIER (_, "await"), { await = false; _ } -> tok
        | T_AWAIT, { await = true; _ } -> tok
        | T_IDENTIFIER (_, "yield"), { yield = true; _ } -> T_YIELD
        | T_IDENTIFIER (_, "await"), { await = true; _ } -> T_AWAIT
        | T_YIELD, { yield = false; _ } | T_AWAIT, { await = false; _ } ->
            token_to_ident tok
        | _ -> tok)

  let offer { checkpoint; history; next } tok loc : _ t =
    match (checkpoint : _ checkpoint) with
    | Accepted _, _ -> assert false
    | Rejected, _ | HandlingError _, _ -> assert false
    | Shifting _, _ | AboutToReduce _, _ -> assert false
    | InputNeeded _, _ -> (
        if is_comment tok
        then { checkpoint; history = Token (tok, loc, history); next }
        else
          let tok = normalize_token checkpoint tok in
          let new_checkpoint = advance' checkpoint (tok, Loc.p1 loc, Loc.p2 loc) in
          match (new_checkpoint : 'a checkpoint) with
          | Shifting _, _ | AboutToReduce _, _ -> assert false
          | Rejected, _ | Accepted _, _ | InputNeeded _, _ ->
              let history =
                match tok with
                | T_VIRTUAL_SEMICOLON ->
                    let rec insert = function
                      | Start _ as start -> Token (tok, loc, start)
                      | Checkpoint (_, x) -> insert x
                      | Token (inner_tok, loc', tail) as x ->
                          if is_comment inner_tok
                          then Token (inner_tok, loc', insert tail)
                          else Token (tok, loc, x)
                    in
                    insert history
                | _ -> Token (tok, loc, history)
              in
              { checkpoint = new_checkpoint; history; next }
          | HandlingError _, _ ->
              { checkpoint = new_checkpoint
              ; history = Token (tok, loc, Checkpoint (checkpoint, history))
              ; next
              })

  let try_recover ((h, next) : _ Cursor.t) =
    let rec compute (t : _ w) =
      match t with
      | Start env -> advance env
      | Checkpoint (env, _) -> advance env
      | Token (tok, loc, t) -> (
          if is_comment tok
          then compute t
          else
            match compute t with
            | (InputNeeded _, _) as checkpoint ->
                advance' checkpoint (tok, Loc.p1 loc, Loc.p2 loc)
            | Shifting _, _ | AboutToReduce _, _ -> assert false
            | Accepted _, _ | Rejected, _ | HandlingError _, _ -> assert false)
    in
    let checkpoint = compute h in
    { checkpoint; history = h; next }

  let finalize_error { checkpoint; history; next } =
    let rec loop ((t, ya) : _ checkpoint) =
      match t with
      | HandlingError _ | Shifting _ | AboutToReduce _ ->
          loop (Js_parser.MenhirInterpreter.resume t, ya)
      | Accepted _ | InputNeeded _ -> assert false
      | Rejected -> t, ya
    in
    { checkpoint = loop checkpoint; history; next }

  let all_tokens { history; _ } =
    let rec collect acc t =
      match t with
      | Start _ -> acc
      | Checkpoint (_, tail) -> collect acc tail
      | Token (tok, loc, tail) -> collect ((tok, loc) :: acc) tail
    in
    collect [] history
end

let parse_annot s =
  match String.drop_prefix ~prefix:"//" s with
  | None -> None
  | Some s -> (
      let buf = Lexing.from_string s in
      try Some (Annot_parser.annot Annot_lexer.main buf) with
      | Not_found -> None
      | _ -> None)

let rec nl_separated prev loc' =
  match State.Cursor.last_token prev with
  | None -> true
  | Some (T_VIRTUAL_SEMICOLON, _, prev) -> nl_separated prev loc'
  | Some ((T_YIELD_AWAIT_POP | T_AWAITON | T_AWAITOFF | T_YIELDOFF | T_YIELDON), _, prev)
    -> nl_separated prev loc'
  | Some (_, loc, _) -> Loc.line loc' <> Loc.line_end loc

let rec last prev =
  match State.Cursor.last_token prev with
  | None -> raise Not_found
  | Some (T_VIRTUAL_SEMICOLON, _, prev) -> last prev
  | Some ((T_YIELD_AWAIT_POP | T_AWAITON | T_AWAITOFF | T_YIELDOFF | T_YIELDON), _, prev)
    -> last prev
  | Some (tok, _, _) -> tok

let acceptable state token =
  let module I = Js_parser.MenhirInterpreter in
  let checkpoint, _ = State.checkpoint state in
  I.acceptable checkpoint token Lexer.dummy_pos

let semicolon = Js_token.T_VIRTUAL_SEMICOLON

let dummy_loc = Loc.create Lexer.dummy_pos Lexer.dummy_pos

let dummy_ident = Js_token.T_IDENTIFIER (Utf8_string.of_string_exn "<DUMMY>", "<DUMMY>")

let rec offer_one t (lexbuf : Lexer.t) =
  let tok, loc = Lexer.token lexbuf in
  match tok with
  | TCommentLineDirective _ ->
      let t = State.offer t tok loc in
      offer_one t lexbuf
  | TComment s as tok ->
      let tok =
        match parse_annot s with
        | None -> tok
        | Some a -> TAnnot (s, a)
      in
      let t = State.offer t tok loc in
      offer_one t lexbuf
  | _ ->
      let tok = State.normalize_token (State.checkpoint t) tok in
      let t =
        match tok with
        | T_LPAREN when acceptable t T_LPAREN_ARROW -> State.save_checkpoint t
        | _ -> t
      in
      let h = State.cursor t in
      let tok, loc =
        (* restricted productions
         * 7.9.1 - 3
         * When, as the program is parsed from left to right, a token is encountered
         * that is allowed by some production of the grammar, but the production
         * is a restricted production and the token would be the first token for a
         * terminal or nonterminal immediately following the annotation [no LineTerminator here]
         * within the restricted production (and therefore such a token is called a restricted token),
         * and the restricted token is separated from the previous token by at least
         * one LineTerminator, then a semicolon is automatically inserted before the
         * restricted token. *)
        match State.Cursor.last_token h, tok with
        | ( Some
              ( (T_RETURN | T_CONTINUE | T_BREAK | T_THROW | T_YIELD | T_ASYNC | T_USING)
              , _
              , _ )
          , ((T_SEMICOLON | T_VIRTUAL_SEMICOLON) as tok) ) -> tok, loc
        | ( Some
              ( (T_RETURN | T_CONTINUE | T_BREAK | T_THROW | T_YIELD | T_ASYNC | T_USING)
              , _
              , _ )
          , _ )
          when nl_separated h loc && acceptable t T_VIRTUAL_SEMICOLON ->
            (* restricted token can also appear as regular identifier such
               as in [x.return]. In such case, feeding a virtual semicolon
               could trigger a parser error. Here, we first checkpoint
               that a virtual semicolon is acceptable. *)
            Lexer.rollback lexbuf;
            semicolon, dummy_loc
        (* The practical effect of these restricted productions is as follows:
         * When a ++ or -- token is encountered where the parser would treat it
         * as a postfix operator, and at least one LineTerminator occurred between
         * the preceding token and the ++ or -- token, then a semicolon is automatically
         * inserted before the ++ or -- token. *)
        | _, T_DECR when not (nl_separated h loc) -> Js_token.T_DECR_NB, loc
        | _, T_INCR when not (nl_separated h loc) -> Js_token.T_INCR_NB, loc
        | _, ((T_DIV | T_DIV_ASSIGN) as tok) ->
            if acceptable t tok
            then tok, loc
            else
              let t, loc = Lexer.lex_as_regexp lexbuf in
              t, loc
        | _ -> tok, loc
      in
      State.offer t tok loc

let recover error_checkpoint previous_checkpoint =
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
  match State.Cursor.last_token (State.cursor error_checkpoint) with
  | None -> error_checkpoint
  | Some (offending_token, offending_loc, rest) ->
      let checkpoint =
        match State.Cursor.last_token rest with
        | None -> error_checkpoint
        | Some (last_token, _, _) -> (
            match offending_token with
            | T_VIRTUAL_SEMICOLON -> error_checkpoint
            (* ASI rule: insert semicolon before '}' *)
            | T_RCURLY when acceptable previous_checkpoint Js_token.T_VIRTUAL_SEMICOLON ->
                State.Cursor.insert_token rest semicolon dummy_loc |> State.try_recover
            (* ASI rule: insert semicolon at end of input *)
            | T_EOF when acceptable previous_checkpoint Js_token.T_VIRTUAL_SEMICOLON ->
                State.Cursor.insert_token rest semicolon dummy_loc |> State.try_recover
            (* Arrow function recovery: disambiguate '(params) =>' from parenthesized expr *)
            | T_ARROW when not (nl_separated rest offending_loc) -> (
                match last rest with
                | T_RPAREN -> (
                    (* Restart parsing from the opening parens, patching the
                       token to be T_LPAREN_ARROW to help the parser *)
                    match State.Cursor.rewind_block rest with
                    | Some (T_LPAREN, loc, prev) ->
                        State.Cursor.replace_token prev T_LPAREN_ARROW loc
                        |> State.try_recover
                    | Some _ -> assert false
                    | None -> error_checkpoint)
                | _ -> error_checkpoint)
            | T_OF
              when Poly.equal last_token T_USING
                   && acceptable previous_checkpoint dummy_ident ->
                State.Cursor.replace_token rest (token_to_ident T_OF) offending_loc
                |> State.try_recover
            | _ -> (
                match last_token with
                | T_VIRTUAL_SEMICOLON -> error_checkpoint
                (* ASI rule: insert semicolon when offending token is on a new line *)
                | _
                  when nl_separated rest offending_loc
                       && acceptable previous_checkpoint Js_token.T_VIRTUAL_SEMICOLON ->
                    State.Cursor.insert_token rest semicolon dummy_loc
                    |> State.try_recover
                (* Special ASI for do-while: 'do stmt while (expr)' needs no semicolon *)
                | T_RPAREN
                  when acceptable
                         previous_checkpoint
                         Js_token.T_VIRTUAL_SEMICOLON_DO_WHILE ->
                    State.Cursor.insert_token rest semicolon dummy_loc
                    |> State.try_recover
                (* Special ASI for 'export default function/class { }' *)
                | T_RCURLY
                  when acceptable
                         previous_checkpoint
                         Js_token.T_VIRTUAL_SEMICOLON_EXPORT_DEFAULT ->
                    State.Cursor.insert_token rest semicolon dummy_loc
                    |> State.try_recover
                | _ -> error_checkpoint))
      in
      if phys_equal checkpoint error_checkpoint
      then
        match State.Cursor.last_token rest with
        | None -> checkpoint
        | Some (T_OF, last_token_loc, last_token_prev)
          when match last last_token_prev with
               | T_USING | T_ASYNC -> true
               | _ -> false ->
            State.Cursor.replace_token
              last_token_prev
              (token_to_ident T_OF)
              last_token_loc
            |> State.try_recover
        (* Entering function body after '{': push yield/await state.
                   The appropriate state depends on the function kind
                   (regular, generator, async, async generator). *)
        | _ when acceptable previous_checkpoint T_YIELD_AWAIT_POP ->
            State.Cursor.insert_token rest T_YIELD_AWAIT_POP dummy_loc
            |> State.try_recover
        | _ when acceptable previous_checkpoint T_YIELDOFF ->
            State.Cursor.insert_token rest T_YIELDOFF dummy_loc |> State.try_recover
        | _ when acceptable previous_checkpoint T_YIELDON ->
            State.Cursor.insert_token rest T_YIELDON dummy_loc |> State.try_recover
        | _ when acceptable previous_checkpoint T_AWAITON ->
            State.Cursor.insert_token rest T_AWAITON dummy_loc |> State.try_recover
        | _ when acceptable previous_checkpoint T_AWAITOFF ->
            State.Cursor.insert_token rest T_AWAITOFF dummy_loc |> State.try_recover
        | _ -> checkpoint
      else checkpoint

let parse_aux the_parser yield_await_state (lexbuf : Lexer.t) =
  let init = the_parser (Lexer.curr_pos lexbuf) in
  let rec loop_error checkpoint =
    match State.checkpoint checkpoint with
    | InputNeeded _, _ | Shifting _, _ | AboutToReduce _, _ | Accepted _, _ ->
        assert false
    | Rejected, _ -> `Error checkpoint
    | HandlingError _, _ -> loop_error checkpoint
  in
  let rec loop checkpoint previous_checkpoint =
    match State.checkpoint checkpoint with
    | Shifting _, _ | AboutToReduce _, _ -> assert false
    | Accepted v, _ -> `Ok (v, checkpoint)
    | Rejected, _ -> loop_error checkpoint
    | InputNeeded _env, _ -> (
        match State.pending checkpoint with
        | None ->
            let previous_checkpoint = checkpoint in
            let new_checkpoint = offer_one checkpoint lexbuf in
            loop new_checkpoint previous_checkpoint
        | Some ((tok, loc), previous_checkpoint) ->
            let new_checkpoint = State.offer previous_checkpoint tok loc in
            loop new_checkpoint previous_checkpoint)
    | HandlingError _, _ -> (
        let error_checkpoint = checkpoint in
        let new_checkpoint = recover error_checkpoint previous_checkpoint in
        match State.checkpoint new_checkpoint with
        | HandlingError _, _ -> (
            let checkpoint = State.finalize_error new_checkpoint in
            match State.checkpoint checkpoint with
            | Rejected, _ -> `Error checkpoint
            | _ -> assert false)
        | _ -> loop new_checkpoint new_checkpoint)
  in
  let checkpoint = State.create init yield_await_state in
  let res = loop checkpoint checkpoint in
  match res with
  | `Ok all -> all
  | `Error t ->
      let rec last cursor =
        match State.Cursor.last_token cursor with
        | None -> assert false
        | Some
            ( ( T_VIRTUAL_SEMICOLON
              | T_AWAITOFF
              | T_AWAITON
              | T_YIELDON
              | T_YIELDOFF
              | T_YIELD_AWAIT_POP )
            , _
            , cursor ) -> last cursor
        | Some (_, loc, _) -> Loc.p1 loc
      in
      let p = last (State.cursor t) in
      let rec lastn n acc cursor =
        if n = 0
        then acc
        else
          match State.Cursor.last_token cursor with
          | None -> acc
          | Some (tok, _, cursor) -> lastn (pred n) (tok :: acc) cursor
      in
      if debug ()
      then
        List.iter
          (lastn 10 [] (State.cursor t))
          ~f:(fun tok -> Printf.eprintf "%s " (Js_token.to_string_extra tok));
      if debug () then Printf.eprintf "\n";
      raise (Parsing_error (Parse_info.t_of_pos p))

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

let parse' lex script_or_module =
  let p, toks =
    parse_aux
      Js_parser.Incremental.program
      { yield = false
      ; await =
          (match script_or_module with
          | `Script -> false
          | `Module -> true)
      }
      lex
  in
  check_program p;
  let toks = State.all_tokens toks in
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

let parse lex =
  let p, _ =
    parse_aux Js_parser.Incremental.program { yield = false; await = true } lex
  in
  check_program p;
  List.map p ~f:(fun (_, x) -> x)

let parse_expr lex =
  let expr, _ =
    parse_aux
      Js_parser.Incremental.standalone_expression
      { yield = false; await = false }
      lex
  in
  fail_early#expression expr;
  expr
