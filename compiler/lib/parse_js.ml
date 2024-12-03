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
    let ic = open_in file in
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

module State : sig
  type token = Js_token.t * Loc.t

  module Cursor : sig
    type 'a t

    val insert_token : 'a t -> Js_token.t -> Loc.t -> 'a t

    val replace_token : 'a t -> Js_token.t -> Loc.t -> 'a t

    val last_token : 'a t -> (Js_token.t * Loc.t * 'a t) option

    val rewind_block : 'a t -> (Js_token.t * Loc.t * 'a t) option
  end

  type 'a t

  val save_checkpoint : 'a t -> 'a t

  val cursor : 'a t -> 'a Cursor.t

  val checkpoint : 'a t -> 'a Js_parser.MenhirInterpreter.checkpoint

  val offer : 'a t -> Js_token.t -> Loc.t -> 'a t

  val finalize_error : 'a t -> 'a t

  val try_recover : 'a Cursor.t -> 'a t

  val create : 'a Js_parser.MenhirInterpreter.checkpoint -> 'a t

  val all_tokens : 'a t -> token list
end = struct
  type token = Js_token.t * Loc.t

  type 'a checkpoint = 'a Js_parser.MenhirInterpreter.checkpoint

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

  let save_checkpoint { checkpoint; history; next } =
    { checkpoint; history = Checkpoint (checkpoint, history); next }

  let cursor { history; next; _ } = history, next

  let rec advance t =
    match (t : _ Js_parser.MenhirInterpreter.checkpoint) with
    | Shifting _ | AboutToReduce _ -> advance (Js_parser.MenhirInterpreter.resume t)
    | InputNeeded _ | Accepted _ | HandlingError _ | Rejected -> t

  let create checkpoint = { checkpoint; history = Start checkpoint; next = [] }

  let checkpoint { checkpoint; _ } = checkpoint

  let offer { checkpoint; history; next } tok loc : _ t =
    match (checkpoint : _ checkpoint) with
    | Accepted _ -> assert false
    | Rejected | HandlingError _ -> { checkpoint; history; next = (tok, loc) :: next }
    | Shifting _ | AboutToReduce _ -> assert false
    | InputNeeded _ -> (
        if is_comment tok
        then { checkpoint; history = Token (tok, loc, history); next }
        else
          let new_checkpoint =
            advance
              (Js_parser.MenhirInterpreter.offer checkpoint (tok, Loc.p1 loc, Loc.p2 loc))
          in
          match (new_checkpoint : 'a checkpoint) with
          | Shifting _ | AboutToReduce _ -> assert false
          | Rejected | Accepted _ | InputNeeded _ ->
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
          | HandlingError _ ->
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
            | InputNeeded _ as checkpoint ->
                advance
                  (Js_parser.MenhirInterpreter.offer
                     checkpoint
                     (tok, Loc.p1 loc, Loc.p2 loc))
            | Shifting _ | AboutToReduce _ -> assert false
            | Accepted _ | Rejected | HandlingError _ -> assert false)
    in
    let checkpoint = compute h in
    List.fold_left
      next
      ~init:{ checkpoint; history = h; next = [] }
      ~f:(fun t (tok, loc) -> offer t tok loc)

  let finalize_error { checkpoint; history; next } =
    let rec loop (t : _ Js_parser.MenhirInterpreter.checkpoint) =
      match t with
      | HandlingError _ | Shifting _ | AboutToReduce _ ->
          loop (Js_parser.MenhirInterpreter.resume t)
      | Accepted _ | InputNeeded _ -> assert false
      | Rejected -> t
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
  | Some (_, loc, _) -> Loc.line loc' <> Loc.line_end loc

let acceptable checkpoint token =
  let module I = Js_parser.MenhirInterpreter in
  let checkpoint = State.checkpoint checkpoint in
  I.acceptable checkpoint token Lexer.dummy_pos

let semicolon = Js_token.T_VIRTUAL_SEMICOLON

let dummy_loc = Loc.create Lexer.dummy_pos Lexer.dummy_pos

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
        | ( Some ((T_RETURN | T_CONTINUE | T_BREAK | T_THROW | T_YIELD | T_ASYNC), _, _)
          , ((T_SEMICOLON | T_VIRTUAL_SEMICOLON) as tok) ) -> tok, loc
        | Some ((T_RETURN | T_CONTINUE | T_BREAK | T_THROW | T_YIELD | T_ASYNC), _, _), _
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

let dummy_ident =
  let dummy = "<DUMMY>" in
  Js_token.T_IDENTIFIER (Stdlib.Utf8_string.of_string_exn dummy, dummy)

let token_to_ident t =
  let name = Js_token.to_string t in
  Js_token.T_IDENTIFIER (Stdlib.Utf8_string.of_string_exn name, name)

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
  | Some (offending_token, offending_loc, rest) -> (
      match State.Cursor.last_token rest with
      | None -> error_checkpoint
      | Some (last_token, _, _) -> (
          match offending_token with
          | T_VIRTUAL_SEMICOLON -> error_checkpoint
          (* contextually allowed as identifiers, namely await and yield; *)
          | (T_YIELD | T_AWAIT) when acceptable previous_checkpoint dummy_ident ->
              State.Cursor.replace_token
                rest
                (token_to_ident offending_token)
                offending_loc
              |> State.try_recover
          | T_RCURLY when acceptable previous_checkpoint Js_token.T_VIRTUAL_SEMICOLON ->
              State.Cursor.insert_token rest semicolon dummy_loc |> State.try_recover
          | T_EOF when acceptable previous_checkpoint Js_token.T_VIRTUAL_SEMICOLON ->
              State.Cursor.insert_token rest semicolon dummy_loc |> State.try_recover
          | T_ARROW when not (nl_separated rest offending_loc) -> (
              (* Restart parsing from the openning parens, patching the
                 token to be T_LPAREN_ARROW to help the parser *)
              match last_token with
              | T_RPAREN -> (
                  match State.Cursor.rewind_block rest with
                  | Some (T_LPAREN, loc, prev) ->
                      State.Cursor.replace_token prev T_LPAREN_ARROW loc
                      |> State.try_recover
                  | Some _ -> assert false
                  | None -> error_checkpoint)
              | _ -> error_checkpoint)
          | _ -> (
              match last_token with
              | T_VIRTUAL_SEMICOLON -> error_checkpoint
              | _
                when nl_separated rest offending_loc
                     && acceptable previous_checkpoint Js_token.T_VIRTUAL_SEMICOLON ->
                  State.Cursor.insert_token rest semicolon dummy_loc |> State.try_recover
              | T_RPAREN
                when acceptable previous_checkpoint Js_token.T_VIRTUAL_SEMICOLON_DO_WHILE
                -> State.Cursor.insert_token rest semicolon dummy_loc |> State.try_recover
              | T_RCURLY
                when acceptable
                       previous_checkpoint
                       Js_token.T_VIRTUAL_SEMICOLON_EXPORT_DEFAULT ->
                  State.Cursor.insert_token rest semicolon dummy_loc |> State.try_recover
              | _ -> error_checkpoint)))

let parse_aux the_parser (lexbuf : Lexer.t) =
  let init = the_parser (Lexer.curr_pos lexbuf) in
  let rec loop_error checkpoint =
    match (State.checkpoint checkpoint : _ Js_parser.MenhirInterpreter.checkpoint) with
    | InputNeeded _ | Shifting _ | AboutToReduce _ | Accepted _ -> assert false
    | Rejected -> `Error checkpoint
    | HandlingError _ -> loop_error checkpoint
  in
  let rec loop checkpoint previous_checkpoint =
    match (State.checkpoint checkpoint : _ Js_parser.MenhirInterpreter.checkpoint) with
    | Shifting _ | AboutToReduce _ -> assert false
    | Accepted v -> `Ok (v, checkpoint)
    | Rejected -> loop_error checkpoint
    | InputNeeded _env ->
        let previous_checkpoint = checkpoint in
        let new_checkpoint = offer_one checkpoint lexbuf in
        loop new_checkpoint previous_checkpoint
    | HandlingError _env -> (
        let error_checkpoint = checkpoint in
        let new_checkpoint = recover error_checkpoint previous_checkpoint in
        match State.checkpoint new_checkpoint with
        | HandlingError _ -> (
            let checkpoint = State.finalize_error new_checkpoint in
            match State.checkpoint checkpoint with
            | Rejected -> `Error checkpoint
            | _ -> assert false)
        | _ -> loop new_checkpoint new_checkpoint)
  in
  let checkpoint = State.create init in
  let res = loop checkpoint checkpoint in
  match res with
  | `Ok all -> all
  | `Error t ->
      let rec last cursor =
        match State.Cursor.last_token cursor with
        | None -> assert false
        | Some (T_VIRTUAL_SEMICOLON, _, cursor) -> last cursor
        | Some (_, loc, _) -> Loc.p1 loc
      in
      let p = last (State.cursor t) in
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

let parse' lex =
  let p, toks = parse_aux Js_parser.Incremental.program lex in
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
  let p, _ = parse_aux Js_parser.Incremental.program lex in
  check_program p;
  List.map p ~f:(fun (_, x) -> x)

let parse_expr lex =
  let expr, _ = parse_aux Js_parser.Incremental.standalone_expression lex in
  fail_early#expression expr;
  expr
