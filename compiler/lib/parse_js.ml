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

  val of_file : string -> t

  val of_channel : in_channel -> t

  val of_string : ?pos:Lexing.position -> ?filename:string -> string -> t

  val curr_pos : t -> Lexing.position

  val token : t -> Js_token.t * (Lexing.position * Lexing.position)

  val lex_as_regexp : t -> Js_token.t * (Lexing.position * Lexing.position)

  val dummy_pos : Lexing.position
end = struct
  type t =
    { l : Sedlexing.lexbuf
    ; mutable env : Flow_lexer.Lex_env.t
    }

  let dummy_pos = { Lexing.pos_fname = ""; pos_lnum = 0; pos_cnum = 0; pos_bol = 0 }

  let zero_pos = { Lexing.pos_fname = ""; pos_lnum = 1; pos_cnum = 0; pos_bol = 0 }

  let create l = { l; env = Flow_lexer.Lex_env.create l }

  let of_file file : t =
    let ic = open_in file in
    let lexbuf = Sedlexing.Utf8.from_channel ic in
    Sedlexing.set_filename lexbuf file;
    create lexbuf

  let of_channel ci : t = create (Sedlexing.Utf8.from_channel ci)

  let of_string ?(pos = zero_pos) ?filename s =
    let l = Sedlexing.Utf8.from_string s in
    let pos =
      match filename with
      | None -> pos
      | Some pos_fname -> { pos with pos_fname }
    in
    Sedlexing.set_position l pos;
    Option.iter filename ~f:(Sedlexing.set_filename l);
    create l

  let curr_pos lexbuf = snd (Sedlexing.lexing_positions lexbuf.l)

  let report_errors res =
    match Flow_lexer.Lex_result.errors res with
    | [] -> ()
    | l ->
        List.iter l ~f:(fun (loc, e) ->
            let loc =
              match loc.Flow_lexer.Loc.source with
              | None ->
                  Printf.sprintf
                    "%d:%d"
                    loc.start.pos_lnum
                    (loc.start.pos_cnum - loc.start.pos_bol)
              | Some f ->
                  Printf.sprintf
                    "%s:%d:%d"
                    f
                    loc.start.pos_lnum
                    (loc.start.pos_cnum - loc.start.pos_bol)
            in

            Printf.eprintf
              "Lexer error: %s: %s\n"
              loc
              (Flow_lexer.Parse_error.to_string e))

  let token (t : t) =
    let env, res = Flow_lexer.lex t.env in
    t.env <- env;
    let tok = Flow_lexer.Lex_result.token res in
    let pos = Flow_lexer.Lex_result.loc res in
    report_errors res;
    tok, pos

  let lex_as_regexp (t : t) =
    Sedlexing.rollback t.l;
    let env, res = Flow_lexer.regexp t.env in
    t.env <- env;
    let tok = Flow_lexer.Lex_result.token res in
    let pos = Flow_lexer.Lex_result.loc res in
    report_errors res;
    tok, pos
end

exception Parsing_error of Parse_info.t

let matching_token (o : Js_token.t) (c : Js_token.t) =
  match o, c with
  | T_LPAREN, T_RPAREN | T_LBRACKET, T_RBRACKET | T_LCURLY, T_RCURLY -> true
  | _ -> false

module Tokens : sig
  type elt = Js_token.t * (Lexing.position * Lexing.position)

  type +'a t

  val add : elt -> 'a -> 'a t -> 'a t

  val last : 'a t -> elt option

  val last' : 'a t -> (elt * 'a t * 'a) option

  val empty : 'a t

  val all : 'a t -> (Js_token.t * Parse_info.t) list
end = struct
  type elt = Js_token.t * (Lexing.position * Lexing.position)

  type 'a t = (elt * 'a) list

  let empty = []

  let add elt data t = (elt, data) :: t

  let rec last = function
    | [] -> None
    | (((Js_token.TComment _ | TCommentLineDirective _), _), _) :: l -> last l
    | (x, _) :: _ -> Some x

  let rec last' = function
    | [] -> None
    | (((Js_token.TComment _ | TCommentLineDirective _), _), _) :: l -> last' l
    | (x, data) :: l -> Some (x, l, data)

  let all t_rev = List.rev_map t_rev ~f:(fun ((t, (p, _)), _) -> t, Parse_info.t_of_pos p)
end

let parse_aux the_parser (lexbuf : Lexer.t) =
  let init = the_parser (Lexer.curr_pos lexbuf) in
  let fol prev (_, (c, _)) =
    match Tokens.last prev with
    | None -> true
    | Some (_, (_, p)) -> c.Lexing.pos_lnum <> p.Lexing.pos_lnum
  in
  let rec loop_error prev checkpoint =
    let module I = Js_parser.MenhirInterpreter in
    match checkpoint with
    | I.InputNeeded _env ->
        let checkpoint =
          I.offer checkpoint (Js_token.T_EOF, Lexer.curr_pos lexbuf, Lexer.curr_pos lexbuf)
        in
        loop_error prev checkpoint
    | I.Shifting _ | I.AboutToReduce _ -> loop_error prev (I.resume checkpoint)
    | I.Accepted _ -> assert false
    | I.Rejected -> `Error prev
    | I.HandlingError _ -> loop_error prev (I.resume checkpoint)
  in
  let parse_annot s =
    match String.drop_prefix ~prefix:"//" s with
    | None -> None
    | Some s -> (
        let buf = Lexing.from_string s in
        try
          match Annot_parser.annot Annot_lexer.main buf with
          | `Requires l -> Some (`Requires l)
          | `Provides (n, k, ka) -> Some (`Provides (n, k, ka))
          | `Version l -> Some (`Version l)
          | `Weakdef -> Some `Weakdef
          | `Always -> Some `Always
          | `If name -> Some (`If name)
          | `Ifnot name -> Some (`Ifnot name)
          | `Alias name -> Some (`Alias name)
        with
        | Not_found -> None
        | _ -> None)
  in
  let rec loop prev buffer checkpoint =
    let module I = Js_parser.MenhirInterpreter in
    match checkpoint with
    | I.InputNeeded _env ->
        let token, buffer, prev =
          match Tokens.last prev with
          | Some ((Js_token.T_EOF, _) as last) -> last, buffer, prev
          | _ ->
              let read_tok buffer lexbuf =
                match buffer with
                | [] -> buffer, Lexer.token lexbuf
                | x :: xs -> xs, x
              in
              let rec read_one prev buffer (lexbuf : Lexer.t) =
                let buffer, t = read_tok buffer lexbuf in
                match t with
                | (TCommentLineDirective _, _) as t ->
                    let prev = Tokens.add t checkpoint prev in
                    read_one prev buffer lexbuf
                | (TComment s, loc) as t ->
                    if fol prev t
                    then
                      match parse_annot s with
                      | None ->
                          let prev = Tokens.add t checkpoint prev in
                          read_one prev buffer lexbuf
                      | Some annot ->
                          let t = Js_token.TAnnot (s, annot), loc in
                          t, buffer, prev
                    else
                      let prev = Tokens.add t checkpoint prev in
                      read_one prev buffer lexbuf
                | t -> t, buffer, prev
              in
              let t, buffer, prev = read_one prev buffer lexbuf in
              let (t, pos), buffer =
                match Tokens.last prev, t with
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
                | ( Some ((T_RETURN | T_CONTINUE | T_BREAK | T_THROW | T_YIELD), _)
                  , (((T_SEMICOLON | T_VIRTUAL_SEMICOLON), _) as t) ) -> t, buffer
                | Some ((T_RETURN | T_CONTINUE | T_BREAK | T_THROW | T_YIELD), _), t
                  when fol prev t ->
                    let buffer = t :: buffer in
                    (T_VIRTUAL_SEMICOLON, (Lexer.dummy_pos, Lexer.dummy_pos)), buffer
                (* The practical effect of these restricted productions is as follows:
                 * When a ++ or -- token is encountered where the parser would treat it
                 * as a postfix operator, and at least one LineTerminator occurred between
                 * the preceding token and the ++ or -- token, then a semicolon is automatically
                 * inserted before the ++ or -- token. *)
                | _, ((T_DECR, pos) as tok) when not (fol prev tok) ->
                    (Js_token.T_DECR_NB, pos), buffer
                | _, ((T_INCR, pos) as tok) when not (fol prev tok) ->
                    (Js_token.T_INCR_NB, pos), buffer
                | _, ((((T_DIV | T_DIV_ASSIGN) as t), ((start_pos, _) as _pos)) as tok)
                  -> (
                    if I.acceptable checkpoint t start_pos
                    then tok, buffer
                    else
                      match buffer with
                      | [] -> Lexer.lex_as_regexp lexbuf, buffer
                      | _ ->
                          (* Trying to lex token differently, not allowed *) tok, buffer)
                | _, t -> t, buffer
              in
              (t, pos), buffer, prev
        in
        let t, (pos_start, pos_stop) = token in
        let prev = Tokens.add token checkpoint prev in
        let checkpoint = I.offer checkpoint (t, pos_start, pos_stop) in
        loop prev buffer checkpoint
    | I.Shifting _ | I.AboutToReduce _ -> loop prev buffer (I.resume checkpoint)
    | I.Accepted v -> `Ok (v, prev)
    | I.Rejected -> `Error prev
    | I.HandlingError _env -> (
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
        let to_ident (t, loc) =
          let name = Js_token.to_string t in
          Js_token.T_IDENTIFIER (Stdlib.Utf8_string.of_string_exn name, name), loc
        in
        let rec rewind stack buffer prev =
          match Tokens.last' prev with
          | None -> None
          | Some (((tok, loc) as tok'), prev, checkpoint) -> (
              match tok, stack with
              | (T_RPAREN | T_RCURLY | T_RBRACKET), _ ->
                  let buffer = tok' :: buffer in
                  let stack = tok :: stack in
                  rewind stack buffer prev
              | ((T_LPAREN | T_LCURLY | T_LBRACKET) as o), c :: stack -> (
                  if not (matching_token o c)
                  then None
                  else
                    match stack with
                    | [] -> Some (loc, prev, buffer, checkpoint)
                    | _ ->
                        let buffer = tok' :: buffer in
                        rewind stack buffer prev)
              | _, stack ->
                  let buffer = tok' :: buffer in
                  rewind stack buffer prev)
        in
        let end_of_do_whle prev =
          match rewind [ T_RPAREN ] [] prev with
          | None -> false
          | Some (_, prev, _, _) -> (
              match Tokens.last' prev with
              | None -> false
              | Some ((T_WHILE, _), prev, _checkpoint) -> (
                  match Tokens.last' prev with
                  | None -> false
                  | Some ((T_SEMICOLON, _), prev, _checkpoint) -> (
                      match Tokens.last' prev with
                      | None -> false
                      | Some ((T_DO, _), _, _) -> true
                      | Some (_, _, _) -> false)
                  | Some ((T_RCURLY, _), prev, _checkpoint) -> (
                      match rewind [ T_RCURLY ] [] prev with
                      | None -> false
                      | Some (_, prev, _, _) -> (
                          match Tokens.last' prev with
                          | None -> false
                          | Some ((T_DO, _), _, _) -> true
                          | Some (_, _, _) -> false))
                  | Some (_, _, _) -> false)
              | Some (_, _, _) -> false)
        in
        let kind =
          match Tokens.last' prev with
          | None | Some ((T_VIRTUAL_SEMICOLON, _), _, _) -> `None
          (* contextually allowed as identifiers, namely await and yield; *)
          | Some ((((T_YIELD | T_AWAIT), _) as tok), rest, checkpoint)
            when I.acceptable checkpoint (fst (to_ident tok)) Lexer.dummy_pos ->
              `Replace (to_ident tok, rest, checkpoint)
          | Some (((T_RCURLY, _) as tok), rest, checkpoint)
            when I.acceptable checkpoint Js_token.T_VIRTUAL_SEMICOLON Lexer.dummy_pos ->
              `Semi_colon (tok, rest, checkpoint)
          | Some (((T_EOF, _) as tok), rest, checkpoint)
            when I.acceptable checkpoint Js_token.T_VIRTUAL_SEMICOLON Lexer.dummy_pos ->
              `Semi_colon (tok, rest, checkpoint)
          | Some (((T_ARROW, _) as tok), prev, checkpoint) when not (fol prev tok) ->
              `Arrow (tok, prev, checkpoint)
          | Some (last, rest, checkpoint) -> (
              match Tokens.last' rest with
              | Some ((T_VIRTUAL_SEMICOLON, _), _, _) -> `None
              | (Some _ | None)
                when fol rest last
                     && I.acceptable
                          checkpoint
                          Js_token.T_VIRTUAL_SEMICOLON
                          Lexer.dummy_pos -> `Semi_colon (last, rest, checkpoint)
              | Some ((T_RPAREN, _), rest, _)
                when end_of_do_whle rest
                     && I.acceptable
                          checkpoint
                          Js_token.T_VIRTUAL_SEMICOLON
                          Lexer.dummy_pos -> `Semi_colon (last, rest, checkpoint)
              | _ -> `None)
        in

        let drop_annot_or_error () =
          match Tokens.last' prev with
          | Some ((TAnnot (s, _), pos), prev, checkpoint) ->
              let t = Js_token.TComment s, pos in
              let prev = Tokens.add t checkpoint prev in
              loop prev buffer checkpoint
          | _ -> loop_error prev (I.resume checkpoint)
        in
        match kind with
        | `None -> drop_annot_or_error ()
        | `Arrow (tok, prev, _checkpoint) -> (
            (* Restart parsing from the openning parens, patching the
               token to be T_LPAREN_ARROW to help the parser *)
            let buffer = tok :: buffer in
            let err () = loop_error prev (I.resume checkpoint) in
            match Tokens.last' prev with
            | Some (((T_RPAREN, _) as tok), prev, _) -> (
                let buffer = tok :: buffer in
                match rewind [ T_RPAREN ] buffer prev with
                | None -> err ()
                | Some (loc, prev, buffer, checkpoint) ->
                    let buffer = (Js_token.T_LPAREN_ARROW, loc) :: buffer in
                    loop prev buffer checkpoint)
            | Some _ | None -> err ())
        | `Replace (t, prev, checkpoint) ->
            let checkpoint =
              let t, pos = t in
              I.offer checkpoint (t, fst pos, snd pos)
            in
            let prev = Tokens.add t checkpoint prev in
            loop prev buffer checkpoint
        | `Semi_colon (tok, prev, checkpoint) ->
            let buffer = tok :: buffer in
            let t = Js_token.T_VIRTUAL_SEMICOLON, (Lexer.dummy_pos, Lexer.dummy_pos) in
            let checkpoint =
              let t, pos = t in
              I.offer checkpoint (t, fst pos, snd pos)
            in
            let prev = Tokens.add t checkpoint prev in
            loop prev buffer checkpoint)
  in
  match loop Tokens.empty [] init with
  | `Ok x -> x
  | `Error toks ->
      let rec pi last =
        match Tokens.last' last with
        | None -> Parse_info.zero
        | Some ((_, (p, _)), rest, _) ->
            if Poly.(p = Lexer.dummy_pos) then pi rest else Parse_info.t_of_pos p
      in
      raise (Parsing_error (pi toks))

let fail_early =
  object
    inherit Js_traverse.iter

    method early_error p = raise (Parsing_error p.loc)
  end

let check_program p =
  List.iter p ~f:(function
      | `Annot _ -> ()
      | `Item p -> fail_early#program [ p ])

let parse' lex =
  let p, toks = parse_aux Js_parser.Incremental.program lex in
  check_program p;
  let groups =
    List.group p ~f:(fun a pred ->
        match pred, a with
        | `Item _, `Annot _ -> false
        | `Annot _, `Annot _ -> true
        | `Item _, `Item _ -> true
        | `Annot _, `Item _ -> true)
  in
  let p =
    List.map groups ~f:(fun g ->
        List.partition_map g ~f:(function
            | `Annot a -> `Fst a
            | `Item i -> `Snd i))
  in
  p, Tokens.all toks

let parse lex =
  let p, _ = parse_aux Js_parser.Incremental.program lex in
  check_program p;
  List.filter_map p ~f:(function
      | `Item i -> Some i
      | `Annot _ -> None)

let parse_expr lex =
  let expr, _ = parse_aux Js_parser.Incremental.standalone_expression lex in
  fail_early#expression expr;
  expr
