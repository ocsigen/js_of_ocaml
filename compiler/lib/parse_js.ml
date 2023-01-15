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

  val stash : t -> unit

  val rollback : t -> unit

  val token : t -> Js_token.t * (Lexing.position * Lexing.position)

  val regexp : t -> Js_token.t * (Lexing.position * Lexing.position)

  val dummy_pos : Lexing.position
end = struct
  type elt = Js_token.t * (Lexing.position * Lexing.position) * Flow_lexer.Lex_env.t

  type t =
    { l : Sedlexing.lexbuf
    ; mutable env : Flow_lexer.Lex_env.t
    ; mutable curr : elt option
    ; mutable stashed : elt list
    }

  let dummy_pos = { Lexing.pos_fname = ""; pos_lnum = 0; pos_cnum = 0; pos_bol = 0 }

  let zero_pos = { Lexing.pos_fname = ""; pos_lnum = 1; pos_cnum = 0; pos_bol = 0 }

  let create l = { l; env = Flow_lexer.Lex_env.create l; curr = None; stashed = [] }

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

  let token (t : t) =
    match t.stashed with
    | [] ->
        let env, res = Flow_lexer.token t.env in
        t.env <- env;
        let tok = Flow_lexer.Lex_result.token res in
        let pos = Flow_lexer.Lex_result.loc res in
        let c = tok, pos, env in
        t.curr <- Some c;
        tok, pos
    | ((tok, pos, env) as c) :: xs ->
        t.stashed <- xs;
        t.env <- env;
        t.curr <- Some c;
        tok, pos

  let regexp (t : t) =
    match t.stashed with
    | [] ->
        let env, res = Flow_lexer.regexp t.env in
        t.env <- env;
        let tok = Flow_lexer.Lex_result.token res in
        let pos = Flow_lexer.Lex_result.loc res in
        let c = tok, pos, env in
        t.curr <- Some c;
        tok, pos
    | ((tok, pos, env) as c) :: xs ->
        t.stashed <- xs;
        t.env <- env;
        t.curr <- Some c;
        tok, pos

  let rollback (t : t) = Sedlexing.rollback t.l

  let stash (t : t) =
    match t.curr with
    | None -> ()
    | Some (tok, p, env) -> t.stashed <- (tok, p, env) :: t.stashed
end

exception Parsing_error of Parse_info.t

let parse_aux the_parser (lexbuf : Lexer.t) =
  let init = the_parser (Lexer.curr_pos lexbuf) in
  let fol prev (_, (c, _)) =
    match prev with
    | [] -> true
    (* line_comment end_position is on the next line, FIXME *)
    | (_, (p, _)) :: _ -> c.Lexing.pos_lnum <> p.Lexing.pos_lnum
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
  let rec loop prev prev_with_comment (last_checkpoint, checkpoint) =
    let module I = Js_parser.MenhirInterpreter in
    match checkpoint with
    | I.InputNeeded _env ->
        let inputneeded = checkpoint in
        let token, prev_with_comment =
          match prev with
          | ((Js_token.T_EOF, _) as prev) :: _ -> prev, prev_with_comment
          | _ ->
              let rec read_one prev_with_comment (lexbuf : Lexer.t) =
                match Lexer.token lexbuf with
                | (TCommentLineDirective _ as tok), pos ->
                    read_one ((tok, pos) :: prev_with_comment) lexbuf
                | (TComment s as tok), pos ->
                    if fol prev_with_comment (tok, pos)
                    then
                      match parse_annot s with
                      | None -> read_one ((tok, pos) :: prev_with_comment) lexbuf
                      | Some annot ->
                          let tok = Js_token.TAnnot (s, annot) in
                          (tok, pos), prev_with_comment
                    else read_one ((tok, pos) :: prev_with_comment) lexbuf
                | TAnnot _, _pos -> assert false
                | t, pos -> (t, pos), prev_with_comment
              in
              let t, prev_with_comment = read_one prev_with_comment lexbuf in
              let t, pos =
                match prev, t with
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
                | ( ((T_RETURN | T_CONTINUE | T_BREAK | T_THROW), _) :: _
                  , (((T_SEMICOLON | T_VIRTUAL_SEMICOLON), _) as t) ) -> t
                | ((T_RETURN | T_CONTINUE | T_BREAK | T_THROW), _) :: _, t when fol prev t
                  ->
                    Lexer.stash lexbuf;
                    T_VIRTUAL_SEMICOLON, (Lexer.dummy_pos, Lexer.dummy_pos)
                (* The practical effect of these restricted productions is as follows:
                 * When a ++ or -- token is encountered where the parser would treat it
                 * as a postfix operator, and at least one LineTerminator occurred between
                 * the preceding token and the ++ or -- token, then a semicolon is automatically
                 * inserted before the ++ or -- token. *)
                | _, ((T_DECR, pos) as tok) when not (fol prev tok) ->
                    Js_token.T_DECR_NB, pos
                | _, ((T_INCR, pos) as tok) when not (fol prev tok) ->
                    Js_token.T_INCR_NB, pos
                | _, ((((T_DIV | T_DIV_ASSIGN) as t), ((start_pos, _) as _pos)) as tok) ->
                    if I.acceptable checkpoint t start_pos
                    then tok
                    else (
                      Lexer.rollback lexbuf;
                      let t, pos = Lexer.regexp lexbuf in
                      t, pos)
                | _, t -> t
              in
              (t, pos), prev_with_comment
        in
        let last_checkpoint = prev, prev_with_comment, inputneeded in
        let t, pos = token in
        let checkpoint = I.offer checkpoint (t, fst pos, snd pos) in
        loop (token :: prev) (token :: prev_with_comment) (last_checkpoint, checkpoint)
    | I.Shifting _ | I.AboutToReduce _ ->
        loop prev prev_with_comment (last_checkpoint, I.resume checkpoint)
    | I.Accepted v -> `Ok (v, prev_with_comment)
    | I.Rejected -> `Error prev
    | I.HandlingError _ -> (
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
        let insert_virtual_semmit =
          match prev with
          | [] | (T_VIRTUAL_SEMICOLON, _) :: _ -> false
          | (T_RCURLY, _) :: _ -> true
          | (T_EOF, _) :: _ -> true
          | offending :: before :: _ when fol [ before ] offending -> true
          | _ -> false
        in
        let drop_annot_or_error () =
          match prev with
          | (TAnnot (s, _), pos) :: _ ->
              let prev, prev_with_comment, checkpoint = last_checkpoint in
              let t = Js_token.TComment s in
              loop prev ((t, pos) :: prev_with_comment) (last_checkpoint, checkpoint)
          | _ -> loop_error prev (I.resume checkpoint)
        in
        match insert_virtual_semmit with
        | false -> drop_annot_or_error ()
        | true ->
            let prev, prev_with_comment, checkpoint = last_checkpoint in
            if I.acceptable
                 checkpoint
                 Js_token.T_VIRTUAL_SEMICOLON
                 (Lexer.curr_pos lexbuf)
            then (
              Lexer.stash lexbuf;
              let t = Js_token.T_VIRTUAL_SEMICOLON, (Lexer.dummy_pos, Lexer.dummy_pos) in

              let checkpoint =
                let t, pos = t in
                I.offer checkpoint (t, fst pos, snd pos)
              in
              loop (t :: prev) (t :: prev_with_comment) (last_checkpoint, checkpoint))
            else drop_annot_or_error ())
  in
  match loop [] [] (([], [], init), init) with
  | `Ok x -> x
  | `Error tok ->
      let pi =
        match tok with
        | [] -> Parse_info.zero
        | (_, (p, _)) :: _ -> Parse_info.t_of_pos p
      in
      raise (Parsing_error pi)

let parse' lex =
  let p, t_rev = parse_aux Js_parser.Incremental.program lex in
  p, List.rev_map t_rev ~f:(fun (t, (p, _)) -> t, Parse_info.t_of_pos p)

let parse lex =
  let p, _ = parse_aux Js_parser.Incremental.program lex in
  List.map p ~f:(fun (c, _) -> c)

let parse_expr lex =
  let expr, _ = parse_aux Js_parser.Incremental.standalone_expression lex in
  expr
