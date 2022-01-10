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

module Lexer = struct
  type t = Lexing.lexbuf

  let of_file file : t =
    let ic = open_in file in
    let lexbuf = Lexing.from_channel ic in
    { lexbuf with lex_curr_p = { lexbuf.lex_curr_p with pos_fname = file } }

  let of_channel ci : t = Lexing.from_channel ci

  let of_lexbuf lexbuf : t = lexbuf
end

exception Parsing_error of Parse_info.t

let parse_aux the_parser lexbuf =
  let init = the_parser lexbuf.Lexing.lex_start_p in
  let reset lexbuf =
    lexbuf.Lexing.lex_curr_p <- lexbuf.Lexing.lex_start_p;
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_start_pos
  in
  let fol prev (tok : Js_token.t) =
    match prev with
    | [] -> true
    | p :: _ -> (Js_token.info p).Parse_info.line <> (Js_token.info tok).Parse_info.line
  in
  let rec loop_error prev checkpoint =
    let module I = Js_parser.MenhirInterpreter in
    match checkpoint with
    | I.InputNeeded _env ->
        let checkpoint =
          I.offer
            checkpoint
            ( Js_token.EOF Parse_info.zero
            , lexbuf.Lexing.lex_curr_p
            , lexbuf.Lexing.lex_curr_p )
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
          | (Js_token.EOF _ as prev) :: _ -> prev, prev_with_comment
          | _ ->
              let rec read_one prev_with_comment lexbuf =
                match Js_lexer.main lexbuf with
                | TCommentLineDirective _ as tok ->
                    read_one (tok :: prev_with_comment) lexbuf
                | TComment (s, pi) as tok ->
                    if fol prev_with_comment tok
                    then
                      match parse_annot s with
                      | None -> read_one (tok :: prev_with_comment) lexbuf
                      | Some annot ->
                          let tok = Js_token.TAnnot (s, pi, annot) in
                          tok, prev_with_comment
                    else read_one (tok :: prev_with_comment) lexbuf
                | TAnnot _ -> assert false
                | t -> t, prev_with_comment
              in
              let t, prev_with_comment = read_one prev_with_comment lexbuf in
              let t =
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
                | ( (T_RETURN _ | T_CONTINUE _ | T_BREAK _ | T_THROW _) :: _
                  , ((T_SEMICOLON _ | T_VIRTUAL_SEMICOLON _) as t) ) -> t
                | (T_RETURN _ | T_CONTINUE _ | T_BREAK _ | T_THROW _) :: _, t
                  when fol prev t ->
                    reset lexbuf;
                    T_VIRTUAL_SEMICOLON Parse_info.zero
                (* The practical effect of these restricted productions is as follows:
                 * When a ++ or -- token is encountered where the parser would treat it
                 * as a postfix operator, and at least one LineTerminator occurred between
                 * the preceding token and the ++ or -- token, then a semicolon is automatically
                 * inserted before the ++ or -- token. *)
                | _, (T_DECR cpi as tok) when not (fol prev tok) -> Js_token.T_DECR_NB cpi
                | _, (T_INCR cpi as tok) when not (fol prev tok) -> Js_token.T_INCR_NB cpi
                | _, ((T_DIV _ | T_DIV_ASSIGN _) as tok) ->
                    if I.acceptable checkpoint tok lexbuf.Lexing.lex_start_p
                    then tok
                    else (
                      reset lexbuf;
                      Js_lexer.main_regexp lexbuf)
                | _, t -> t
              in
              t, prev_with_comment
        in
        let last_checkpoint = prev, prev_with_comment, inputneeded in
        let checkpoint =
          I.offer checkpoint (token, lexbuf.Lexing.lex_start_p, lexbuf.Lexing.lex_curr_p)
        in
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
          | [] | T_VIRTUAL_SEMICOLON _ :: _ -> false
          | T_RCURLY _ :: _ -> true
          | EOF _ :: _ -> true
          | offending :: before :: _ when fol [ before ] offending -> true
          | _ -> false
        in
        let drop_annot_or_error () =
          match prev with
          | TAnnot (s, i, _) :: _ ->
              let prev, prev_with_comment, checkpoint = last_checkpoint in
              let t = Js_token.TComment (s, i) in
              loop prev (t :: prev_with_comment) (last_checkpoint, checkpoint)
          | _ -> loop_error prev (I.resume checkpoint)
        in
        match insert_virtual_semmit with
        | false -> drop_annot_or_error ()
        | true ->
            let prev, prev_with_comment, checkpoint = last_checkpoint in
            if I.acceptable
                 checkpoint
                 (Js_token.T_VIRTUAL_SEMICOLON Parse_info.zero)
                 lexbuf.Lexing.lex_curr_p
            then (
              reset lexbuf;
              let t = Js_token.T_VIRTUAL_SEMICOLON Parse_info.zero in
              let checkpoint =
                I.offer checkpoint (t, lexbuf.Lexing.lex_curr_p, lexbuf.Lexing.lex_curr_p)
              in
              loop (t :: prev) (t :: prev_with_comment) (last_checkpoint, checkpoint))
            else drop_annot_or_error ())
  in
  match loop [] [] (([], [], init), init) with
  | `Ok x -> x
  | `Error tok ->
      let tok =
        match tok with
        | [] -> Js_token.EOF Parse_info.zero
        | x :: _ -> x
      in
      let pi = Js_token.info tok in
      raise (Parsing_error pi)

let parse' lex =
  let p, t_rev = parse_aux Js_parser.Incremental.program lex in
  p, List.rev t_rev

let parse lex =
  let p, _ = parse_aux Js_parser.Incremental.program lex in
  List.map p ~f:(fun (c, _) -> c)

let parse_expr lex =
  let expr, _ = parse_aux Js_parser.Incremental.standalone_expression lex in
  expr
