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
  type t = Js_token.t list

  let rec until_non_comment acc = function
    | [] -> acc, None
    | x :: xs ->
        if Js_token.is_comment x
        then until_non_comment (x :: acc) xs
        else acc, Some (x, xs)

  let adjust_tokens ?(keep_comment = true) l =
    match until_non_comment [] l with
    | acc, None when keep_comment -> List.rev acc
    | _, None -> []
    | past, Some (first, rest) ->
        let open Js_token in
        let f prev x acc =
          match prev, x with
          (* restricted productions *)
          (* 7.9.1 - 3 *)
          (* When, as the program is parsed from left to right, a token is encountered *)
          (* that is allowed by some production of the grammar, but the production *)
          (* is a restricted production and the token would be the first token for a *)
          (* terminal or nonterminal immediately following the annotation [no LineTerminator here] *)
          (* within the restricted production (and therefore such a token is called a restricted token), *)
          (* and the restricted token is separated from the previous token by at least *)
          (* one LineTerminator, then a semicolon is automatically inserted before the *)
          (* restricted token. *)
          | ( (T_RETURN _ | T_CONTINUE _ | T_BREAK _ | T_THROW _)
            , (T_SEMICOLON _ | T_VIRTUAL_SEMICOLON _) ) ->
              x :: acc
          | (T_RETURN _ | T_CONTINUE _ | T_BREAK _ | T_THROW _), _ ->
              let x' = Js_token.info x in
              let prev' = Js_token.info prev in
              if prev'.Parse_info.line <> x'.Parse_info.line
              then x :: Js_token.T_VIRTUAL_SEMICOLON x' :: acc
              else x :: acc
          | _, _ -> x :: acc
        in
        let rec aux prev acc = function
          | [] -> List.rev acc
          | e :: l ->
              let nprev, nacc =
                if Js_token.is_comment e
                then if keep_comment then prev, e :: acc else prev, acc
                else e, f prev e acc
              in
              aux nprev nacc l
        in
        let past = if keep_comment then past else [] in
        aux first (first :: past) rest

  let lexer_aux ?(rm_comment = true) lexbuf =
    let rec loop lexbuf ~prev acc =
      let t = Js_lexer.main prev lexbuf in
      match t with
      | Js_token.EOF _ -> List.rev acc
      | _ ->
          let prev = if Js_token.is_comment t then prev else t :: prev in
          loop lexbuf ~prev (t :: acc)
    in
    let toks = loop lexbuf ~prev:[] [] in
    (* hack: adjust tokens *)
    adjust_tokens ~keep_comment:(not rm_comment) toks

  let of_file ?rm_comment file : t =
    let ic = open_in file in
    let lexbuf = Lexing.from_channel ic in
    let lexbuf =
      { lexbuf with lex_curr_p = { lexbuf.lex_curr_p with pos_fname = file } }
    in
    let lexer = lexer_aux ?rm_comment lexbuf in
    close_in ic;
    lexer

  let of_channel ?rm_comment ci : t =
    let lexbuf = Lexing.from_channel ci in
    lexer_aux ?rm_comment lexbuf

  let of_lexbuf ?rm_comment lexbuf : t = lexer_aux ?rm_comment lexbuf

  let fold ~f ~init l = List.fold_left ~f ~init l

  let of_list l = adjust_tokens l
end

exception Parsing_error of Parse_info.t

let parse_aux the_parser toks =
  let state = ref (Js_token.TUnknown ("", Parse_info.zero) :: toks) in
  let lexer_fun _lb =
    match !state with
    | [] -> assert false
    | [ last ] ->
        let info = Js_token.info last in
        Js_token.EOF info
    | _prev :: (curr :: _ as rest) ->
        state := rest;
        curr
  in
  let lexbuf = Lexing.from_string "" in
  try the_parser lexer_fun lexbuf
  with Js_parser.Error | Parsing.Parse_error ->
    let pi =
      match !state with
      | [] -> assert false
      | top :: _ -> Js_token.info top
    in
    raise (Parsing_error pi)

let parse lex = parse_aux Js_parser.program lex

let parse_expr lex = parse_aux Js_parser.standalone_expression lex
