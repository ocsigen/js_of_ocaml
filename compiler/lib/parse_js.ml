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

let strip_comment l= List.filter (fun x -> not (Js_token.is_comment x)) l

let rec until_non_comment acc = function
  | [] -> acc,None
  | x::xs ->
    if Js_token.is_comment x
    then until_non_comment (x::acc) xs
    else (acc, Some (x,xs))

let adjust_tokens ?(keep_comment=true) l = match until_non_comment [] l with
  | acc,None when keep_comment -> List.rev acc
  | _,None -> []
  | past,Some (first,rest) ->
    let open Js_token in
    let f prev x acc = match prev, x with
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
      | (T_RETURN _ | T_CONTINUE _ | T_BREAK _ | T_THROW _),(T_SEMICOLON _ | T_VIRTUAL_SEMICOLON _) ->
        x::acc
      | (T_RETURN _ | T_CONTINUE _ | T_BREAK _ | T_THROW _),_ ->
        let x' = Js_token.info_of_tok x in
        let prev' = Js_token.info_of_tok prev in
        if prev'.Parse_info.line <> x'.Parse_info.line
        then x::(Js_token.T_VIRTUAL_SEMICOLON x')::acc
        else x::acc
      | _, _ -> x::acc in
    let rec aux prev acc = function
      | [] -> List.rev acc
      | e::l ->
        let nprev,nacc =
          if Js_token.is_comment e
          then if keep_comment then prev,(e::acc) else prev,acc
          else e,(f prev e acc) in
        aux nprev nacc l in
    let past = if keep_comment then past else [] in
    aux first (first::past) rest

type lexer = Js_token.token list

let lexer_aux ?(rm_comment=true) lines_info lexbuf =
  let rec loop lexbuf extra lines_info prev acc =
    let tokinfo lexbuf =
      let pi = Parse_info.t_of_lexbuf lines_info lexbuf in
      let pi = match prev with
        | None -> { pi with Parse_info.fol=Some true}
        | Some prev ->
          let prev_pi = Js_token.info_of_tok prev in
          if prev_pi.Parse_info.line <> pi.Parse_info.line
          then {pi with Parse_info.fol=Some true}
          else pi in
      match extra with
      | None -> pi
      | Some (file,offset) ->
         let src = Parse_info.relative_path lines_info file in
         { pi with Parse_info.
           src;
           name = Some file;
           line = pi.Parse_info.line - offset } in
    let t = Js_lexer.initial tokinfo prev lexbuf in
    match t with
      | Js_token.EOF _ -> List.rev acc
      | _ ->
        let extra = match t with
          | Js_token.TComment (ii,cmt) when String.length cmt > 1 && cmt.[0] = '#' ->
            let lexbuf = Lexing.from_string cmt in
            begin
              try
                let file,line = Js_lexer.pos lexbuf in
                match extra with
                | None -> Some (file, ii.Parse_info.line - ( line - 2))
                | Some (_,offset) -> Some (file, ii.Parse_info.line - (line - 2) + offset)
              with _ -> extra end
          | _ -> extra in
        let prev =
          if Js_token.is_comment t
          then prev
          else Some t in
        loop lexbuf extra lines_info prev (t::acc)
  in
  let toks = loop lexbuf None lines_info None [] in
  (* hack: adjust tokens *)
  adjust_tokens ~keep_comment:(not rm_comment) toks

let lexer_from_file ?rm_comment file : lexer =
  let lines_info = Parse_info.make_lineinfo_from_file file in
  let ic = open_in file in
  let lexbuf = Lexing.from_channel ic in
  let lexer = lexer_aux ?rm_comment lines_info lexbuf in
  close_in ic;
  lexer

let lexer_from_channel ?rm_comment ci : lexer =
  let lines_info,str = Parse_info.make_lineinfo_from_channel ci in
  let lexbuf = Lexing.from_string str in
  lexer_aux ?rm_comment lines_info lexbuf

let lexer_from_string ?rm_comment ?offset str : lexer =
  let lines_info = Parse_info.make_lineinfo_from_string ?offset str in
  let lexbuf = Lexing.from_string str in
  lexer_aux ?rm_comment lines_info lexbuf

let lexer_map = List.map
let lexer_fold f acc l = List.fold_left f acc l
let lexer_filter f l = List.filter f l
let lexer_from_list l = adjust_tokens l

exception Parsing_error of Parse_info.t

type st = {
  mutable rest : Js_token.token list;
  mutable current : Js_token.token ;
  mutable passed : Js_token.token list;
  mutable eof : bool }

let parse_aux the_parser toks =
  let state = match toks with
    | [] -> {
      rest = [];
      passed = [];
      current = Js_token.EOF Parse_info.zero;
      eof = false }
    | hd :: _ -> {
      rest = toks;
      passed = [];
      current = hd ;
      eof = false } in
  let lexer_fun _lb =
    match state.rest with
      | [] when not state.eof ->
        state.eof <- true;
        let info = Js_token.info_of_tok state.current in
        Js_token.EOF info
      | [] -> assert false
      | x::tl ->
        state.rest <- tl;
        state.current <- x;
        state.passed <- x::state.passed;
        x in
  let lexbuf = Lexing.from_string "" in
  try the_parser lexer_fun lexbuf
  with
    | Js_parser.Error
    | Parsing.Parse_error ->
      let pi = Js_token.info_of_tok state.current in
      raise (Parsing_error pi)

let parse lex = parse_aux Js_parser.program lex

let parse_expr lex = parse_aux Js_parser.standalone_expression lex
