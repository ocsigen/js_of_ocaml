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

let info_of_tok t =
  let open Js_parser in
      match t with
  | TUnknown ii -> ii
  | TCommentSpace (ii,_) -> ii
  | TCommentNewline (ii,_) -> ii
  | TComment (ii,_) -> ii
  | EOF ii -> ii

  | T_NUMBER (s, _,ii) -> ii
  | T_IDENTIFIER (s, ii) -> ii
  | T_STRING (s, ii) -> ii
  | T_REGEX (s, ii) -> ii

  | T_FUNCTION ii -> ii
  | T_IF ii -> ii
  | T_IN ii -> ii
  | T_INSTANCEOF ii -> ii
  | T_RETURN ii -> ii
  | T_SWITCH ii -> ii
  | T_THIS ii -> ii
  | T_THROW ii -> ii
  | T_TRY ii -> ii
  | T_VAR ii -> ii
  | T_WHILE ii -> ii
  | T_WITH ii -> ii
  | T_NULL ii -> ii
  | T_FALSE ii -> ii
  | T_TRUE ii -> ii
  | T_BREAK ii -> ii
  | T_CASE ii -> ii
  | T_CATCH ii -> ii
  | T_CONTINUE ii -> ii
  | T_DEFAULT ii -> ii
  | T_DO ii -> ii
  | T_FINALLY ii -> ii
  | T_FOR ii -> ii
  | T_ELSE ii -> ii
  | T_NEW ii -> ii
  | T_LCURLY ii -> ii
  | T_RCURLY ii -> ii
  | T_LPAREN ii -> ii
  | T_RPAREN ii -> ii
  | T_LBRACKET ii -> ii
  | T_RBRACKET ii -> ii
  | T_SEMICOLON ii -> ii
  | T_COMMA ii -> ii
  | T_PERIOD ii -> ii
  | T_RSHIFT3_ASSIGN ii -> ii
  | T_RSHIFT_ASSIGN ii -> ii
  | T_LSHIFT_ASSIGN ii -> ii
  | T_BIT_XOR_ASSIGN ii -> ii
  | T_BIT_OR_ASSIGN ii -> ii
  | T_BIT_AND_ASSIGN ii -> ii
  | T_MOD_ASSIGN ii -> ii
  | T_DIV_ASSIGN ii -> ii
  | T_MULT_ASSIGN ii -> ii
  | T_MINUS_ASSIGN ii -> ii
  | T_PLUS_ASSIGN ii -> ii
  | T_ASSIGN ii -> ii
  | T_PLING ii -> ii
  | T_COLON ii -> ii
  | T_OR ii -> ii
  | T_AND ii -> ii
  | T_BIT_OR ii -> ii
  | T_BIT_XOR ii -> ii
  | T_BIT_AND ii -> ii
  | T_EQUAL ii -> ii
  | T_NOT_EQUAL ii -> ii
  | T_STRICT_EQUAL ii -> ii
  | T_STRICT_NOT_EQUAL ii -> ii
  | T_LESS_THAN_EQUAL ii -> ii
  | T_GREATER_THAN_EQUAL ii -> ii
  | T_LESS_THAN ii -> ii
  | T_GREATER_THAN ii -> ii
  | T_LSHIFT ii -> ii
  | T_RSHIFT ii -> ii
  | T_RSHIFT3 ii -> ii
  | T_PLUS ii -> ii
  | T_MINUS ii -> ii
  | T_DIV ii -> ii
  | T_MULT ii -> ii
  | T_MOD ii -> ii
  | T_NOT ii -> ii
  | T_BIT_NOT ii -> ii
  | T_INCR ii -> ii
  | T_DECR ii -> ii
  | T_DELETE ii -> ii
  | T_TYPEOF ii -> ii
  | T_VOID ii -> ii
  | T_VIRTUAL_SEMICOLON ii -> ii


let string_of_tok t =
  let open Js_parser in
      match t with
  | TUnknown ii -> "COMMENT"
  | TCommentSpace (ii,_) -> "COMMENT"
  | TCommentNewline (ii,_) -> "COMMENT"
  | TComment (ii,_) -> "COMMENT"
  | EOF ii -> "EOF"

  | T_NUMBER (s, _,ii) -> "T_NUMBER"
  | T_IDENTIFIER (s, ii) -> "T_IDENTIFIER"
  | T_STRING (s, ii) -> "T_STRING"
  | T_REGEX (s, ii) -> "T_REGEX"

  | T_FUNCTION ii -> " T_FUNCTION"
  | T_IF ii -> "T_IF"
  | T_IN ii -> "T_IN"
  | T_INSTANCEOF ii -> "T_INSTANCEOF"
  | T_RETURN ii -> "T_RETURN"
  | T_SWITCH ii -> "T_SWITCH"
  | T_THIS ii -> "T_THIS"
  | T_THROW ii -> "T_THROW"
  | T_TRY ii -> "T_TRY"
  | T_VAR ii -> "T_VAR"
  | T_WHILE ii -> "T_WHILE"
  | T_WITH ii -> "T_WITH"
  | T_NULL ii -> "T_NULL"
  | T_FALSE ii -> "T_FALSE"
  | T_TRUE ii -> "T_TRUE"
  | T_BREAK ii -> "T_BREAK"
  | T_CASE ii -> "T_CASE"
  | T_CATCH ii -> "T_CATCH"
  | T_CONTINUE ii -> "T_CONTINUE"
  | T_DEFAULT ii -> "T_DEFAULT"
  | T_DO ii -> "T_DO"
  | T_FINALLY ii -> "T_FINALLY"
  | T_FOR ii -> "T_FOR"
  | T_ELSE ii -> "T_ELSE"
  | T_NEW ii -> "T_NEW"
  | T_LCURLY ii -> "T_LCURLY"
  | T_RCURLY ii -> "T_RCURLY"
  | T_LPAREN ii -> "T_LPAREN"
  | T_RPAREN ii -> "T_RPAREN"
  | T_LBRACKET ii -> "T_LBRACKET"
  | T_RBRACKET ii -> "T_RBRACKET"
  | T_SEMICOLON ii -> "T_SEMICOLON"
  | T_COMMA ii -> "T_COMMA"
  | T_PERIOD ii -> "T_PERIOD"
  | T_RSHIFT3_ASSIGN ii -> "T_RSHIFT3"
  | T_RSHIFT_ASSIGN ii -> "T_RSHIFT"
  | T_LSHIFT_ASSIGN ii -> "T_LSHIFT"
  | T_BIT_XOR_ASSIGN ii -> "T_BIT"
  | T_BIT_OR_ASSIGN ii -> "T_BIT"
  | T_BIT_AND_ASSIGN ii -> "T_BIT"
  | T_MOD_ASSIGN ii -> "T_MOD"
  | T_DIV_ASSIGN ii -> "T_DIV"
  | T_MULT_ASSIGN ii -> "T_MULT"
  | T_MINUS_ASSIGN ii -> "T_MINUS"
  | T_PLUS_ASSIGN ii -> "T_PLUS"
  | T_ASSIGN ii -> "T_ASSIGN"
  | T_PLING ii -> "T_PLING"
  | T_COLON ii -> "T_COLON"
  | T_OR ii -> "T_OR"
  | T_AND ii -> "T_AND"
  | T_BIT_OR ii -> "T_BIT"
  | T_BIT_XOR ii -> "T_BIT"
  | T_BIT_AND ii -> "T_BIT"
  | T_EQUAL ii -> "T_EQUAL"
  | T_NOT_EQUAL ii -> "T_NOT"
  | T_STRICT_EQUAL ii -> "T_STRICT"
  | T_STRICT_NOT_EQUAL ii -> "T_STRICT"
  | T_LESS_THAN_EQUAL ii -> "T_LESS"
  | T_GREATER_THAN_EQUAL ii -> "T_GREATER"
  | T_LESS_THAN ii -> "T_LESS"
  | T_GREATER_THAN ii -> "T_GREATER"
  | T_LSHIFT ii -> "T_LSHIFT"
  | T_RSHIFT ii -> "T_RSHIFT"
  | T_RSHIFT3 ii -> "T_RSHIFT3"
  | T_PLUS ii -> "T_PLUS"
  | T_MINUS ii -> "T_MINUS"
  | T_DIV ii -> "T_DIV"
  | T_MULT ii -> "T_MULT"
  | T_MOD ii -> "T_MOD"
  | T_NOT ii -> "T_NOT"
  | T_BIT_NOT ii -> "T_BIT"
  | T_INCR ii -> "T_INCR"
  | T_DECR ii -> "T_DECR"
  | T_DELETE ii -> "T_DELETE"
  | T_TYPEOF ii -> "T_TYPEOF"
  | T_VOID ii -> "T_VOID"
  | T_VIRTUAL_SEMICOLON ii -> "T_VIRTUAL"


let is_comment = function
  | Js_parser.TCommentSpace _
  | Js_parser.TCommentNewline _
  | Js_parser.TComment _ -> true
  | _ -> false

let strip_comment l= List.filter (fun x -> not (is_comment x)) l

let push v l = l := v :: !l
let pop l =
  let v = List.hd !l in
  l := List.tl !l;
  v

let iter_with_previous_opt f = function
  | [] -> ()
  | e::l ->
    f None e;
    let rec iter_with_previous_ previous = function
      | [] -> ()
      | e::l -> f (Some previous) e ; iter_with_previous_ e l
    in iter_with_previous_ e l

let rparens_of_if toks =
  let open Js_parser in
  let toks = strip_comment toks in
  let stack = ref [] in
  let rparens_if = ref [] in
  iter_with_previous_opt (fun prev x ->
    (match x with
      | T_LPAREN _ -> push prev stack;
      | T_RPAREN info ->
        if !stack <> []
        then begin
          match pop stack with
            | Some (T_IF _) -> push info rparens_if
            | _ -> ()
        end
      | _ -> ()
    )
  ) toks;
  !rparens_if

let auto_semi_if_newline x prev res =
  let x' = info_of_tok x in
  let prev' = info_of_tok prev in
  if prev'.Parse_info.line <> x'.Parse_info.line
  then push (Js_parser.T_VIRTUAL_SEMICOLON x') res
  else ();
  push x res

let rec adjust_tokens xs =
  let open Js_parser in
  let rparens_if = rparens_of_if xs in
  let hrparens_if =
    let h = Hashtbl.create 101 in
    List.iter (fun s -> Hashtbl.add h s true) rparens_if;
    h in

  match xs with
    | [] -> []
    | y::ys ->
      let res = ref [] in
      push y res;
      let rec aux prev f = function
        | [] -> ()
        | e::l ->
          if is_comment e
          then (push e res;aux prev f l)
          else (f prev e;aux e f l) in
      let f = (fun prev x ->
        match prev, x with
          (* 7.9.1 - 1 *)
          (* When, as the program is parsed from left to right, a token (called the offending token)
             is encountered that is not allowed by any production of the grammar, then a semicolon
             is automatically inserted before the offending token if one or more of the following
             conditions is true:
             - The offending token is }. *)
          | (T_LCURLY _ | T_SEMICOLON _ | T_VIRTUAL_SEMICOLON _),T_RCURLY _ ->
            push x res
          | _, T_RCURLY fake ->
            push (T_VIRTUAL_SEMICOLON fake) res;
            push x res

          (* - The offending token is separated from the previous *)
          (* token by at least one LineTerminator. (TODO/FIXME) *)
          | T_RCURLY _,
            ( T_IDENTIFIER _ | T_THIS _
            | T_IF _ | T_VAR _ | T_FOR _
            | T_RETURN _ | T_BREAK _
            | T_SWITCH _ | T_FUNCTION _ | T_NEW _ ) ->
            auto_semi_if_newline x prev res
          (* this is valid only if the RPAREN is not the closing paren
           * of a if *)
          | T_RPAREN info,
            ( T_VAR _
            | T_IF _ | T_ELSE _ | T_FOR _
            | T_THIS _ | T_IDENTIFIER _
            | T_RETURN _ | T_CONTINUE _
            ) when not (Hashtbl.mem hrparens_if info)  ->
            auto_semi_if_newline x prev res
          | T_RBRACKET _,
            (T_FOR _ | T_IF _ | T_VAR _ | T_IDENTIFIER _) ->
            auto_semi_if_newline x prev res
          | ( T_IDENTIFIER _
            | T_NULL _
            | T_STRING _ | T_REGEX _
            | T_FALSE _ | T_TRUE _ | T_NUMBER _),
            ( T_VAR _
            | T_IDENTIFIER _
            | T_IF _
            | T_THIS _
            | T_RETURN _
            | T_BREAK _
            | T_ELSE _
            | T_NUMBER _ )  ->
            auto_semi_if_newline x prev res

          (* 7.9.1 - 2 *)
          (* When, as the program is parsed from left to right, the end of the input stream of tokens *)
          (* is encountered and the parser is unable to parse the input token stream as a single *)
          (* complete ECMAScript Program, then a semicolon is automatically inserted at the end *)
          (* of the input stream. *)
          | (T_SEMICOLON _ | T_VIRTUAL_SEMICOLON _), EOF _ ->
            push x res
          | _, EOF fake ->
            push (T_VIRTUAL_SEMICOLON fake) res;
            push x res

          (*restricted productions *)
          (* 7.9.1 - 3 *)
          (* When, as the program is parsed from left to right, a token is encountered
             that is allowed by some production of the grammar, but the production
             is a restricted production and the token would be the first token for a
             terminal or nonterminal immediately following the annotation [no LineTerminator here]
             within the restricted production (and therefore such a token is called a restricted token),
             and the restricted token is separated from the previous token by at least
             one LineTerminator, then a semicolon is automatically inserted before the
             restricted token.*)
          | (T_RETURN _ | T_CONTINUE _ | T_BREAK _ | T_THROW _),(T_SEMICOLON _ | T_VIRTUAL_SEMICOLON _) ->
            push x res
          | (T_RETURN _ | T_CONTINUE _ | T_BREAK _ | T_THROW _),_ ->
            auto_semi_if_newline x prev res
          | (T_LPAREN _ | T_LCURLY _ | T_LBRACKET _
            |T_SEMICOLON _ | T_VIRTUAL_SEMICOLON _),
            (T_INCR _ | T_DECR _) ->
            push x res
          | _, (T_INCR _ | T_DECR _) ->
            auto_semi_if_newline x prev res

          | _, _ -> push x res
      )
      in
      aux y f ys;
      List.rev !res

type st = {
  mutable rest : Js_parser.token list;
  mutable current : Js_parser.token ;
  mutable passed : Js_parser.token list }

type lexer = Js_parser.token list
let lexer_aux ?(rm_comment=true) lines_info lexbuf =
  let tokinfo = Parse_info.t_of_lexbuf lines_info in
  let rec loop lexbuf lines_info prev acc =
    let t = Js_lexer.initial tokinfo prev lexbuf in
    match t with
      | Js_parser.EOF _ -> List.rev (t::acc)
      | _ ->
        let prev =
          if is_comment t
          then prev
          else Some t in
        loop lexbuf lines_info prev (t::acc)
  in
  let toks = loop lexbuf lines_info None [] in
  (* hack: adjust tokens *)
  let toks = adjust_tokens toks in
  (* remove comments *)
  if rm_comment
  then strip_comment toks
  else toks

let lexer_from_file ?rm_comment file : lexer =
  let lines_info = Parse_info.make_lineinfo_from_file file in
  let ic = open_in file in
  let lexbuf = Lexing.from_channel ic in
  lexer_aux ?rm_comment lines_info lexbuf

let lexer_from_channel ?rm_comment ci : lexer =
  let lines_info,str = Parse_info.make_lineinfo_from_channel ci in
  let lexbuf = Lexing.from_string str in
  lexer_aux ?rm_comment lines_info lexbuf

let lexer_from_string ?rm_comment str : lexer =
  let lines_info = Parse_info.make_lineinfo_from_string str in
  let lexbuf = Lexing.from_string str in
  lexer_aux ?rm_comment lines_info lexbuf

let lexer_map = List.map
let lexer_fold f acc l = List.fold_left f acc l
let lexer_filter f l = List.filter f l
let lexer_from_list x =
  let rec rm_virtual = function
    | Js_parser.T_VIRTUAL_SEMICOLON _ ::xs -> rm_virtual xs
    | l -> l in
  let x = rm_virtual x in

  let l = match List.rev x with
    | Js_parser.EOF _ :: _  -> x
    | (last::_) as all -> List.rev (Js_parser.EOF (info_of_tok last) :: all)
    | [] -> raise (Invalid_argument "lexer_from_list; empty list") in
  adjust_tokens l

exception Parsing_error of Parse_info.t

let parse toks =
  let state = {
    rest = toks;
    passed = [];
    current = List.hd toks
  }
  in
  let lexer_fun lb =
    match state.rest with
      | [] -> assert false
      | x::tl ->
        state.rest <- tl;
        state.current <- x;
        state.passed <- x::state.passed;
        x in
  let lexbuf = Lexing.from_string "" in
  try Js_parser.program lexer_fun lexbuf
  with
    | Js_parser.Error
    | Parsing.Parse_error ->
      let pi = info_of_tok state.current in
      raise (Parsing_error pi)
