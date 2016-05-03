(****************************************************************************)
(*                                                                          *)
(*                                   OCaml                                  *)
(*                                                                          *)
(*                            INRIA Rocquencourt                            *)
(*                                                                          *)
(*  Copyright 2002-2006 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed under   *)
(*  the terms of the GNU Library General Public License, with the special   *)
(*  exception on linking described in LICENSE at the top of the Camlp4      *)
(*  source tree.                                                            *)
(*                                                                          *)
(****************************************************************************)
open Camlp4

open PreCast

open Syntax

open Camlp4.Sig

module Ast2pt = Camlp4.Struct.Camlp4Ast2OCamlAst.Make(Ast)

module Lexer = Camlp4.Struct.Lexer.Make(Token)

external not_filtered : 'a -> 'a Gram.not_filtered = "%identity"

let wrap parse_fun lb =
  let () = Register.iter_and_take_callbacks (fun (_, f) -> f ()) in
  let not_filtered_token_stream = Lexer.from_lexbuf lb in
  try
    let token_stream = Gram.filter (not_filtered not_filtered_token_stream)
    in
    let (__strm : _ Stream.t) = token_stream
    in
    match Stream.peek __strm with
    | Some ((EOI, _)) -> (Stream.junk __strm; raise End_of_file)
    | _ -> parse_fun token_stream
  with
  | ((End_of_file | Sys.Break) as x)
  | Loc.Exc_located (_, (End_of_file | Sys.Break as x)) ->
    raise x
  | x ->
    let x,exc =
      match x with
      | Loc.Exc_located (loc, x) ->
        let loc = Loc.to_ocaml_location loc in
        Toploop.print_location Format.err_formatter loc;
        x, JsooTopError.Camlp4 (loc, x)
      | x ->
        x, x
    in
    (Format.eprintf "@[<0>%a@]@." Camlp4.ErrorHandler.print x;
     raise exc)

let toplevel_phrase token_stream =
  match Gram.parse_tokens_after_filter Syntax.top_phrase token_stream with
  | Some str_item ->
    let str_item =
      AstFilters.fold_topphrase_filters (fun t filter -> filter t) str_item
    in Ast2pt.phrase str_item
  | None -> raise End_of_file

let use_file token_stream =
  let (pl0, eoi) =
    let rec loop () =
      let (pl, stopped_at_directive) =
        Gram.parse_tokens_after_filter Syntax.use_file token_stream
      in
        if stopped_at_directive <> None
        then
          (match pl with
           | [ Ast.StDir (_, "load", (Ast.ExStr (_, s))) ] ->
               (Topdirs.dir_load Format.std_formatter s; loop ())
           | [ Ast.StDir (_, "directory", (Ast.ExStr (_, s))) ] ->
               (Topdirs.dir_directory s; loop ())
           | _ -> (pl, false))
        else (pl, true)
    in loop () in
  let pl =
    if eoi
    then []
    else
      (let rec loop () =
         let (pl, stopped_at_directive) =
           Gram.parse_tokens_after_filter Syntax.use_file token_stream
         in if stopped_at_directive <> None then pl @ (loop ()) else pl
       in loop ())
  in List.map Ast2pt.phrase (pl0 @ pl)

let _ = Toploop.parse_toplevel_phrase := wrap toplevel_phrase

let _ = Toploop.parse_use_file := wrap use_file

let _ =
  current_warning :=
    fun loc txt ->
      Toploop.print_warning (Loc.to_ocaml_location loc) Format.err_formatter
        (Warnings.Preprocessor txt)

let _ = Register.iter_and_take_callbacks (fun (_, f) -> f ())
