(* Js_of_ocaml library
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2014 Hugo Heuzard
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
        x, Js_of_ocaml_toplevel.For_js_of_ocaml_toplevel_camlp4.camlp4_exception (loc, x)
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
