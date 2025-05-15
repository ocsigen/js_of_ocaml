(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2013 Hugo Heuzard
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

open! Js_of_ocaml_compiler.Stdlib
open Js_of_ocaml_compiler

let error k = Format.ksprintf (fun s -> failwith s) k

let () = Sys.catch_break true

let f { Cmd_arg.common; output_file; use_stdin; files } =
  Jsoo_cmdline.Arg.eval common;
  let chop_extension s = try Filename.chop_extension s with Invalid_argument _ -> s in
  let with_output f =
    match output_file with
    | Some "-" -> f stdout
    | None when use_stdin -> f stdout
    | None | Some _ ->
        let file =
          match output_file with
          | Some f -> f
          | None ->
              if List.length files = 1
              then chop_extension (List.hd files) ^ ".min.js"
              else "a.min.js"
        in
        Filename.gen_file file f
  in
  let gen pp =
    let pretty = Config.Flag.pretty () in
    Pretty_print.set_compact pp (not pretty);
    let error_of_pi pi =
      match pi with
      | { Parse_info.name = Some src; line; col; _ }
      | { Parse_info.src = Some src; line; col; _ } ->
          error "error at file:%S l:%d col:%d" src line col
      | { Parse_info.src = None; name = None; line; col; _ } ->
          error "error at l:%d col:%d" line col
    in
    let p =
      List.flatten
        (List.map files ~f:(fun file ->
             let lex = Parse_js.Lexer.of_file file in
             try Parse_js.parse lex with Parse_js.Parsing_error pi -> error_of_pi pi))
    in
    let p =
      if use_stdin
      then
        let lex = Parse_js.Lexer.of_channel stdin in
        try p @ Parse_js.parse lex with Parse_js.Parsing_error pi -> error_of_pi pi
      else p
    in
    let true_ () = true in
    let open Config in
    let passes : ((unit -> bool) * (unit -> Js_traverse.mapper)) list =
      [ ( Flag.shortvar
        , fun () -> (new Js_traverse.rename_variable ~esm:false :> Js_traverse.mapper) )
      ; (true_, fun () -> new Js_traverse.simpl)
      ; (true_, fun () -> new Js_traverse.clean)
      ]
    in
    let p =
      List.fold_left passes ~init:p ~f:(fun p (t, m) ->
          if t () then (m ())#program p else p)
    in
    let p = Js_assign.program p in
    let (_ : Source_map.info) = Js_output.program pp p in
    ()
  in
  with_output (fun out_channel ->
      let pp = Pretty_print.to_out_channel out_channel in
      gen pp)

let main =
  let t = Cmdliner.Term.(const f $ Cmd_arg.options) in
  Cmdliner.Cmd.v Cmd_arg.info t

let (_ : int) =
  try
    Cmdliner.Cmd.eval
      ~catch:false
      ~argv:(Jsoo_cmdline.normalize_argv ~warn:(warn "%s") Sys.argv)
      main
  with
  | (Match_failure _ | Assert_failure _ | Not_found) as exc ->
      let backtrace = Printexc.get_backtrace () in
      Format.eprintf
        "%s: You found a bug. Please report it at \
         https://github.com/ocsigen/js_of_ocaml/issues :@."
        Sys.argv.(0);
      Format.eprintf "Error: %s@." (Printexc.to_string exc);
      prerr_string backtrace;
      exit 1
  | Failure s ->
      Format.eprintf "%s: Error: %s@." Sys.argv.(0) s;
      exit 1
  | exc ->
      Format.eprintf "%s: Error: %s@." Sys.argv.(0) (Printexc.to_string exc);
      exit 1
