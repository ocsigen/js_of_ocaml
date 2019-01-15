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

open Js_of_ocaml_compiler
open Js_of_ocaml_compiler.Stdlib

let error k = Format.ksprintf (fun s -> failwith s) k

let _ = Sys.catch_break true

let f {MinifyArg.common; output_file; use_stdin; files} =
  CommonArg.eval common;
  let chop_extension s = try Filename.chop_extension s with Invalid_argument _ -> s in
  let pp, finalize =
    match output_file with
    | Some "-" -> Pretty_print.to_out_channel stdout, fun _ -> ()
    | Some file ->
        let oc = open_out file in
        Pretty_print.to_out_channel oc, fun _ -> close_out oc
    | None when not use_stdin ->
        let file =
          if List.length files = 1
          then chop_extension (List.hd files) ^ ".min.js"
          else "a.min.js"
        in
        let oc = open_out file in
        Pretty_print.to_out_channel oc, fun _ -> close_out oc
    | None (* when stdin *) -> Pretty_print.to_out_channel stdout, fun _ -> ()
  in
  let pretty = Config.Flag.pretty () in
  Pretty_print.set_compact pp (not pretty);
  Code.Var.set_pretty pretty;
  let error_of_pi pi =
    match pi with
    | {Parse_info.name = Some src; line; col; _}
     |{Parse_info.src = Some src; line; col; _} ->
        error "error at file:%S l:%d col:%d" src line col
    | {Parse_info.line; col; _} -> error "error at l:%d col:%d" line col
  in
  let p =
    List.flatten
      (List.map files ~f:(fun file ->
           let lex = Parse_js.lexer_from_file file in
           try Parse_js.parse lex with Parse_js.Parsing_error pi -> error_of_pi pi ))
  in
  let p =
    if use_stdin
    then
      let lex = Parse_js.lexer_from_channel stdin in
      try p @ Parse_js.parse lex with Parse_js.Parsing_error pi -> error_of_pi pi
    else p
  in
  let free = new Js_traverse.free in
  let _pfree = free#program p in
  let toplevel_def = free#get_def_name in
  let () = VarPrinter.add_reserved (StringSet.elements toplevel_def) in
  let true_ () = true in
  let open Config in
  let passes : ((unit -> bool) * (unit -> Js_traverse.mapper)) list =
    [ ( Flag.shortvar
      , fun () -> (new Js_traverse.rename_variable toplevel_def :> Js_traverse.mapper) )
    ; (true_, fun () -> new Js_traverse.simpl)
    ; (true_, fun () -> new Js_traverse.clean) ]
  in
  let p =
    List.fold_left passes ~init:p ~f:(fun p (t, m) ->
        if t () then (m ())#program p else p )
  in
  let p = Js_assign.program p in
  Js_output.program pp p; finalize ()

let main = Cmdliner.Term.(pure f $ MinifyArg.options), MinifyArg.info

let _ =
  Timer.init Sys.time;
  try
    Cmdliner.Term.eval ~catch:false ~argv:(Util.normalize_argv ~warn_:true Sys.argv) main
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
