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


let error k = Format.ksprintf (fun s ->
    Format.eprintf "%s@." s;
    exit 1
  ) k


let read_file f =
  let c = open_in f in
  let out = Buffer.create 1024 in
  let buf = String.create 1024 in
  try
    while true; do
      let i = input c buf 0 1023 in
      if i <> 0
      then Buffer.add_substring out buf 0 i
      else raise Not_found
    done;
    assert false;
  with Not_found -> Buffer.contents out

let _ =
  Util.Timer.init Unix.gettimeofday;
  let js_files = ref [] in
  let stdin = ref false in
  let output_file = ref None in
  let options =
    [("-debug", Arg.String Option.Debug.set, "<name> debug module <name>");
     ("-disable",
      Arg.String Option.Optim.disable, "<name> disable optimization <name>");
     ("-pretty", Arg.Unit (fun () -> Option.Optim.enable "pretty"), " pretty print the output");
     ("-debuginfo", Arg.Unit (fun () -> Option.Optim.enable "debuginfo"), " output debug info");
     ("-noinline", Arg.Unit (fun () -> Option.Optim.disable "inline"), " disable inlining");
     ("-stdin", Arg.Set stdin, " read from standard input");
     ("-o", Arg.String (fun s -> output_file := Some s),
      "<file> set output file name to <file>")]
  in

  let usage_header = (Format.sprintf "Usage: %s [options]" Sys.argv.(0)) in


  Arg.parse (Arg.align options)
    (fun s ->
       if Filename.check_suffix s ".js" then
         if Sys.file_exists s
         then js_files := s :: !js_files
         else error "file '%s' do not exist" s
       else
         error "Don't know what to do with '%s'" s
    )
    (usage_header);

  let chop_extension s =
    try Filename.chop_extension s with Invalid_argument _ -> s in

  let pp,finalize = match !output_file with
    | Some "-" ->
      Pretty_print.to_out_channel stdout,(fun _ -> ())
    | Some file ->
      let oc = open_out file in
      Pretty_print.to_out_channel oc, (fun _ -> close_out oc)
    | None when not(!stdin) && List.length !js_files = 1 ->
      let file = chop_extension (List.hd !js_files) ^ ".min.js" in
      let oc = open_out file in
      Pretty_print.to_out_channel oc, (fun _ -> close_out oc)
    | None when !stdin -> Pretty_print.to_out_channel stdout,(fun _ -> ())
    | None -> Arg.usage options usage_header; exit 0 in

  let pretty = Option.Optim.pretty () in
  Pretty_print.set_compact pp (not pretty);
  Code.Var.set_pretty pretty;

  let error_of_pi pi =
    if pi.Parse_info.name = ""
    then error "error at l:%d col:%d" pi.Parse_info.line pi.Parse_info.col
    else error "error at file:%S l:%d col:%d" pi.Parse_info.name pi.Parse_info.line pi.Parse_info.col in

  let p = List.flatten (List.map (fun file ->
    let lex = Parse_js.lexer_from_file file in
    try Parse_js.parse lex with Parse_js.Parsing_error pi -> error_of_pi pi) !js_files) in

  let p =
    if !stdin
    then
      let lex = Parse_js.lexer_from_channel Pervasives.stdin in
      try p@(Parse_js.parse lex) with Parse_js.Parsing_error pi -> error_of_pi pi;
    else
      p in

  let true_ = (fun () -> true) in
  let open Option in
  let passes : ((unit -> bool) * (unit -> Js_traverse.mapper)) list =
    [ Optim.shortvar, (fun () -> ((new Js_traverse.rename_variable Util.StringSet.empty) :> Js_traverse.mapper) );
      Optim.share_constant, (fun () -> new Js_traverse.share_constant);
      true_, (fun () -> new Js_traverse.clean);
    ] in

  let p = List.fold_left (fun p (t,m) -> if t() then (m())#program p else p) p passes in
  let p = Js_assign.program p in
  Js_output.program pp (fun _ -> None) p;
  finalize()
