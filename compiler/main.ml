(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2010 Jérôme Vouillon
 * Laboratoire PPS - CNRS Université Paris Diderot
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

let debug = Util.debug "main"
let times = Util.debug "times"

let f paths js_files input_file output_file =
  let t = Util.Timer.make () in
  List.iter Linker.add_file js_files;
  let paths = List.rev_append paths [Findlib.package_directory "stdlib"] in

  
  let t1 = Util.Timer.make () in
  let p =
    match input_file with
      None ->
        Parse.from_channel ~paths stdin
    | Some f ->
        let ch = open_in_bin f in
        let p = Parse.from_channel ~paths ch in
        close_in ch;
        p
  in
  if times () then Format.eprintf "  parsing: %a@." Util.Timer.print t1;
  let output_program = Driver.f p in
  begin match output_file with
    None ->
      output_program (Pretty_print.to_out_channel stdout)
  | Some f ->
      let ch = open_out_bin f in
      output_program (Pretty_print.to_out_channel ch);
      close_out ch
  end;
  if times () then Format.eprintf "compilation: %a@." Util.Timer.print t
 
let _ =
  Findlib.init ();
  Util.Timer.init Unix.gettimeofday;
  let js_files = ref [] in
  let output_file = ref None in
  let input_file = ref None in
  let no_runtime = ref false in
  let paths = ref [] in
  let options =
    [("-debug", Arg.String Util.set_debug, "<name> debug module <name>");
     ("-disable",
      Arg.String Util.set_disabled, "<name> disable optimization <name>");
     ("-pretty", Arg.Unit Driver.set_pretty, " pretty print the output");
     ("-noinline", Arg.Unit Inline.disable_inlining, " disable inlining");
     ("-noruntime", Arg.Unit (fun () -> no_runtime := true),
      " do not include the standard runtime");
     ("-toplevel", Arg.Unit Parse.build_toplevel,
      " compile a toplevel");
     ("-I", Arg.String (fun s -> paths := s :: !paths),
      "<dir> Add <dir> to the list of include directories");
     ("-o", Arg.String (fun s -> output_file := Some s),
      "<file> set output file name to <file>")]
  in
  Arg.parse (Arg.align options)
    (fun s ->
       if Filename.check_suffix s ".js" then
         js_files := s :: !js_files
       else
         input_file := Some s)
    (Format.sprintf "Usage: %s [options]" Sys.argv.(0));
  let runtime =
    if !no_runtime then [] else
    try
      [Filename.concat (Findlib.package_directory "js_of_ocaml") "runtime.js"]
    with Findlib.No_such_package _ ->
      Format.eprintf "%s: runtime file 'runtime.js' not found.@." Sys.argv.(0);
      exit 1
  in
  let chop_extension s =
    try Filename.chop_extension s with Invalid_argument _ -> s in
  f !paths (runtime @ List.rev !js_files) !input_file
    (match !output_file with
       Some _ -> !output_file
     | None   -> Util.opt_map (fun s -> chop_extension s ^ ".js") !input_file)
