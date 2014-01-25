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

let times = Option.Debug.find "times"

let f toplevel linkall paths js_files input_file output_file source_map =
  let t = Util.Timer.make () in
  List.iter Linker.add_file js_files;
  let paths = List.rev_append paths [Util.find_pkg_dir "stdlib"] in
  let t1 = Util.Timer.make () in
  let need_debug =
    if source_map <> None || Option.Optim.debuginfo () then `Full else
    if Option.Optim.pretty () then `Names else `No
  in
  let p,d =
    match input_file with
      None ->
      Parse_bytecode.from_channel ~toplevel ~debug:need_debug ~paths stdin
    | Some f ->
      let ch = open_in_bin f in
      let p,d = Parse_bytecode.from_channel ~debug:need_debug ~toplevel ~paths ch in
      close_in ch;
      p,d
  in
  if times () then Format.eprintf "  parsing: %a@." Util.Timer.print t1;
  let output_program fmt = Driver.f ~toplevel ~linkall ?source_map fmt d p in
  begin match output_file with
    | None ->
      output_program (Pretty_print.to_out_channel stdout)
    | Some f ->
      let f_tmp = Filename.temp_file ~temp_dir:(Filename.dirname f) (Filename.basename f) ".tmpjs" in
      try
        let ch = open_out_bin f_tmp in
        output_program (Pretty_print.to_out_channel ch);
        close_out ch;
        (try Sys.remove f with Sys_error _ -> ());
        Sys.rename f_tmp f
      with exc ->
        Sys.remove f_tmp;
        Format.eprintf "compilation error: %s@." (Printexc.to_string exc);
        raise exc
  end;
  if times () then Format.eprintf "compilation: %a@." Util.Timer.print t

let _ =
  Util.Timer.init Unix.gettimeofday;
  let js_files = ref [] in
  let output_file = ref None in
  let input_file = ref None in
  let no_runtime = ref false in
  let linkall = ref false in
  let toplevel = ref false in
  let source_map = ref false in
  let paths = ref [] in
  let options =
    [("-debug", Arg.String Option.Debug.set, "<name> debug module <name>");
     ("-disable",
      Arg.String Option.Optim.disable, "<name> disable optimization <name>");
     ("-enable",
      Arg.String Option.Optim.enable, "<name> enable optimization <name>");
     ("-pretty", Arg.Unit (fun () -> Option.Optim.enable "pretty"), " pretty print the output");
     ("-debuginfo", Arg.Unit (fun () -> Option.Optim.enable "debuginfo"), " output debug info");
     ("-opt", Arg.Int Driver.set_profile, "<oN> set optimization profile : o1 (default), o2, o3");
     ("-noinline", Arg.Unit (fun () -> Option.Optim.disable "inline"), " disable inlining");
     ("-linkall", Arg.Set linkall, " link all primitives");
     ("-noruntime", Arg.Unit (fun () -> no_runtime := true),
      " do not include the standard runtime");
     ("-sourcemap", Arg.Unit (fun () -> source_map := true), " generate source map");
     ("-toplevel", Arg.Set toplevel, " compile a toplevel");
     ("-I", Arg.String (fun s -> paths := s :: !paths),
      "<dir> Add <dir> to the list of include directories");
     ("-o", Arg.String (fun s -> output_file := Some s),
      "<file> set output file name to <file>")]
  in
  if !toplevel then linkall:=true;
  Arg.parse (Arg.align options)
      (fun s ->
         if Filename.check_suffix s ".js" then
           js_files := s :: !js_files
         else
           input_file := Some s)
      (Format.sprintf "Usage: %s [options]" Sys.argv.(0));
    let runtime = if !no_runtime then [] else ["+runtime.js"] in
    let chop_extension s =
      try Filename.chop_extension s with Invalid_argument _ -> s in
    let output_f = match !output_file with
        Some _ -> !output_file
      | None   -> Util.opt_map (fun s -> chop_extension s ^ ".js") !input_file in
    let source_m =
      if !source_map
      then
        if Option.Optim.pretty ()
        then begin
          Format.eprintf "Source-map is not compatible with pretty mode@.";
          exit 1
        end
        else match output_f with
          | Some file ->
            Some (
              chop_extension file ^ ".map",
              {
                Source_map.version = 3;
                file;
                sourceroot = None;
                sources = [];
                sources_content = [];
                names = [];
                mappings = []
              })
          | None ->
            Format.eprintf "Don't know where to output the Source-map@.";
            exit 1
      else None in
    f !toplevel !linkall !paths (runtime @ List.rev !js_files)
      !input_file output_f source_m
