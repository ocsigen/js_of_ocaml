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

let _ = Sys.catch_break true

let temp_file_name =
  (* Inlined unavailable Filename.temp_file_name. Filename.temp_file gives
     us incorrect permissions. https://github.com/ocsigen/js_of_ocaml/issues/182 *)
  let prng = lazy(Random.State.make_self_init ()) in
  fun ~temp_dir prefix suffix ->
    let rnd = (Random.State.bits (Lazy.force prng)) land 0xFFFFFF in
    Filename.concat temp_dir (Printf.sprintf "%s%06x%s" prefix rnd suffix)

let gen_file file f =
  let f_tmp = temp_file_name
      ~temp_dir:(Filename.dirname file)
      (Filename.basename file) ".tmp" in
  try
    let ch = open_out_bin f_tmp in
    (try f ch with e -> close_out ch; raise e);
    close_out ch;
    (try Sys.remove file with Sys_error _ -> ());
    Sys.rename f_tmp file;
  with exc ->
    Sys.remove f_tmp;
    raise exc

let f {
    CompileArg.common;
    profile; source_map; runtime_files; input_file; output_file;
    params ;
    linkall; toplevel; nocmis;
    include_dir; fs_files; fs_output; fs_external } =
  CommonArg.eval common;
  List.iter (fun (s,v) -> Option.Param.set s v) params;
  let t = Util.Timer.make () in
  Linker.load_files runtime_files;
  let paths =
    try List.append include_dir [Util.find_pkg_dir "stdlib"]
    with Not_found -> include_dir in
  let t1 = Util.Timer.make () in
  if times () then Format.eprintf "Start parsing...@.";
  let need_debug =
    if source_map <> None || Option.Optim.debuginfo () then `Full else
    if Option.Optim.pretty () then `Names else `No
  in
  let p, cmis, d =
    match input_file with
      None ->
        Parse_bytecode.from_channel ~toplevel ~debug:need_debug stdin
    | Some f ->
        let ch = open_in_bin f in
        let p,cmis,d = Parse_bytecode.from_channel ~toplevel ~debug:need_debug ch in
        close_in ch;
        p, cmis, d
  in
  let () =
    if source_map <> None &&  Parse_bytecode.Debug.is_empty d
    then
      Util.warn
	"Warning: '--source-map' is enabled but the bytecode program \
	 was compiled with no debugging information.\n\
	 Warning: Consider passing '-g' option to ocamlc.\n%!"
  in
  let cmis = if nocmis then Util.StringSet.empty else cmis in
  let p =
    if fs_external
    then
      let instrs = [
        Code.(Let(Var.fresh (), Prim (Extern "caml_fs_init", [])))
      ] in
      Code.prepend p instrs
    else p in
  if times () then Format.eprintf "  parsing: %a@." Util.Timer.print t1;
  begin match output_file with
    | None ->
      let p = PseudoFs.f p cmis fs_files paths in
      let fmt = Pretty_print.to_out_channel stdout in
      Driver.f ?profile ~toplevel ~linkall ?source_map fmt d p
    | Some file ->
      gen_file file (fun chan ->
          let p =
            if fs_output = None
            then PseudoFs.f p cmis fs_files paths
            else p in
          let fmt = Pretty_print.to_out_channel chan in
          Driver.f ?profile ~toplevel ~linkall ?source_map fmt d p;
        );
      Util.opt_iter (fun file ->
          gen_file file (fun chan ->
              let pfs = PseudoFs.f_empty cmis fs_files paths in
              let pfs_fmt = Pretty_print.to_out_channel chan in
              Driver.f ?profile pfs_fmt d pfs
            )
        ) fs_output
  end;
  if times () then Format.eprintf "compilation: %a@." Util.Timer.print t

let main =
  Cmdliner.Term.(pure f $ CompileArg.options),
  CompileArg.info

let _ =
  Util.Timer.init Sys.time;
  try Cmdliner.Term.eval ~catch:false ~argv:(Util.normalize_argv ~warn_:true Sys.argv) main with
  | (Match_failure _ | Assert_failure _ | Not_found) as exc ->
    let backtrace = Printexc.get_backtrace () in
    Format.eprintf
      "%s: You found a bug. \
       Please report it at https://github.com/ocsigen/js_of_ocaml/issues :@."
      Sys.argv.(0);
    Format.eprintf "Error: %s@." (Printexc.to_string exc);
    prerr_string backtrace;
    exit 1
  | Util.MagicNumber.Bad_magic_number s ->
    Format.eprintf "%s: Error: Not an ocaml executable bytecode@." Sys.argv.(0);
    Format.eprintf "%s: Error: Invalid magic number %S, expecting %S@." Sys.argv.(0) s Util.MagicNumber.(to_string current);
    exit 1
  | Util.MagicNumber.Bad_magic_version h ->
    Format.eprintf "%s: Error: Bytecode version missmatch. Got version %S, expecting %S.@."
      Sys.argv.(0)
      Util.MagicNumber.(to_string h)
      Util.MagicNumber.(to_string current);
    let comp =
      if Util.MagicNumber.(compare h current) < 0
      then "an older"
      else "a newer" in
    Format.eprintf "%s: Error: Your program and the js_of_ocaml compiler have to be compiled with the same version of ocaml.@." Sys.argv.(0);
    Format.eprintf "%s: Error: The Js_of_ocaml compiler has been compiled with ocaml version %s.@." Sys.argv.(0) Sys.ocaml_version;
    Format.eprintf "%s: Error: Its seems that your program has been compiled with %s version of ocaml.@." Sys.argv.(0) comp;
    exit 1
  | Failure s ->
    Format.eprintf "%s: Error: %s@." Sys.argv.(0) s;
    exit 1
  | exc ->
    let backtrace = Printexc.get_backtrace () in
    Format.eprintf "%s: Error: %s@." Sys.argv.(0) (Printexc.to_string exc);
    prerr_string backtrace;
    exit 1
