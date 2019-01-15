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

open Js_of_ocaml_compiler
open Js_of_ocaml_compiler.Stdlib

let times = Debug.find "times"

let debug_mem = Debug.find "mem"

let _ = Sys.catch_break true

let temp_file_name =
  (* Inlined unavailable Filename.temp_file_name. Filename.temp_file gives
     us incorrect permissions. https://github.com/ocsigen/js_of_ocaml/issues/182 *)
  let prng = lazy (Random.State.make_self_init ()) in
  fun ~temp_dir prefix suffix ->
    let rnd = Random.State.bits (Lazy.force prng) land 0xFFFFFF in
    Filename.concat temp_dir (Printf.sprintf "%s%06x%s" prefix rnd suffix)

let gen_file file f =
  let f_tmp =
    temp_file_name ~temp_dir:(Filename.dirname file) (Filename.basename file) ".tmp"
  in
  try
    let ch = open_out_bin f_tmp in
    (try f ch with e -> close_out ch; raise e);
    close_out ch;
    (try Sys.remove file with Sys_error _ -> ());
    Sys.rename f_tmp file
  with exc -> Sys.remove f_tmp; raise exc

let f
    { CompileArg.common
    ; profile
    ; source_map
    ; runtime_files
    ; input_file
    ; output_file
    ; params
    ; static_env
    ; wrap_with_fun
    ; dynlink
    ; linkall
    ; toplevel
    ; nocmis
    ; runtime_only
    ; include_dir
    ; fs_files
    ; fs_output
    ; fs_external
    ; export_file } =
  let dynlink = dynlink || toplevel || runtime_only in
  let custom_header = common.CommonArg.custom_header in
  let global =
    match wrap_with_fun with Some fun_name -> `Bind_to fun_name | None -> `Auto
  in
  CommonArg.eval common;
  ( match output_file with
  | None | Some "" | Some "-" -> ()
  | Some name when debug_mem () -> Debug.start_profiling name
  | Some _ -> () );
  List.iter params ~f:(fun (s, v) -> Config.Param.set s v);
  List.iter static_env ~f:(fun (s, v) -> Eval.set_static_env s v);
  let t = Timer.make () in
  let include_dir =
    List.map include_dir ~f:(fun d ->
        match Findlib.path_require_findlib d with
        | Some d ->
            let pkg, d' =
              match String.split ~sep:Filename.dir_sep d with
              | [] -> assert false
              | [d] -> "js_of_ocaml", d
              | pkg :: l -> pkg, List.fold_left l ~init:"" ~f:Filename.concat
            in
            Filename.concat (Findlib.find_pkg_dir pkg) d'
        | None -> d )
  in
  let expunge =
    match export_file with
    | None -> None
    | Some file ->
        if not (Sys.file_exists file)
        then failwith (Printf.sprintf "export file %S does not exists" file);
        let ic = open_in file in
        let t = Hashtbl.create 17 in
        ( try
            while true do
              Hashtbl.add t (input_line ic) ()
            done;
            assert false
          with End_of_file -> () );
        close_in ic;
        Some (fun s -> try Hashtbl.find t s; `Keep with Not_found -> `Skip)
  in
  Linker.load_files runtime_files;
  let paths =
    try List.append include_dir [Findlib.find_pkg_dir "stdlib"] with Not_found ->
      include_dir
  in
  let t1 = Timer.make () in
  if times () then Format.eprintf "Start parsing...@.";
  let need_debug =
    if source_map <> None || Config.Flag.debuginfo () || toplevel
    then `Full
    else if Config.Flag.pretty ()
    then `Names
    else `No
  in
  let p, cmis, d, standalone =
    if runtime_only
    then
      ( Parse_bytecode.predefined_exceptions ()
      , StringSet.empty
      , Parse_bytecode.Debug.create ()
      , true )
    else
      match input_file with
      | None ->
          Parse_bytecode.from_channel
            ~includes:paths
            ~toplevel
            ?expunge
            ~dynlink
            ~debug:need_debug
            stdin
      | Some f ->
          let ch = open_in_bin f in
          let res =
            Parse_bytecode.from_channel
              ~includes:paths
              ~toplevel
              ?expunge
              ~dynlink
              ~debug:need_debug
              ch
          in
          close_in ch; res
  in
  let () =
    if (not runtime_only) && source_map <> None && Parse_bytecode.Debug.is_empty d
    then
      warn
        "Warning: '--source-map' is enabled but the bytecode program was compiled with \
         no debugging information.\n\
         Warning: Consider passing '-g' option to ocamlc.\n\
         %!"
  in
  let cmis = if nocmis then StringSet.empty else cmis in
  let p =
    let l =
      List.map static_env ~f:(fun (k, v) ->
          Primitive.add_external "caml_set_static_env";
          let args = [Code.Pc (IString k); Code.Pc (IString v)] in
          Code.(Let (Var.fresh (), Prim (Extern "caml_set_static_env", args))) )
    in
    Code.prepend p l
  in
  let p =
    if fs_external
    then
      let instrs = [Code.(Let (Var.fresh (), Prim (Extern "caml_fs_init", [])))] in
      Code.prepend p instrs
    else p
  in
  if times () then Format.eprintf "  parsing: %a@." Timer.print t1;
  let paths = paths @ StringSet.elements (Parse_bytecode.Debug.paths d ~units:cmis) in
  ( match output_file with
  | None ->
      let p = PseudoFs.f p cmis fs_files paths in
      let fmt = Pretty_print.to_out_channel stdout in
      Driver.f
        ~standalone
        ?profile
        ~linkall
        ~global
        ~dynlink
        ?source_map
        ?custom_header
        fmt
        d
        p
  | Some file ->
      gen_file file (fun chan ->
          let p = if fs_output = None then PseudoFs.f p cmis fs_files paths else p in
          let fmt = Pretty_print.to_out_channel chan in
          Driver.f
            ~standalone
            ?profile
            ~linkall
            ~global
            ~dynlink
            ?source_map
            ?custom_header
            fmt
            d
            p );
      Option.iter fs_output ~f:(fun file ->
          gen_file file (fun chan ->
              let pfs = PseudoFs.f_empty cmis fs_files paths in
              let pfs_fmt = Pretty_print.to_out_channel chan in
              Driver.f ~standalone ?profile ?custom_header ~global pfs_fmt d pfs ) ) );
  if times () then Format.eprintf "compilation: %a@." Timer.print t;
  Debug.stop_profiling ()

let main = Cmdliner.Term.(pure f $ CompileArg.options), CompileArg.info

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
  | Magic_number.Bad_magic_number s ->
      Format.eprintf "%s: Error: Not an ocaml bytecode file@." Sys.argv.(0);
      Format.eprintf "%s: Error: Invalid magic number %S@." Sys.argv.(0) s;
      exit 1
  | Magic_number.Bad_magic_version h ->
      Format.eprintf "%s: Error: Bytecode version missmatch.@." Sys.argv.(0);
      let k =
        match Magic_number.kind h with
        | (`Cmo | `Cma | `Exe) as x -> x
        | `Other _ -> assert false
      in
      let comp =
        if Magic_number.compare h (Magic_number.current k) < 0
        then "an older"
        else "a newer"
      in
      Format.eprintf
        "%s: Error: Your ocaml bytecode and the js_of_ocaml compiler have to be \
         compiled with the same version of ocaml.@."
        Sys.argv.(0);
      Format.eprintf
        "%s: Error: The Js_of_ocaml compiler has been compiled with ocaml version %s.@."
        Sys.argv.(0)
        Sys.ocaml_version;
      Format.eprintf
        "%s: Error: Its seems that your ocaml bytecode has been compiled with %s \
         version of ocaml.@."
        Sys.argv.(0)
        comp;
      exit 1
  | Failure s ->
      Format.eprintf "%s: Error: %s@." Sys.argv.(0) s;
      exit 1
  | exc ->
      let backtrace = Printexc.get_backtrace () in
      Format.eprintf "%s: Error: %s@." Sys.argv.(0) (Printexc.to_string exc);
      prerr_string backtrace;
      exit 1
