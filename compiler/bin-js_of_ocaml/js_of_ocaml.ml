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

open! Js_of_ocaml_compiler.Stdlib
open Js_of_ocaml_compiler

let times = Debug.find "times"

let debug_mem = Debug.find "mem"

let _ = Sys.catch_break true

let gen_unit_filename dir u =
  Filename.concat dir (Printf.sprintf "%s.js" u.Cmo_format.cu_name)

let f
    { Arg.common
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
    ; export_file
    ; keep_unit_names
    } =
  let dynlink = dynlink || toplevel || runtime_only in
  let custom_header = common.Jsoo_cmdline.Arg.custom_header in
  let global =
    match wrap_with_fun with
    | Some fun_name -> `Bind_to fun_name
    | None -> `Auto
  in
  Jsoo_cmdline.Arg.eval common;
  (match output_file with
  | `Stdout, _ -> ()
  | `Name name, _ when debug_mem () -> Debug.start_profiling name
  | `Name _, _ -> ());
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
              | [ d ] -> "js_of_ocaml", d
              | pkg :: l -> pkg, List.fold_left l ~init:"" ~f:Filename.concat
            in
            Filename.concat (Findlib.find_pkg_dir pkg) d'
        | None -> d)
  in
  let exported_unit =
    match export_file with
    | None -> None
    | Some file ->
        if not (Sys.file_exists file)
        then failwith (Printf.sprintf "export file %S does not exists" file);
        let ic = open_in file in
        let t = Hashtbl.create 17 in
        (try
           while true do
             Hashtbl.add t (input_line ic) ()
           done;
           assert false
         with End_of_file -> ());
        close_in ic;
        Some (Hashtbl.fold (fun cmi () acc -> cmi :: acc) t [])
  in
  Linker.load_files runtime_files;
  let paths =
    try List.append include_dir [ Findlib.find_pkg_dir "stdlib" ]
    with Not_found -> include_dir
  in
  if times () then Format.eprintf "Start parsing...@.";
  let need_debug = Option.is_some source_map || Config.Flag.debuginfo () in
  let check_debug debug =
    if (not runtime_only)
       && Option.is_some source_map
       && Parse_bytecode.Debug.is_empty debug
    then
      warn
        "Warning: '--source-map' is enabled but the bytecode program was compiled with \
         no debugging information.\n\
         Warning: Consider passing '-g' option to ocamlc.\n\
         %!"
  in
  let pseudo_fs_instr prim debug cmis =
    let cmis = if nocmis then StringSet.empty else cmis in
    let paths =
      paths @ StringSet.elements (Parse_bytecode.Debug.paths debug ~units:cmis)
    in
    Pseudo_fs.f ~prim ~cmis ~files:fs_files ~paths
  in
  let env_instr () =
    List.map static_env ~f:(fun (k, v) ->
        Primitive.add_external "caml_set_static_env";
        let args = [ Code.Pc (IString k); Code.Pc (IString v) ] in
        Code.(Let (Var.fresh (), Prim (Extern "caml_set_static_env", args))))
  in
  let output (one : Parse_bytecode.one) ~standalone output_file =
    check_debug one.debug;
    let init_pseudo_fs = fs_external && standalone in
    (match output_file with
    | `Stdout ->
        let instr =
          List.concat
            [ pseudo_fs_instr `caml_create_file one.debug one.cmis
            ; (if init_pseudo_fs then [ Pseudo_fs.init () ] else [])
            ; env_instr ()
            ]
        in
        let code = Code.prepend one.code instr in
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
          one.debug
          code
    | `Name file ->
        let fs_instr1, fs_instr2 =
          match fs_output with
          | None -> pseudo_fs_instr `caml_create_file one.debug one.cmis, []
          | Some _ -> [], pseudo_fs_instr `caml_create_file_extern one.debug one.cmis
        in
        Filename.gen_file file (fun chan ->
            let instr =
              List.concat
                [ fs_instr1
                ; (if init_pseudo_fs then [ Pseudo_fs.init () ] else [])
                ; env_instr ()
                ]
            in
            let code = Code.prepend one.code instr in
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
              one.debug
              code);
        Option.iter fs_output ~f:(fun file ->
            Filename.gen_file file (fun chan ->
                let instr = fs_instr2 in
                let code = Code.prepend Code.empty instr in
                let pfs_fmt = Pretty_print.to_out_channel chan in
                Driver.f
                  ~standalone
                  ?profile
                  ?custom_header
                  ~global
                  pfs_fmt
                  one.debug
                  code)));
    if times () then Format.eprintf "compilation: %a@." Timer.print t
  in
  (if runtime_only
  then
    let code : Parse_bytecode.one =
      { code = Parse_bytecode.predefined_exceptions ()
      ; cmis = StringSet.empty
      ; debug = Parse_bytecode.Debug.create ~toplevel:false false
      }
    in
    output code ~standalone:true (fst output_file)
  else
    let kind, ic, close_ic =
      match input_file with
      | None -> Parse_bytecode.from_channel stdin, stdin, fun () -> ()
      | Some fn ->
          let ch = open_in_bin fn in
          let res = Parse_bytecode.from_channel ch in
          res, ch, fun () -> close_in ch
    in
    (match kind with
    | `Exe ->
        let t1 = Timer.make () in
        let code =
          Parse_bytecode.from_exe
            ~includes:paths
            ~toplevel
            ?exported_unit
            ~dynlink
            ~debug:need_debug
            ic
        in
        if times () then Format.eprintf "  parsing: %a@." Timer.print t1;
        output code ~standalone:true (fst output_file)
    | `Cmo cmo ->
        let output_file =
          match output_file, keep_unit_names with
          | (`Stdout, false), true -> `Name (gen_unit_filename "./" cmo)
          | (`Name x, false), true -> `Name (gen_unit_filename (Filename.dirname x) cmo)
          | (`Stdout, _), false -> `Stdout
          | (`Name x, _), false -> `Name x
          | (`Name x, true), true
            when String.length x > 0 && Char.equal x.[String.length x - 1] '/' ->
              `Name (gen_unit_filename x cmo)
          | (`Name _, true), true | (`Stdout, true), true ->
              failwith "use [-o dirname/] or remove [--keep-unit-names]"
        in
        let t1 = Timer.make () in
        let code =
          Parse_bytecode.from_cmo ~includes:paths ~toplevel ~debug:need_debug cmo ic
        in
        if times () then Format.eprintf "  parsing: %a@." Timer.print t1;
        output code ~standalone:false output_file
    | `Cma cma when keep_unit_names ->
        List.iter cma.lib_units ~f:(fun cmo ->
            let output_file =
              match output_file with
              | `Stdout, false -> `Name (gen_unit_filename "./" cmo)
              | `Name x, false -> `Name (gen_unit_filename (Filename.dirname x) cmo)
              | `Name x, true
                when String.length x > 0 && Char.equal x.[String.length x - 1] '/' ->
                  `Name (gen_unit_filename x cmo)
              | `Stdout, true | `Name _, true ->
                  failwith "use [-o dirname/] or remove [--keep-unit-names]"
            in
            let t1 = Timer.make () in
            let code =
              Parse_bytecode.from_cmo ~includes:paths ~toplevel ~debug:need_debug cmo ic
            in
            if times ()
            then Format.eprintf "  parsing: %a (%s)@." Timer.print t1 cmo.cu_name;
            output code ~standalone:false output_file)
    | `Cma cma ->
        let t1 = Timer.make () in
        let code =
          Parse_bytecode.from_cma ~includes:paths ~toplevel ~debug:need_debug cma ic
        in
        if times () then Format.eprintf "  parsing: %a@." Timer.print t1;
        output code ~standalone:false (fst output_file));
    close_ic ());
  Debug.stop_profiling ()

let main = Cmdliner.Term.(pure f $ Arg.options), Arg.info

let runtime_only =
  Cmdliner.Term.(pure f $ Arg.options_runtime_only), Cmdliner.Term.info "runtime-only"

let _ =
  Timer.init Sys.time;
  try
    Cmdliner.Term.eval_choice
      ~catch:false
      ~argv:(Jsoo_cmdline.normalize_argv ~warn:(warn "%s") Sys.argv)
      main
      [ runtime_only ]
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
      Format.eprintf "%s: Error: Bytecode version mismatch.@." Sys.argv.(0);
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
        "%s: Error: Your ocaml bytecode and the js_of_ocaml compiler have to be compiled \
         with the same version of ocaml.@."
        Sys.argv.(0);
      Format.eprintf
        "%s: Error: The Js_of_ocaml compiler has been compiled with ocaml version %s.@."
        Sys.argv.(0)
        Sys.ocaml_version;
      Format.eprintf
        "%s: Error: Its seems that your ocaml bytecode has been compiled with %s version \
         of ocaml.@."
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
