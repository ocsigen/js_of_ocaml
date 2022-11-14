(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
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

let run
    { Cmd_arg.common
    ; profile
    ; source_map
    ; runtime_files
    ; no_runtime
    ; input_file
    ; output_file
    ; params
    ; static_env
    ; wrap_with_fun
    ; dynlink
    ; linkall
    ; target_env
    ; toplevel
    ; no_cmis
    ; runtime_only
    ; include_dirs
    ; fs_files
    ; fs_output
    ; fs_external
    ; export_file
    ; keep_unit_names
    } =
  let include_cmis = toplevel && not no_cmis in
  let custom_header = common.Jsoo_cmdline.Arg.custom_header in
  Jsoo_cmdline.Arg.eval common;
  (match output_file with
  | `Stdout, _ -> ()
  | `Name name, _ when debug_mem () -> Debug.start_profiling name
  | `Name _, _ -> ());
  List.iter params ~f:(fun (s, v) -> Config.Param.set s v);
  List.iter static_env ~f:(fun (s, v) -> Eval.set_static_env s v);
  let t = Timer.make () in
  let include_dirs =
    List.filter_map (include_dirs @ [ "+stdlib/" ]) ~f:(fun d -> Findlib.find [] d)
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
  let runtime_files =
    if toplevel || dynlink
    then
      let add_if_absent x l = if List.mem x ~set:l then l else x :: l in
      runtime_files |> add_if_absent "+toplevel.js" |> add_if_absent "+dynlink.js"
    else runtime_files
  in
  let runtime_files, builtin =
    List.partition_map runtime_files ~f:(fun name ->
        match Builtins.find name with
        | Some t -> `Snd t
        | None -> `Fst name)
  in
  let t1 = Timer.make () in
  let builtin =
    if no_runtime then builtin else Js_of_ocaml_compiler_runtime_files.runtime @ builtin
  in
  List.iter builtin ~f:(fun t ->
      let filename = Builtins.File.name t in
      let runtimes = Linker.Fragment.parse_builtin t in
      Linker.load_fragments ~target_env ~filename runtimes);
  Linker.load_files ~target_env runtime_files;
  Linker.check_deps ();
  if times () then Format.eprintf "  parsing js: %a@." Timer.print t1;
  if times () then Format.eprintf "Start parsing...@.";
  let need_debug = Option.is_some source_map || Config.Flag.debuginfo () in
  let check_debug (one : Parse_bytecode.one) =
    if (not runtime_only)
       && Option.is_some source_map
       && Parse_bytecode.Debug.is_empty one.debug
       && not (Code.is_empty one.code)
    then
      warn
        "Warning: '--source-map' is enabled but the bytecode program was compiled with \
         no debugging information.\n\
         Warning: Consider passing '-g' option to ocamlc.\n\
         %!"
  in
  let pseudo_fs_instr prim debug cmis =
    let paths =
      include_dirs @ StringSet.elements (Parse_bytecode.Debug.paths debug ~units:cmis)
    in
    Pseudo_fs.f ~prim ~cmis ~files:fs_files ~paths
  in
  let env_instr () =
    List.concat_map static_env ~f:(fun (k, v) ->
        Primitive.add_external "caml_set_static_env";
        let var_k = Code.Var.fresh () in
        let var_v = Code.Var.fresh () in
        Code.
          [ Let (var_k, Prim (Extern "caml_jsstring_of_string", [ Pc (String k) ]))
          ; Let (var_v, Prim (Extern "caml_jsstring_of_string", [ Pc (String v) ]))
          ; Let (Var.fresh (), Prim (Extern "caml_set_static_env", [ Pv var_k; Pv var_v ]))
          ])
  in
  let output (one : Parse_bytecode.one) ~linkall ~standalone output_file =
    check_debug one;
    let init_pseudo_fs = fs_external && standalone in
    (match output_file with
    | `Stdout ->
        let instr =
          List.concat
            [ pseudo_fs_instr `create_file one.debug one.cmis
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
          ~wrap_with_fun
          ?source_map
          ?custom_header
          fmt
          one.debug
          code
    | `Name file ->
        let fs_instr1, fs_instr2 =
          match fs_output with
          | None -> pseudo_fs_instr `create_file one.debug one.cmis, []
          | Some _ -> [], pseudo_fs_instr `create_file_extern one.debug one.cmis
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
              ~wrap_with_fun
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
                  ~wrap_with_fun
                  pfs_fmt
                  one.debug
                  code)));
    if times () then Format.eprintf "compilation: %a@." Timer.print t
  in
  let output_partial code output_file =
    output code ~standalone:false ~linkall:false output_file
  in
  (if runtime_only
  then
    let code : Parse_bytecode.one =
      { code = Parse_bytecode.predefined_exceptions ()
      ; cmis = StringSet.empty
      ; debug = Parse_bytecode.Debug.create ~include_cmis:false false
      }
    in
    output code ~standalone:true ~linkall:true (fst output_file)
  else
    let kind, ic, close_ic, include_dirs =
      match input_file with
      | None -> Parse_bytecode.from_channel stdin, stdin, (fun () -> ()), include_dirs
      | Some fn ->
          let ch = open_in_bin fn in
          let res = Parse_bytecode.from_channel ch in
          let include_dirs = Filename.dirname fn :: include_dirs in
          res, ch, (fun () -> close_in ch), include_dirs
    in
    (match kind with
    | `Exe ->
        let t1 = Timer.make () in
        (* The OCaml compiler can generate code using the
           "caml_string_greaterthan" primitive but does not use it
           itself. This is (was at some point at least) the only primitive
           in this case.  Ideally, Js_of_ocaml should parse the .mli files
           for primitives as well as marking this primitive as potentially
           used. But the -linkall option is probably good enough. *)
        let linkall = linkall || toplevel || dynlink in
        let code =
          Parse_bytecode.from_exe
            ~includes:include_dirs
            ~include_cmis
            ~link_info:(toplevel || dynlink)
            ~linkall
            ?exported_unit
            ~debug:need_debug
            ic
        in
        if times () then Format.eprintf "  parsing: %a@." Timer.print t1;
        output code ~standalone:true ~linkall (fst output_file)
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
          Parse_bytecode.from_cmo
            ~includes:include_dirs
            ~include_cmis
            ~debug:need_debug
            cmo
            ic
        in
        if times () then Format.eprintf "  parsing: %a@." Timer.print t1;
        output_partial code output_file
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
              Parse_bytecode.from_cmo
                ~includes:include_dirs
                ~include_cmis
                ~debug:need_debug
                cmo
                ic
            in
            if times ()
            then Format.eprintf "  parsing: %a (%s)@." Timer.print t1 cmo.cu_name;
            output_partial code output_file)
    | `Cma cma ->
        let t1 = Timer.make () in
        let code =
          Parse_bytecode.from_cma
            ~includes:include_dirs
            ~include_cmis
            ~debug:need_debug
            cma
            ic
        in
        if times () then Format.eprintf "  parsing: %a@." Timer.print t1;
        output_partial code (fst output_file));
    close_ic ());
  Debug.stop_profiling ()

let info name =
  Info.make
    ~name
    ~doc:"Js_of_ocaml compiler"
    ~description:
      "Js_of_ocaml is a compiler from OCaml bytecode to Javascript. It makes it possible \
       to run pure OCaml programs in JavaScript environments like web browsers and \
       Node.js."

let term = Cmdliner.Term.(const run $ Cmd_arg.options)

let command =
  let t = Cmdliner.Term.(const run $ Cmd_arg.options) in
  Cmdliner.Cmd.v (info "compile") t
