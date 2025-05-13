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

let () = Sys.catch_break true

let gen_unit_filename dir u =
  Filename.concat dir (Printf.sprintf "%s.js" (Ocaml_compiler.Cmo_format.name u))

let header formatter ~custom_header =
  match custom_header with
  | None -> ()
  | Some c -> Pretty_print.string formatter (c ^ "\n")

let jsoo_header formatter build_info =
  Pretty_print.string formatter (Printf.sprintf "%s\n" Global_constant.header);
  Pretty_print.string formatter (Build_info.to_string build_info)

let source_map_enabled : Source_map.Encoding_spec.t option -> bool = function
  | None -> false
  | Some _ -> true

let output_gen
    ~standalone
    ~custom_header
    ~build_info
    ~(source_map : Source_map.Encoding_spec.t option)
    output_file
    f =
  let f chan k =
    let fmt = Pretty_print.to_out_channel chan in
    Driver.configure fmt;
    if standalone then header ~custom_header fmt;
    if Config.Flag.header () then jsoo_header fmt build_info;
    let sm = f ~standalone ~source_map (k, fmt) in
    match source_map, sm with
    | None, _ | _, None -> ()
    | Some { output_file = output; source_map; keep_empty }, Some sm ->
        let sm = if keep_empty then Source_map.Standard source_map else sm in
        if Debug.find "invariant" () then Source_map.invariant sm;
        let urlData =
          match output with
          | None ->
              let data = Source_map.to_string sm in
              "data:application/json;base64," ^ Base64.encode_exn data
          | Some output_file ->
              Source_map.to_file sm output_file;
              Filename.basename output_file
        in
        Pretty_print.newline fmt;
        Pretty_print.string fmt (Printf.sprintf "//# sourceMappingURL=%s\n" urlData)
  in

  match output_file with
  | `Stdout -> f stdout `Stdout
  | `Name name -> Filename.gen_file name (fun chan -> f chan `File)

let find_source file =
  match Builtins.find file with
  | Some f -> Some (Source_map.Source_content.create (Builtins.File.content f))
  | None ->
      if String.equal file Js_output.blackbox_filename
      then Some (Source_map.Source_content.create "(* generated code *)")
      else if Sys.file_exists file && not (Sys.is_directory file)
      then
        let content = Fs.read_file file in
        Some (Source_map.Source_content.create content)
      else None

let sourcemap_section_of_info
    ~(base : Source_map.Standard.t)
    { Source_map.sources; names; mappings } =
  let sources_content =
    match base.sources_content with
    | None -> None
    | Some _ -> Some (List.map ~f:find_source sources)
  in
  let sources =
    List.map sources ~f:(fun filename ->
        match Builtins.find filename with
        | None -> filename
        | Some _ -> Filename.concat "/builtin" filename)
  in
  let ignore_list =
    List.filter sources ~f:(fun filename ->
        String.starts_with ~prefix:"/builtin/" filename)
  in
  let offset, mappings = Source_map.Mappings.encode_with_offset mappings in
  let map =
    { (base : Source_map.Standard.t) with
      sources
    ; sources_content
    ; names
    ; mappings
    ; ignore_list
    }
  in
  { Source_map.Index.offset; map }

let sourcemap_of_infos ~base l =
  match base with
  | None -> None
  | Some (base : Source_map.Standard.t) ->
      let sections = List.map l ~f:(sourcemap_section_of_info ~base) in
      Some
        (Source_map.Index
           { Source_map.Index.version = base.Source_map.Standard.version
           ; file = base.file
           ; sections
           })

let sourcemap_of_info ~base info = sourcemap_of_infos ~base [ info ]

let run
    { Cmd_arg.common
    ; profile
    ; source_map
    ; runtime_files = runtime_files_from_cmdline
    ; no_runtime
    ; bytecode
    ; output_file
    ; params
    ; static_env
    ; wrap_with_fun
    ; dynlink
    ; linkall
    ; target_env
    ; toplevel
    ; no_cmis
    ; include_dirs
    ; fs_files
    ; fs_output
    ; fs_external
    ; export_file
    ; keep_unit_names
    ; include_runtime
    ; effects
    } =
  let source_map_base =
    Option.map ~f:(fun spec -> spec.Source_map.Encoding_spec.source_map) source_map
  in
  let include_cmis = toplevel && not no_cmis in
  let custom_header = common.Jsoo_cmdline.Arg.custom_header in
  Config.set_target `JavaScript;
  Jsoo_cmdline.Arg.eval common;
  Config.set_effects_backend effects;
  Linker.reset ();
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
        then failwith (Printf.sprintf "export file %S does not exist" file);
        let ic = open_in_text file in
        let t = String.Hashtbl.create 17 in
        (try
           while true do
             String.Hashtbl.add t (String.trim (In_channel.input_line_exn ic)) ()
           done;
           assert false
         with End_of_file -> ());
        close_in ic;
        Some (String.Hashtbl.fold (fun cmi () acc -> cmi :: acc) t [])
  in
  let runtime_files =
    if (not no_runtime) && (toplevel || dynlink)
    then
      let add_if_absent x l = if List.mem ~eq:String.equal x l then l else x :: l in
      runtime_files_from_cmdline
      |> add_if_absent "+toplevel.js"
      |> add_if_absent "+dynlink.js"
    else runtime_files_from_cmdline
  in
  let runtime_files, builtin =
    List.partition_map runtime_files ~f:(fun name ->
        match Builtins.find name with
        | Some t -> Right t
        | None -> Left name)
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
    if Option.is_some source_map && Parse_bytecode.Debug.is_empty one.debug
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
  let output
      (one : Parse_bytecode.one)
      ~check_sourcemap
      ~standalone
      ~(source_map : Source_map.Encoding_spec.t option)
      ~link
      output_file =
    if check_sourcemap then check_debug one;
    let init_pseudo_fs = fs_external && standalone in
    let sm =
      match output_file with
      | `Stdout, formatter ->
          let instr =
            List.concat
              [ pseudo_fs_instr `create_file one.debug one.cmis
              ; (if init_pseudo_fs then [ Pseudo_fs.init () ] else [])
              ; env_instr ()
              ]
          in
          let code = Code.prepend one.code instr in
          Driver.f
            ~standalone
            ?profile
            ~link
            ~wrap_with_fun
            ~source_map:(source_map_enabled source_map)
            ~formatter
            code
      | `File, formatter ->
          let fs_instr1, fs_instr2 =
            match fs_output with
            | None -> pseudo_fs_instr `create_file one.debug one.cmis, []
            | Some _ -> [], pseudo_fs_instr `create_file_extern one.debug one.cmis
          in
          let instr =
            List.concat
              [ fs_instr1
              ; (if init_pseudo_fs then [ Pseudo_fs.init () ] else [])
              ; env_instr ()
              ]
          in
          let code = Code.prepend one.code instr in
          let res =
            Driver.f
              ~standalone
              ?profile
              ~link
              ~wrap_with_fun
              ~source_map:(source_map_enabled source_map)
              ~formatter
              code
          in
          Option.iter fs_output ~f:(fun file ->
              Filename.gen_file file (fun chan ->
                  let instr = fs_instr2 in
                  let code = Code.prepend Code.empty instr in
                  let pfs_fmt = Pretty_print.to_out_channel chan in
                  Driver.f' ~standalone ~link:`Needed ?profile ~wrap_with_fun pfs_fmt code));
          res
    in
    if times () then Format.eprintf "compilation: %a@." Timer.print t;
    sm
  in
  let output_partial
      (cmo : Cmo_format.compilation_unit)
      ~standalone
      ~source_map
      code
      ((_, fmt) as output_file) =
    assert (not standalone);
    let uinfo = Unit_info.of_cmo cmo in
    Pretty_print.string fmt "\n";
    Pretty_print.string fmt (Unit_info.to_string uinfo);
    output code ~check_sourcemap:true ~source_map ~standalone ~link:`No output_file
  in
  let output_partial_runtime ~standalone ~source_map ((_, fmt) as output_file) =
    assert (not standalone);
    let primitives, aliases =
      let all = Linker.list_all_with_aliases ~from:runtime_files_from_cmdline () in
      StringMap.fold
        (fun n a (primitives, aliases) ->
          let primitives = StringSet.add n primitives in
          let aliases = List.map (StringSet.elements a) ~f:(fun a -> a, n) @ aliases in
          primitives, aliases)
        all
        (StringSet.empty, [])
    in
    let uinfo = Unit_info.of_primitives ~aliases (StringSet.elements primitives) in
    Pretty_print.string fmt "\n";
    Pretty_print.string fmt (Unit_info.to_string uinfo);
    let code =
      { Parse_bytecode.code = Code.empty
      ; cmis = StringSet.empty
      ; debug = Parse_bytecode.Debug.default_summary
      }
    in
    output
      code
      ~check_sourcemap:false
      ~source_map
      ~standalone
      ~link:(`All_from runtime_files_from_cmdline)
      output_file
  in
  (match bytecode with
  | `None ->
      let primitives, aliases =
        let all = Linker.list_all_with_aliases () in
        StringMap.fold
          (fun n a (primitives, aliases) ->
            let primitives = StringSet.add n primitives in
            let aliases = List.map (StringSet.elements a) ~f:(fun a -> a, n) @ aliases in
            primitives, aliases)
          all
          (StringSet.empty, [])
      in
      let primitives = StringSet.elements primitives in
      assert (List.length primitives > 0);
      let code, uinfo = Parse_bytecode.predefined_exceptions () in
      let uinfo = Unit_info.union uinfo (Unit_info.of_primitives ~aliases primitives) in
      let code : Parse_bytecode.one =
        { code; cmis = StringSet.empty; debug = Parse_bytecode.Debug.default_summary }
      in
      output_gen
        ~standalone:true
        ~custom_header
        ~build_info:(Build_info.create `Runtime)
        ~source_map
        (fst output_file)
        (fun ~standalone ~source_map ((_, fmt) as output_file) ->
          Pretty_print.string fmt "\n";
          Pretty_print.string fmt (Unit_info.to_string uinfo);
          output
            code
            ~check_sourcemap:false
            ~source_map
            ~standalone
            ~link:`All
            output_file
          |> sourcemap_of_info ~base:source_map_base)
  | (`Stdin | `File _) as bytecode ->
      let kind, ic, close_ic, include_dirs =
        match bytecode with
        | `Stdin -> Parse_bytecode.from_channel stdin, stdin, (fun () -> ()), include_dirs
        | `File fn ->
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
          output_gen
            ~standalone:true
            ~custom_header
            ~build_info:(Build_info.create `Exe)
            ~source_map
            (fst output_file)
            (fun ~standalone ~source_map output_file ->
              output
                code
                ~check_sourcemap:true
                ~standalone
                ~source_map
                ~link:(if linkall then `All else `Needed)
                output_file
              |> sourcemap_of_info ~base:source_map_base)
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
          output_gen
            ~standalone:false
            ~custom_header
            ~build_info:(Build_info.create `Cmo)
            ~source_map
            output_file
            (fun ~standalone ~source_map output ->
              match include_runtime with
              | true ->
                  let sm1 = output_partial_runtime ~standalone ~source_map output in
                  let sm2 = output_partial cmo code ~standalone ~source_map output in
                  sourcemap_of_infos ~base:source_map_base [ sm1; sm2 ]
              | false ->
                  output_partial cmo code ~standalone ~source_map output
                  |> sourcemap_of_info ~base:source_map_base)
      | `Cma cma when keep_unit_names ->
          (if include_runtime
           then
             let output_file =
               let gen dir = Filename.concat dir "runtime.js" in
               match output_file with
               | `Stdout, false -> gen "./"
               | `Name x, false -> gen (Filename.dirname x)
               | `Name x, true
                 when String.length x > 0 && Char.equal x.[String.length x - 1] '/' ->
                   gen x
               | `Stdout, true | `Name _, true ->
                   failwith "use [-o dirname/] or remove [--keep-unit-names]"
             in
             output_gen
               ~standalone:false
               ~custom_header
               ~build_info:(Build_info.create `Runtime)
               ~source_map
               (`Name output_file)
               (fun ~standalone ~source_map output ->
                 output_partial_runtime ~standalone ~source_map output
                 |> sourcemap_of_info ~base:source_map_base));
          List.iter cma.lib_units ~f:(fun cmo ->
              let output_file =
                match output_file with
                | `Stdout, false -> gen_unit_filename "./" cmo
                | `Name x, false -> gen_unit_filename (Filename.dirname x) cmo
                | `Name x, true
                  when String.length x > 0 && Char.equal x.[String.length x - 1] '/' ->
                    gen_unit_filename x cmo
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
              then
                Format.eprintf
                  "  parsing: %a (%s)@."
                  Timer.print
                  t1
                  (Ocaml_compiler.Cmo_format.name cmo);
              output_gen
                ~standalone:false
                ~custom_header
                ~build_info:(Build_info.create `Cma)
                ~source_map
                (`Name output_file)
                (fun ~standalone ~source_map output ->
                  output_partial ~standalone ~source_map cmo code output
                  |> sourcemap_of_info ~base:source_map_base))
      | `Cma cma ->
          let f ~standalone ~source_map output =
            let source_map_runtime =
              if not include_runtime
              then None
              else Some (output_partial_runtime ~standalone ~source_map output)
            in

            let source_map_units =
              List.map cma.lib_units ~f:(fun cmo ->
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
                  then
                    Format.eprintf
                      "  parsing: %a (%s)@."
                      Timer.print
                      t1
                      (Ocaml_compiler.Cmo_format.name cmo);
                  output_partial ~standalone ~source_map cmo code output)
            in
            let sm =
              match source_map_runtime with
              | None -> source_map_units
              | Some x -> x :: source_map_units
            in
            sourcemap_of_infos ~base:source_map_base sm
          in
          output_gen
            ~standalone:false
            ~custom_header
            ~build_info:(Build_info.create `Cma)
            ~source_map
            (fst output_file)
            f);
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
