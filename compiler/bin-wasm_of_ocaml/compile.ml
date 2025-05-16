(* Wasm_of_ocaml compiler
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
open Wasm_of_ocaml_compiler

let times = Debug.find "times"

let debug_mem = Debug.find "mem"

let debug_wat = Debug.find "wat"

let () = Sys.catch_break true

let update_sourcemap ~sourcemap_root ~sourcemap_don't_inline_content sourcemap_file =
  if Option.is_some sourcemap_root || not sourcemap_don't_inline_content
  then (
    let open Source_map in
    let source_map =
      match Source_map.of_file sourcemap_file with
      | Index _ -> assert false
      | Standard sm -> sm
    in
    assert (List.is_empty (Option.value source_map.sources_content ~default:[]));
    (* Add source file contents to source map *)
    let sources_content =
      if sourcemap_don't_inline_content
      then None
      else
        Some
          (List.map source_map.sources ~f:(fun file ->
               if String.equal file Wasm_source_map.blackbox_filename
               then
                 Some (Source_map.Source_content.create Wasm_source_map.blackbox_contents)
               else if Sys.file_exists file && not (Sys.is_directory file)
               then Some (Source_map.Source_content.create (Fs.read_file file))
               else None))
    in
    let source_map =
      { source_map with
        sources_content
      ; sourceroot =
          (if Option.is_some sourcemap_root then sourcemap_root else source_map.sourceroot)
      ; ignore_list =
          (if
             List.mem
               ~eq:String.equal
               Wasm_source_map.blackbox_filename
               source_map.sources
           then [ Wasm_source_map.blackbox_filename ]
           else [])
      }
    in
    Source_map.to_file (Standard source_map) sourcemap_file)

let opt_with action x f =
  match x with
  | None -> f None
  | Some x -> action x (fun y -> f (Some y))

let with_runtime_files ~runtime_wasm_files f =
  let inputs =
    List.map
      ~f:(fun file -> { Wat_preprocess.module_name = "env"; file; source = File })
      runtime_wasm_files
  in
  Wat_preprocess.with_preprocessed_files ~variables:[] ~inputs f

let build_runtime ~runtime_file =
  (* Keep this variables in sync with gen/gen.ml *)
  let variables =
    [ ( "effects"
      , Wat_preprocess.String
          (match Config.effects () with
          | `Jspi -> "jspi"
          | `Cps -> "cps"
          | `Disabled | `Double_translation -> assert false) )
    ]
  in
  match
    List.find_opt Runtime_files.precompiled_runtimes ~f:(fun (flags, _) ->
        assert (List.length flags = List.length variables);
        List.equal
          ~eq:(fun (k1, v1) (k2, v2) ->
            assert (String.equal k1 k2);
            Wat_preprocess.value_equal v1 v2)
          flags
          variables)
  with
  | Some (_, contents) -> Fs.write_file ~name:runtime_file ~contents
  | None ->
      let inputs =
        List.map
          ~f:(fun (module_name, contents) ->
            { Wat_preprocess.module_name
            ; file = module_name ^ ".wat"
            ; source = Contents contents
            })
          Runtime_files.wat_files
      in
      Runtime.build
        ~link_options:[ "-g" ]
        ~opt_options:[ "-g"; "-O2" ]
        ~variables
        ~allowed_imports:
          (Some
             [ "bindings"
             ; "Math"
             ; "js"
             ; "wasm:js-string"
             ; "wasm:text-encoder"
             ; "wasm:text-decoder"
             ])
        ~inputs
        ~output_file:runtime_file

let link_and_optimize
    ~profile
    ~sourcemap_root
    ~sourcemap_don't_inline_content
    ~opt_sourcemap
    runtime_wasm_files
    wat_files
    output_file =
  let opt_sourcemap_file =
    (* Check that Binaryen supports the necessary sourcemaps options (requires
       version >= 118) *)
    match opt_sourcemap with
    | Some _ when Sys.command "wasm-merge -osm foo 2> /dev/null" <> 0 -> None
    | Some _ | None -> opt_sourcemap
  in
  let enable_source_maps = Option.is_some opt_sourcemap_file in
  Fs.with_intermediate_file (Filename.temp_file "runtime" ".wasm")
  @@ fun runtime_file ->
  build_runtime ~runtime_file;
  Fs.with_intermediate_file (Filename.temp_file "wasm-merged" ".wasm")
  @@ fun temp_file ->
  opt_with
    Fs.with_intermediate_file
    (if enable_source_maps
     then Some (Filename.temp_file "wasm-merged" ".wasm.map")
     else None)
  @@ fun opt_temp_sourcemap ->
  (with_runtime_files ~runtime_wasm_files
  @@ fun runtime_inputs ->
  Binaryen.link
    ~inputs:
      ({ Binaryen.module_name = "env"; file = runtime_file; source_map_file = None }
       :: runtime_inputs
      @ List.map
          ~f:(fun (file, source_map_file) ->
            { Binaryen.module_name = "OCaml"; file; source_map_file })
          wat_files)
    ~opt_output_sourcemap:opt_temp_sourcemap
    ~output_file:temp_file
    ());
  Fs.with_intermediate_file (Filename.temp_file "wasm-dce" ".wasm")
  @@ fun temp_file' ->
  opt_with
    Fs.with_intermediate_file
    (if enable_source_maps then Some (Filename.temp_file "wasm-dce" ".wasm.map") else None)
  @@ fun opt_temp_sourcemap' ->
  let primitives =
    Binaryen.dead_code_elimination
      ~dependencies:Runtime_files.dependencies
      ~opt_input_sourcemap:opt_temp_sourcemap
      ~opt_output_sourcemap:opt_temp_sourcemap'
      ~input_file:temp_file
      ~output_file:temp_file'
  in
  Binaryen.optimize
    ~profile
    ~opt_input_sourcemap:opt_temp_sourcemap'
    ~opt_output_sourcemap:opt_sourcemap
    ~input_file:temp_file'
    ~output_file
    ();
  Option.iter
    ~f:(update_sourcemap ~sourcemap_root ~sourcemap_don't_inline_content)
    opt_sourcemap_file;
  primitives

let link_runtime ~profile runtime_wasm_files output_file =
  if List.is_empty runtime_wasm_files
  then build_runtime ~runtime_file:output_file
  else
    Fs.with_intermediate_file (Filename.temp_file "extra_runtime" ".wasm")
    @@ fun extra_runtime ->
    Fs.with_intermediate_file (Filename.temp_file "merged_runtime" ".wasm")
    @@ fun temp_file ->
    (with_runtime_files ~runtime_wasm_files
    @@ fun runtime_inputs ->
    Binaryen.link
      ~opt_output_sourcemap:None
      ~inputs:runtime_inputs
      ~output_file:temp_file
      ());
    Binaryen.optimize
      ~profile
      ~opt_input_sourcemap:None
      ~opt_output_sourcemap:None
      ~input_file:temp_file
      ~output_file:extra_runtime
      ();
    Fs.with_intermediate_file (Filename.temp_file "runtime" ".wasm")
    @@ fun runtime_file ->
    build_runtime ~runtime_file;
    Binaryen.link
      ~opt_output_sourcemap:None
      ~inputs:
        (List.map
           ~f:(fun file -> { Binaryen.module_name = "env"; file; source_map_file = None })
           [ runtime_file; extra_runtime ])
      ~output_file
      ()

let generate_prelude ~out_file =
  Filename.gen_file out_file
  @@ fun ch ->
  let code, uinfo = Parse_bytecode.predefined_exceptions () in
  let profile = Profile.O1 in
  let Driver.{ program; variable_uses; in_cps; deadcode_sentinal; _ } =
    Driver.optimize ~profile code
  in
  let context = Generate.start () in
  let _ =
    Generate.f
      ~context
      ~unit_name:(Some "prelude")
      ~live_vars:variable_uses
      ~in_cps
      ~deadcode_sentinal
      program
  in
  Generate.wasm_output ch ~opt_source_map_file:None ~context;
  uinfo.provides

let build_prelude z =
  Fs.with_intermediate_file (Filename.temp_file "prelude" ".wasm")
  @@ fun prelude_file ->
  let predefined_exceptions = generate_prelude ~out_file:prelude_file in
  Zip.add_file z ~name:"prelude.wasm" ~file:prelude_file;
  predefined_exceptions

let build_js_runtime ~primitives ?runtime_arguments () =
  let always_required_js, primitives =
    let l =
      StringSet.fold
        (fun nm l ->
          let id = Utf8_string.of_string_exn nm in
          Javascript.Property (PNI id, EVar (S { name = id; var = None; loc = N })) :: l)
        primitives
        []
    in
    match
      List.split_last
      @@ Driver.link_and_pack
           ~link:`Needed
           [ Javascript.Return_statement (Some (EObj l), N), N ]
    with
    | Some x -> x
    | None -> assert false
  in
  let primitives =
    match primitives with
    | Javascript.Expression_statement e, N -> e
    | _ -> assert false
  in
  let init_fun =
    match Parse_js.parse (Parse_js.Lexer.of_string Runtime_files.js_runtime) with
    | [ (Expression_statement f, _) ] -> f
    | _ -> assert false
  in
  let launcher =
    let js = Javascript.call init_fun [ primitives ] N in
    let js =
      match runtime_arguments with
      | None -> js
      | Some runtime_arguments -> Javascript.call js [ runtime_arguments ] N
    in
    [ Javascript.Expression_statement js, Javascript.N ]
  in
  Link.output_js (always_required_js @ launcher)

let add_source_map sourcemap_don't_inline_content z opt_source_map =
  let sm =
    match opt_source_map with
    | `File opt_file -> Option.map ~f:Source_map.of_file opt_file
    | `Source_map sm -> Some sm
  in
  Option.iter sm ~f:(fun sm ->
      Zip.add_entry z ~name:"source_map.map" ~contents:(Source_map.to_string sm);
      if not sourcemap_don't_inline_content
      then
        Wasm_source_map.iter_sources sm (fun i j file ->
            if Sys.file_exists file && not (Sys.is_directory file)
            then
              let sm = Fs.read_file file in
              Zip.add_entry
                z
                ~name:(Link.source_name i j file)
                ~contents:(Yojson.Basic.to_string (`String sm))))

let run
    { Cmd_arg.common
    ; profile
    ; runtime_only
    ; runtime_files
    ; input_file
    ; output_file
    ; enable_source_maps
    ; params
    ; include_dirs
    ; sourcemap_root
    ; sourcemap_don't_inline_content
    ; effects
    } =
  Config.set_target `Wasm;
  Jsoo_cmdline.Arg.eval common;
  Config.set_effects_backend effects;
  Generate.init ();
  let output_file = fst output_file in
  if debug_mem () then Debug.start_profiling output_file;
  List.iter params ~f:(fun (s, v) -> Config.Param.set s v);
  let t = Timer.make () in
  let include_dirs =
    List.filter_map (include_dirs @ [ "+stdlib/" ]) ~f:(fun d -> Findlib.find [] d)
  in
  let runtime_wasm_files, runtime_js_files =
    List.partition runtime_files ~f:(fun name ->
        List.exists
          ~f:(fun s -> Filename.check_suffix name s)
          [ ".wasm"; ".wat"; ".wast" ])
  in
  let runtime_js_files, builtin =
    List.partition_map runtime_js_files ~f:(fun name ->
        match Builtins.find name with
        | Some t -> Right t
        | None -> Left name)
  in
  let t1 = Timer.make () in
  let builtin = Js_of_ocaml_compiler_runtime_files.runtime @ builtin in
  List.iter builtin ~f:(fun t ->
      let filename = Builtins.File.name t in
      let runtimes = Linker.Fragment.parse_builtin t in
      Linker.load_fragments ~target_env:Target_env.Isomorphic ~filename runtimes);
  Linker.load_files ~target_env:Target_env.Isomorphic runtime_js_files;
  Linker.check_deps ();
  if times () then Format.eprintf "  parsing js: %a@." Timer.print t1;
  if times () then Format.eprintf "Start parsing...@.";
  let need_debug = enable_source_maps || Config.Flag.debuginfo () in
  let check_debug (one : Parse_bytecode.one) =
    if
      (not runtime_only)
      && enable_source_maps
      && Parse_bytecode.Debug.is_empty one.debug
      && not (Code.is_empty one.code)
    then
      warn
        "Warning: '--source-map' is enabled but the bytecode program was compiled with \
         no debugging information.\n\
         Warning: Consider passing '-g' option to ocamlc.\n\
         %!"
  in
  let profile =
    match profile with
    | Some p -> p
    | None -> Profile.O1
  in
  let output (one : Parse_bytecode.one) ~unit_name ~wat_file ~file ~opt_source_map_file =
    check_debug one;
    let code = one.code in
    let standalone = Option.is_none unit_name in
    let Driver.{ program; variable_uses; in_cps; deadcode_sentinal; _ } =
      Driver.optimize ~profile code
    in
    let context = Generate.start () in
    let toplevel_name, generated_js =
      Generate.f
        ~context
        ~unit_name
        ~live_vars:variable_uses
        ~in_cps
        ~deadcode_sentinal
        program
    in
    if standalone then Generate.add_start_function ~context toplevel_name;
    let ch = open_out_bin file in
    Generate.wasm_output ch ~opt_source_map_file ~context;
    close_out ch;
    if debug_wat ()
    then (
      let ch = open_out_bin wat_file in
      Generate.output ch ~context;
      close_out ch);
    if times () then Format.eprintf "compilation: %a@." Timer.print t;
    generated_js
  in
  (if runtime_only
   then (
     Fs.gen_file output_file
     @@ fun tmp_output_file ->
     Fs.with_intermediate_file (Filename.temp_file "wasm" ".wasm")
     @@ fun tmp_wasm_file ->
     link_runtime ~profile runtime_wasm_files tmp_wasm_file;
     let primitives =
       tmp_wasm_file
       |> (fun file -> Link.Wasm_binary.read_imports ~file)
       |> List.filter_map ~f:(fun { Link.Wasm_binary.module_; name; _ } ->
              if String.equal module_ "js" then Some name else None)
       |> StringSet.of_list
     in
     let js_runtime = build_js_runtime ~primitives () in
     let z = Zip.open_out tmp_output_file in
     Zip.add_file z ~name:"runtime.wasm" ~file:tmp_wasm_file;
     Zip.add_entry z ~name:"runtime.js" ~contents:js_runtime;
     let predefined_exceptions = build_prelude z in
     Link.add_info
       z
       ~predefined_exceptions
       ~build_info:(Build_info.create `Runtime)
       ~unit_data:[]
       ();
     Zip.close_out z)
   else
     let kind, ic, close_ic, include_dirs =
       let input_file =
         match input_file with
         | None -> assert false
         | Some f -> f
       in
       let ch = open_in_bin input_file in
       let res = Parse_bytecode.from_channel ch in
       let include_dirs = Filename.dirname input_file :: include_dirs in
       res, ch, (fun () -> close_in ch), include_dirs
     in
     let compile_cmo cmo cont =
       let t1 = Timer.make () in
       let code =
         Parse_bytecode.from_cmo ~includes:include_dirs ~debug:need_debug cmo ic
       in
       let unit_info = Unit_info.of_cmo cmo in
       let unit_name = Ocaml_compiler.Cmo_format.name cmo in
       if times () then Format.eprintf "  parsing: %a (%s)@." Timer.print t1 unit_name;
       Fs.with_intermediate_file (Filename.temp_file unit_name ".wasm")
       @@ fun tmp_wasm_file ->
       opt_with
         Fs.with_intermediate_file
         (if enable_source_maps
          then Some (Filename.temp_file unit_name ".wasm.map")
          else None)
       @@ fun opt_tmp_map_file ->
       let unit_data =
         Fs.with_intermediate_file (Filename.temp_file unit_name ".wasm")
         @@ fun input_file ->
         opt_with
           Fs.with_intermediate_file
           (if enable_source_maps
            then Some (Filename.temp_file unit_name ".wasm.map")
            else None)
         @@ fun opt_input_sourcemap ->
         let strings, fragments =
           output
             code
             ~wat_file:
               (Filename.concat (Filename.dirname output_file) (unit_name ^ ".wat"))
             ~unit_name:(Some unit_name)
             ~file:input_file
             ~opt_source_map_file:opt_input_sourcemap
         in
         Binaryen.optimize
           ~profile
           ~opt_input_sourcemap
           ~opt_output_sourcemap:opt_tmp_map_file
           ~input_file
           ~output_file:tmp_wasm_file
           ();
         { Link.unit_name; unit_info; strings; fragments }
       in
       cont unit_data unit_name tmp_wasm_file opt_tmp_map_file
     in
     (match kind with
     | `Exe ->
         let t1 = Timer.make () in
         let code =
           Parse_bytecode.from_exe
             ~includes:include_dirs
             ~include_cmis:false
             ~link_info:false
             ~linkall:false
             ~debug:need_debug
             ic
         in
         if times () then Format.eprintf "  parsing: %a@." Timer.print t1;
         Fs.with_intermediate_file (Filename.temp_file "code" ".wasm")
         @@ fun input_wasm_file ->
         let dir = Filename.chop_extension output_file ^ ".assets" in
         Link.gen_dir dir
         @@ fun tmp_dir ->
         Sys.mkdir tmp_dir 0o777;
         let opt_sourcemap =
           if enable_source_maps
           then Some (Filename.concat tmp_dir "code.wasm.map")
           else None
         in
         let opt_source_map_file =
           if enable_source_maps
           then Some (Filename.temp_file "code" ".wasm.map")
           else None
         in
         let generated_js =
           output
             code
             ~unit_name:None
             ~wat_file:(Filename.chop_extension output_file ^ ".wat")
             ~file:input_wasm_file
             ~opt_source_map_file
         in
         let tmp_wasm_file = Filename.concat tmp_dir "code.wasm" in
         let primitives =
           link_and_optimize
             ~profile
             ~sourcemap_root
             ~sourcemap_don't_inline_content
             ~opt_sourcemap
             runtime_wasm_files
             [ input_wasm_file, opt_source_map_file ]
             tmp_wasm_file
         in
         let wasm_name =
           Printf.sprintf
             "code-%s"
             (String.sub (Digest.to_hex (Digest.file tmp_wasm_file)) ~pos:0 ~len:20)
         in
         let tmp_wasm_file' = Filename.concat tmp_dir (wasm_name ^ ".wasm") in
         Sys.rename tmp_wasm_file tmp_wasm_file';
         if enable_source_maps
         then (
           Sys.rename (Filename.concat tmp_dir "code.wasm.map") (tmp_wasm_file' ^ ".map");
           Link.Wasm_binary.append_source_map_section
             ~file:tmp_wasm_file'
             ~url:(wasm_name ^ ".wasm.map"));
         let js_runtime =
           let missing_primitives =
             let l = Link.Wasm_binary.read_imports ~file:tmp_wasm_file' in
             List.filter_map
               ~f:(fun { Link.Wasm_binary.module_; name; _ } ->
                 if String.equal module_ "env" then Some name else None)
               l
           in
           build_js_runtime
             ~primitives
             ~runtime_arguments:
               (Link.build_runtime_arguments
                  ~missing_primitives
                  ~wasm_dir:dir
                  ~link_spec:[ wasm_name, None ]
                  ~separate_compilation:false
                  ~generated_js:[ None, generated_js ]
                  ())
             ()
         in
         Fs.gen_file output_file
         @@ fun tmp_output_file ->
         Fs.write_file ~name:tmp_output_file ~contents:js_runtime
     | `Cmo cmo ->
         Fs.gen_file output_file
         @@ fun tmp_output_file ->
         let z = Zip.open_out tmp_output_file in
         let compile_cmo' z cmo =
           compile_cmo cmo (fun unit_data _ tmp_wasm_file opt_tmp_map_file ->
               Zip.add_file z ~name:"code.wasm" ~file:tmp_wasm_file;
               add_source_map sourcemap_don't_inline_content z (`File opt_tmp_map_file);
               unit_data)
         in
         let unit_data = [ compile_cmo' z cmo ] in
         Link.add_info z ~build_info:(Build_info.create `Cmo) ~unit_data ();
         Zip.close_out z
     | `Cma cma ->
         Fs.gen_file output_file
         @@ fun tmp_output_file ->
         let z = Zip.open_out tmp_output_file in
         let unit_data =
           let tmp_buf = Buffer.create 10000 in
           List.fold_right
             ~f:(fun cmo cont l ->
               compile_cmo cmo
               @@ fun unit_data unit_name tmp_wasm_file opt_tmp_map_file ->
               cont ((unit_data, unit_name, tmp_wasm_file, opt_tmp_map_file) :: l))
             cma.lib_units
             ~init:(fun l ->
               Fs.with_intermediate_file (Filename.temp_file "wasm" ".wasm")
               @@ fun tmp_wasm_file ->
               let l = List.rev l in
               let source_map =
                 Wasm_link.f
                   (List.map
                      ~f:(fun (_, _, file, opt_source_map) ->
                        { Wasm_link.module_name = "OCaml"
                        ; file
                        ; code = None
                        ; opt_source_map =
                            Option.map
                              ~f:(fun f -> Source_map.Standard.of_file ~tmp_buf f)
                              opt_source_map
                        })
                      l)
                   ~output_file:tmp_wasm_file
               in
               Zip.add_file z ~name:"code.wasm" ~file:tmp_wasm_file;
               if enable_source_maps
               then
                 add_source_map sourcemap_don't_inline_content z (`Source_map source_map);
               List.map ~f:(fun (unit_data, _, _, _) -> unit_data) l)
             []
         in
         Link.add_info z ~build_info:(Build_info.create `Cma) ~unit_data ();
         Zip.close_out z);
     close_ic ());
  Debug.stop_profiling ()

let info name =
  Info.make
    ~name
    ~doc:"Wasm_of_ocaml compiler"
    ~description:"Wasm_of_ocaml is a compiler from OCaml bytecode to WebAssembly."

let options = Cmd_arg.options ()

let term = Cmdliner.Term.(const run $ options)

let command =
  let t = Cmdliner.Term.(const run $ options) in
  Cmdliner.Cmd.v (info "compile") t
