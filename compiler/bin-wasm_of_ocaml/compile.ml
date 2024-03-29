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
open Wasm_of_ocaml_compiler

let times = Debug.find "times"

let debug_mem = Debug.find "mem"

let () = Sys.catch_break true

let update_sourcemap ~sourcemap_root ~sourcemap_don't_inline_content sourcemap_file =
  if Option.is_some sourcemap_root || not sourcemap_don't_inline_content
  then (
    let open Source_map in
    let source_map, mappings = Source_map_io.of_file_no_mappings sourcemap_file in
    assert (List.is_empty (Option.value source_map.sources_content ~default:[]));
    (* Add source file contents to source map *)
    let sources_content =
      if sourcemap_don't_inline_content
      then None
      else
        Some
          (List.map source_map.sources ~f:(fun file ->
               if Sys.file_exists file && not (Sys.is_directory file)
               then Some (Fs.read_file file)
               else None))
    in
    let source_map =
      { source_map with
        sources_content
      ; sourceroot =
          (if Option.is_some sourcemap_root then sourcemap_root else source_map.sourceroot)
      }
    in
    Source_map_io.to_file ?mappings source_map ~file:sourcemap_file)

let opt_with action x f =
  match x with
  | None -> f None
  | Some x -> action x (fun y -> f (Some y))

let output_gen output_file f =
  Code.Var.set_pretty true;
  Code.Var.set_stable (Config.Flag.stable_var ());
  Filename.gen_file output_file f

let link_and_optimize
    ~profile
    ~sourcemap_root
    ~sourcemap_don't_inline_content
    ~opt_sourcemap
    ~opt_sourcemap_url
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
  Fs.write_file ~name:runtime_file ~contents:Wa_runtime.wasm_runtime;
  Fs.with_intermediate_file (Filename.temp_file "wasm-merged" ".wasm")
  @@ fun temp_file ->
  opt_with
    Fs.with_intermediate_file
    (if enable_source_maps
     then Some (Filename.temp_file "wasm-merged" ".wasm.map")
     else None)
  @@ fun opt_temp_sourcemap ->
  Wa_binaryen.link
    ~runtime_files:(runtime_file :: runtime_wasm_files)
    ~input_files:wat_files
    ~opt_output_sourcemap:opt_temp_sourcemap
    ~output_file:temp_file;
  Fs.with_intermediate_file (Filename.temp_file "wasm-dce" ".wasm")
  @@ fun temp_file' ->
  opt_with
    Fs.with_intermediate_file
    (if enable_source_maps then Some (Filename.temp_file "wasm-dce" ".wasm.map") else None)
  @@ fun opt_temp_sourcemap' ->
  let primitives =
    Wa_binaryen.dead_code_elimination
      ~dependencies:Wa_runtime.dependencies
      ~opt_input_sourcemap:opt_temp_sourcemap
      ~opt_output_sourcemap:opt_temp_sourcemap'
      ~input_file:temp_file
      ~output_file:temp_file'
  in
  Wa_binaryen.optimize
    ~profile
    ~opt_input_sourcemap:opt_temp_sourcemap'
    ~opt_output_sourcemap:opt_sourcemap
    ~opt_sourcemap_url
    ~input_file:temp_file'
    ~output_file;
  Option.iter
    ~f:(update_sourcemap ~sourcemap_root ~sourcemap_don't_inline_content)
    opt_sourcemap_file;
  primitives

let build_js_runtime ~primitives ~runtime_arguments =
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
      @@ Driver.link_and_pack [ Javascript.Return_statement (Some (EObj l)), N ]
    with
    | Some x -> x
    | None -> assert false
  in
  let primitives =
    match primitives with
    | Javascript.Expression_statement e, N -> e
    | _ -> assert false
  in
  let prelude = Wa_link.output_js always_required_js in
  let init_fun =
    match Parse_js.parse (Parse_js.Lexer.of_string Wa_runtime.js_runtime) with
    | [ (Expression_statement f, _) ] -> f
    | _ -> assert false
  in
  let launcher =
    let js =
      let js = Javascript.call init_fun [ primitives ] N in
      let js = Javascript.call js [ runtime_arguments ] N in
      [ Javascript.Expression_statement js, Javascript.N ]
    in
    Wa_link.output_js js
  in
  prelude ^ launcher

let run
    { Cmd_arg.common
    ; profile
    ; runtime_files
    ; input_file
    ; output_file
    ; enable_source_maps
    ; params
    ; include_dirs
    ; sourcemap_root
    ; sourcemap_don't_inline_content
    } =
  Jsoo_cmdline.Arg.eval common;
  Wa_generate.init ();
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
        | Some t -> `Snd t
        | None -> `Fst name)
  in
  let t1 = Timer.make () in
  let builtin = Js_of_ocaml_compiler_runtime_files.runtime @ builtin in
  List.iter builtin ~f:(fun t ->
      let filename = Builtins.File.name t in
      let runtimes = Linker.Fragment.parse_builtin t in
      Linker.load_fragments
        ~ignore_always_annotation:true
        ~target_env:Target_env.Isomorphic
        ~filename
        runtimes);
  Linker.load_files
    ~ignore_always_annotation:true
    ~target_env:Target_env.Isomorphic
    runtime_js_files;
  Linker.check_deps ();
  if times () then Format.eprintf "  parsing js: %a@." Timer.print t1;
  if times () then Format.eprintf "Start parsing...@.";
  let need_debug = enable_source_maps || Config.Flag.debuginfo () in
  let output (one : Parse_bytecode.one) ~standalone ch =
    let code = one.code in
    let live_vars, in_cps, p, debug =
      Driver.f
        ~target:Wasm
        ~standalone
        ?profile
        ~linkall:false
        ~wrap_with_fun:`Iife
        one.debug
        code
    in
    let generated_js = Wa_generate.f ch ~debug ~live_vars ~in_cps p in
    if times () then Format.eprintf "compilation: %a@." Timer.print t;
    generated_js
  in
  (let kind, ic, close_ic, include_dirs =
     let ch = open_in_bin input_file in
     let res = Parse_bytecode.from_channel ch in
     let include_dirs = Filename.dirname input_file :: include_dirs in
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
       let code =
         Parse_bytecode.from_exe
           ~target:`Wasm
           ~includes:include_dirs
           ~include_cmis:false
           ~link_info:false
           ~linkall:false
           ~debug:need_debug
           ic
       in
       if times () then Format.eprintf "  parsing: %a@." Timer.print t1;
       Fs.gen_file (Filename.chop_extension output_file ^ ".wat")
       @@ fun wat_file ->
       let wasm_file =
         if Filename.check_suffix output_file ".wasm.js"
         then Filename.chop_extension output_file
         else Filename.chop_extension output_file ^ ".wasm"
       in
       Fs.gen_file wasm_file
       @@ fun tmp_wasm_file ->
       opt_with
         Fs.gen_file
         (if enable_source_maps then Some (wasm_file ^ ".map") else None)
       @@ fun opt_tmp_sourcemap ->
       let generated_js = output_gen wat_file (output code ~standalone:true) in
       let primitives =
         link_and_optimize
           ~profile
           ~sourcemap_root
           ~sourcemap_don't_inline_content
           ~opt_sourcemap:opt_tmp_sourcemap
           ~opt_sourcemap_url:
             (if enable_source_maps
              then Some (Filename.basename wasm_file ^ ".map")
              else None)
           runtime_wasm_files
           [ wat_file ]
           tmp_wasm_file
       in
       let js_runtime =
         let missing_primitives =
           let l = Wa_link.Wasm_binary.read_imports ~file:tmp_wasm_file in
           List.filter_map
             ~f:(fun { Wa_link.Wasm_binary.module_; name; _ } ->
               if String.equal module_ "env" then Some name else None)
             l
         in
         build_js_runtime
           ~primitives
           ~runtime_arguments:
             (Wa_link.build_runtime_arguments
                ~missing_primitives
                ~wasm_file
                ~generated_js)
       in
       Fs.gen_file output_file
       @@ fun tmp_output_file -> Fs.write_file ~name:tmp_output_file ~contents:js_runtime
   | `Cmo _ | `Cma _ -> assert false);
   close_ic ());
  Debug.stop_profiling ()

let info name =
  Info.make
    ~name
    ~doc:"Wasm_of_ocaml compiler"
    ~description:"Wasm_of_ocaml is a compiler from OCaml bytecode to WebAssembly."

let term = Cmdliner.Term.(const run $ Cmd_arg.options)

let command =
  let t = Cmdliner.Term.(const run $ Cmd_arg.options) in
  Cmdliner.Cmd.v (info "compile") t
