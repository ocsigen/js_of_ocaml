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

let command cmdline =
  let cmdline = String.concat ~sep:" " cmdline in
  let res = Sys.command cmdline in
  if res = 127 then raise (Sys_error cmdline);
  assert (res = 0)
(*ZZZ*)

let write_file name contents =
  let ch = open_out name in
  output_string ch contents;
  close_out ch

let remove_file filename =
  try if Sys.file_exists filename then Sys.remove filename with Sys_error _msg -> ()

let with_intermediate_file ?(keep = false) name f =
  match f name with
  | _ -> if not keep then remove_file name
  | exception e ->
      remove_file name;
      raise e

let output_gen output_file f =
  Code.Var.set_pretty true;
  Code.Var.set_stable (Config.Flag.stable_var ());
  Filename.gen_file output_file f

let common_binaryen_options =
  [ "--enable-gc"
  ; "--enable-multivalue"
  ; "--enable-exception-handling"
  ; "--enable-reference-types"
  ; "--enable-tail-call"
  ; "--enable-bulk-memory"
  ; "--enable-nontrapping-float-to-int"
  ; "--enable-strings"
  ; "-g"
  ; "-n"
  ]

let link runtime_files input_file output_file =
  command
    (("wasm-merge" :: common_binaryen_options)
    @ List.flatten
        (List.map
           ~f:(fun runtime_file -> [ Filename.quote runtime_file; "env" ])
           runtime_files)
    @ [ Filename.quote input_file; "exec"; "-o"; Filename.quote output_file ])

let dead_code_elimination in_file out_file =
  with_intermediate_file (Filename.temp_file "deps" ".json")
  @@ fun deps_file ->
  write_file deps_file Wa_runtime.dependencies;
  command
    (("wasm-metadce" :: common_binaryen_options)
    @ [ "--graph-file"
      ; Filename.quote deps_file
      ; Filename.quote in_file
      ; "-o"
      ; Filename.quote out_file
      ; ">"
      ; "/dev/null"
      ])

let optimize in_file out_file =
  command
    (("wasm-opt" :: common_binaryen_options)
    @ [ "-O3"; Filename.quote in_file; "-o"; Filename.quote out_file ])

let link_and_optimize runtime_wasm_files wat_file output_file =
  with_intermediate_file (Filename.temp_file "runtime" ".wasm")
  @@ fun runtime_file ->
  write_file runtime_file Wa_runtime.wasm_runtime;
  with_intermediate_file (Filename.temp_file "wasm-merged" ".wasm")
  @@ fun temp_file ->
  link (runtime_file :: runtime_wasm_files) wat_file temp_file;
  with_intermediate_file (Filename.temp_file "wasm-dce" ".wasm")
  @@ fun temp_file' ->
  dead_code_elimination temp_file temp_file';
  optimize temp_file' output_file

let escape_string s =
  let l = String.length s in
  let b = Buffer.create (String.length s + 2) in
  for i = 0 to l - 1 do
    let c = s.[i] in
    match c with
    (* https://github.com/ocsigen/js_of_ocaml/issues/898 *)
    | '/' when i > 0 && Char.equal s.[i - 1] '<' -> Buffer.add_string b "\\/"
    | '\000' .. '\031' | '\127' ->
        Buffer.add_string b "\\x";
        Buffer.add_char_hex b c
    | '"' ->
        Buffer.add_char b '\\';
        Buffer.add_char b c
    | c -> Buffer.add_char b c
  done;
  Buffer.contents b

let build_js_runtime wasm_file output_file =
  let wrap_in_iife ~use_strict js =
    let module J = Javascript in
    let var ident e = J.variable_declaration [ J.ident ident, (e, J.N) ], J.N in
    let expr e = J.Expression_statement e, J.N in
    let freenames =
      let o = new Js_traverse.free in
      let (_ : J.program) = o#program js in
      o#get_free
    in
    let export_shim js =
      if J.IdentSet.mem (J.ident Constant.exports_) freenames
      then
        let export_node =
          let s =
            Printf.sprintf
              {|((typeof module === 'object' && module.exports) || %s)|}
              Constant.global_object
          in
          let lex = Parse_js.Lexer.of_string s in
          Parse_js.parse_expr lex
        in
        var Constant.exports_ export_node :: js
      else js
    in
    let old_global_object_shim js =
      if J.IdentSet.mem (J.ident Constant.old_global_object_) freenames
      then
        var Constant.old_global_object_ (J.EVar (J.ident Constant.global_object_)) :: js
      else js
    in

    let efun args body = J.EFun (None, J.fun_ args body J.U) in
    let mk f =
      let js = export_shim js in
      let js = old_global_object_shim js in
      let js =
        if use_strict
        then expr (J.EStr (Utf8_string.of_string_exn "use strict")) :: js
        else js
      in
      f [ J.ident Constant.global_object_ ] js
    in
    expr (J.call (mk efun) [ J.EVar (J.ident Constant.global_object_) ] J.N)
  in
  let always_required_js =
    List.map
      Linker.((link [] (init ())).always_required_codes)
      ~f:(fun { Linker.program; _ } -> wrap_in_iife ~use_strict:false program)
  in
  let b = Buffer.create 1024 in
  let f = Pretty_print.to_buffer b in
  Pretty_print.set_compact f (not (Config.Flag.pretty ()));
  ignore (Js_output.program f always_required_js);
  let s = Wa_runtime.js_runtime in
  let rec find i =
    if String.equal (String.sub s ~pos:i ~len:4) "CODE" then i else find (i + 1)
  in
  let i = String.index s '\n' + 1 in
  let j = find 0 in
  write_file
    output_file
    (String.sub s ~pos:0 ~len:i
    ^ Buffer.contents b
    ^ String.sub s ~pos:i ~len:(j - i)
    ^ escape_string (Filename.basename wasm_file)
    ^ String.sub s ~pos:(j + 4) ~len:(String.length s - j - 4))

let run { Cmd_arg.common; profile; runtime_files; input_file; output_file; params } =
  Wa_generate.init ();
  Jsoo_cmdline.Arg.eval common;
  (match output_file with
  | name, _ when debug_mem () -> Debug.start_profiling name
  | _, _ -> ());
  List.iter params ~f:(fun (s, v) -> Config.Param.set s v);
  let t = Timer.make () in
  let include_dirs = List.filter_map [ "+stdlib/" ] ~f:(fun d -> Findlib.find [] d) in
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
      Linker.load_fragments ~target_env:Target_env.Isomorphic ~filename runtimes);
  Linker.load_files ~target_env:Target_env.Isomorphic runtime_js_files;
  Linker.check_deps ();
  if times () then Format.eprintf "  parsing js: %a@." Timer.print t1;
  if times () then Format.eprintf "Start parsing...@.";
  let need_debug = Config.Flag.debuginfo () in
  let output (one : Parse_bytecode.one) ~standalone ch =
    let code = one.code in
    let _ =
      Driver.f
        ~target:(`Wasm ch)
        ~standalone
        ?profile
        ~linkall:false
        ~wrap_with_fun:`Iife
        one.debug
        code
    in
    if times () then Format.eprintf "compilation: %a@." Timer.print t
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
       let wat_file = Filename.chop_extension (fst output_file) ^ ".wat" in
       let wasm_file = Filename.chop_extension (fst output_file) ^ ".wasm" in
       output_gen wat_file (output code ~standalone:true);
       link_and_optimize runtime_wasm_files wat_file wasm_file;
       build_js_runtime wasm_file (fst output_file)
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
