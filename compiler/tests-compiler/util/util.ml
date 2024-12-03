(* Js_of_ocaml tests
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2019 Ty Overby
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
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
open Js_of_ocaml_compiler.Stdlib
module Jsoo = Js_of_ocaml_compiler

let exe =
  match Sys.os_type with
  | "Cygwin" | "Win32" -> fun x -> x ^ ".exe"
  | "Unix" | _ -> fun x -> x

let ocamlc = try Sys.getenv "OCAMLC" with Not_found -> exe "ocamlc"

let ocamlrun = try Sys.getenv "OCAMLRUN" with Not_found -> exe "ocamlrun"

let node = try Sys.getenv "NODE" with Not_found -> exe "node"

let js_of_ocaml_root =
  try
    let dir = Sys.getenv "PROJECT_ROOT" in
    if Filename.is_relative dir then Filename.concat (Sys.getcwd ()) dir else dir
  with Not_found -> (
    let regex_text = "_build" in
    let regex = Str.regexp regex_text in
    match Sys.getcwd () |> Str.split regex with
    | left :: _ :: _ -> Filename.concat (Filename.concat left regex_text) "default"
    | _ -> failwith "unable to find project root")

let prng = lazy (Random.State.make_self_init ())

let temp_file_name temp_dir prefix suffix =
  let rnd = Random.State.bits (Stdlib.Lazy.force prng) land 0xFFFFFF in
  Filename.concat temp_dir (Printf.sprintf "%s%06x%s" prefix rnd suffix)

let remove_dir =
  let rec loop_files dir handle =
    match Unix.readdir handle with
    | ".." | "." -> loop_files dir handle
    | f ->
        let dir_or_file = Filename.concat dir f in
        if Sys.is_directory dir_or_file
        then remove_dir dir_or_file
        else Sys.remove dir_or_file;
        loop_files dir handle
    | exception End_of_file -> ()
  and remove_dir dir =
    let handle = Unix.opendir dir in
    loop_files dir handle;
    Unix.closedir handle;
    Unix.rmdir dir
  in
  remove_dir

let with_temp_dir ~f =
  let old_cwd = Sys.getcwd () in
  let temp = Filename.get_temp_dir_name () in
  let dir = temp_file_name temp "jsoo-test" "" in
  Unix.mkdir dir 0o700;
  Sys.chdir dir;
  let x = f () in
  Sys.chdir old_cwd;
  remove_dir dir;
  x

module Filetype : Filetype_intf.S = struct
  type ocaml_text = string

  type js_text = string

  type sourcemap_text = string

  type ocaml_file = string

  type sourcemap_file = string

  type js_file = string

  type cmo_file = string

  type bc_file = string

  let read_file file =
    let ic = open_in_bin file in
    let n = in_channel_length ic in
    let s = Bytes.create n in
    really_input ic s 0 n;
    close_in ic;
    Bytes.unsafe_to_string s

  let write_file name contents =
    let channel = open_out name in
    Printf.fprintf channel "%s" contents;
    close_out channel

  let read_js = read_file

  let read_map = read_file

  let read_ocaml = read_file

  let write_js ~name content =
    write_file name content;
    name

  let write_ocaml ~name content =
    write_file name content;
    name

  let id x = x

  let js_text_of_string = id

  let ocaml_text_of_string = id

  let string_of_js_text = id

  let string_of_map_text = id

  let string_of_ocaml_text = id

  let path_of_ocaml_file = id

  let path_of_js_file = id

  let path_of_map_file = id

  let path_of_cmo_file = id

  let path_of_bc_file = id

  let ocaml_file_of_path = id

  let js_file_of_path = id

  let map_file_of_path = id

  let cmo_file_of_path = id

  let bc_file_of_path = id
end

let parse_js file =
  let content = file |> Filetype.read_js |> Filetype.string_of_js_text in
  let filename = Filetype.path_of_js_file file in
  try Jsoo.Parse_js.Lexer.of_string ~filename content |> Jsoo.Parse_js.parse
  with Jsoo.Parse_js.Parsing_error pi as e ->
    Printf.eprintf "failed to parse %s:%d:%d\n%s\n" filename pi.line pi.col content;
    raise e

let channel_to_string c_in =
  let good_round_number = 1024 in
  let buffer = Buffer.create good_round_number in
  let rec loop () =
    Buffer.add_channel buffer c_in good_round_number;
    loop ()
  in
  (try loop () with End_of_file -> ());
  Buffer.contents buffer

let exec_to_string_exn ?input ~fail ~cmd () =
  let build_path_prefix_map = "BUILD_PATH_PREFIX_MAP=" in
  let cwd = Sys.getcwd () in
  let build_path =
    let open Js_of_ocaml_compiler.Build_path_prefix_map in
    encode_map [ Some { target = "/dune-root"; source = cwd } ]
  in
  let env =
    let prev, env =
      Unix.environment ()
      |> Array.to_list
      |> List.partition ~f:(String.is_prefix ~prefix:build_path_prefix_map)
    in
    let prev =
      List.filter_map ~f:(String.drop_prefix ~prefix:build_path_prefix_map) prev
    in
    let build_path =
      build_path_prefix_map ^ String.concat ~sep:":" (build_path :: prev)
    in
    Array.of_list (build_path :: env)
  in
  let proc_result_ok std_out =
    let open Unix in
    function
    | WEXITED 0 -> std_out
    | WEXITED i ->
        Format.sprintf "%s\nprocess exited with error code %d\n %s\n" std_out i cmd
    | WSIGNALED i ->
        Format.sprintf "%s\nprocess signaled with signal number %d\n %s\n" std_out i cmd
    | WSTOPPED i ->
        Format.sprintf "%s\nprocess stopped with signal number %d\n %s\n" std_out i cmd
  in
  let ((proc_in, oc, proc_err) as proc_full) = Unix.open_process_full cmd env in
  (match input with
  | None -> ()
  | Some s ->
      output_string oc s;
      close_out oc);
  let results = channel_to_string proc_in in
  let results' = channel_to_string proc_err in
  let exit_status = Unix.close_process_full proc_full in
  let res =
    proc_result_ok
      (String.concat
         ~sep:"\n"
         (List.filter
            ~f:(function
              | "" -> false
              | _ -> true)
            [ results'; results ]))
      exit_status
  in
  match exit_status with
  | WEXITED n when n <> 0 && fail ->
      print_endline res;
      raise_notrace (Failure "non-zero exit code")
  | _ -> res

let run_javascript file =
  exec_to_string_exn
    ~fail:false
    ~cmd:(Format.sprintf "%s %s" node (Filetype.path_of_js_file file))
    ()

let check_javascript file =
  exec_to_string_exn
    ~fail:false
    ~cmd:(Format.sprintf "%s --check %s" node (Filetype.path_of_js_file file))
    ()

let check_javascript_source source =
  exec_to_string_exn ~input:source ~fail:false ~cmd:(Format.sprintf "%s --check" node) ()

let run_bytecode file =
  exec_to_string_exn
    ~fail:false
    ~cmd:(Format.sprintf "%s %s" ocamlrun (Filetype.path_of_bc_file file))
    ()

let swap_extention filename ~ext =
  Format.sprintf "%s.%s" (Filename.remove_extension filename) ext

let input_lines file =
  let rec loop acc ic =
    match input_line ic with
    | line -> loop (line :: acc) ic
    | exception End_of_file -> List.rev acc
  in
  let ic = open_in file in
  let lines = loop [] ic in
  close_in ic;
  lines

let print_file file =
  let open Js_of_ocaml_compiler.Stdlib in
  let all = input_lines file in
  Printf.printf "$ cat %S\n" file;
  List.iteri all ~f:(fun i line -> Printf.printf "%3d: %s\n" (i + 1) line)

let extract_sourcemap file =
  let open Js_of_ocaml_compiler.Stdlib in
  let lines =
    input_lines (Filetype.path_of_js_file file)
    |> List.filter_map ~f:(String.drop_prefix ~prefix:"//# sourceMappingURL=")
  in
  match lines with
  | [ line ] ->
      let content =
        match String.drop_prefix ~prefix:"data:application/json;base64," line with
        | None -> String.concat ~sep:"\n" (input_lines line)
        | Some base64 -> Js_of_ocaml_compiler.Base64.decode_exn base64
      in
      Some (Js_of_ocaml_compiler.Source_map.of_string content)
  | _ -> None

let compile_to_javascript
    ?(flags = [])
    ?(use_js_string = false)
    ?(effects = false)
    ~pretty
    ~sourcemap
    file =
  let out_file = swap_extention file ~ext:"js" in
  let extra_args =
    List.flatten
      [ (if pretty then [ "--pretty" ] else [])
      ; (if sourcemap then [ "--sourcemap" ] else [])
      ; (if effects then [ "--enable=effects" ] else [ "--disable=effects" ])
      ; (if use_js_string
         then [ "--enable=use-js-string" ]
         else [ "--disable=use-js-string" ])
      ; flags
      ]
  in
  let extra_args = String.concat ~sep:" " extra_args in
  let compiler_location =
    Filename.concat js_of_ocaml_root "compiler/bin-js_of_ocaml/js_of_ocaml.exe"
  in
  let cmd = Format.sprintf "%s %s %s -o %s" compiler_location extra_args file out_file in

  let stdout = exec_to_string_exn ~fail:true ~cmd () in
  print_string stdout;
  (* this print shouldn't do anything, so if
     something weird happens, we'll get the results here *)
  let jsfile = Filetype.js_file_of_path out_file in
  let stdout = check_javascript jsfile in
  print_string stdout;
  (* this print shouldn't do anything, so if
     something weird happens, we'll get the results here *)
  jsfile

let jsoo_minify ?(flags = []) ~pretty file =
  let file = Filetype.path_of_js_file file in
  let out_file = swap_extention file ~ext:"min.js" in
  let extra_args = List.flatten [ (if pretty then [ "--pretty" ] else []); flags ] in
  let extra_args = String.concat ~sep:" " extra_args in
  let compiler_location =
    Filename.concat js_of_ocaml_root "compiler/bin-jsoo_minify/jsoo_minify.exe"
  in
  let cmd = Format.sprintf "%s %s %s -o %s" compiler_location extra_args file out_file in

  let stdout = exec_to_string_exn ~fail:true ~cmd () in
  print_string stdout;
  (* this print shouldn't do anything, so if
     something weird happens, we'll get the results here *)
  Filetype.js_file_of_path out_file

let compile_bc_to_javascript
    ?flags
    ?effects
    ?use_js_string
    ?(pretty = true)
    ?(sourcemap = true)
    file =
  Filetype.path_of_bc_file file
  |> compile_to_javascript ?flags ?effects ?use_js_string ~pretty ~sourcemap

let compile_cmo_to_javascript
    ?(flags = [])
    ?effects
    ?use_js_string
    ?(pretty = true)
    ?(sourcemap = true)
    file =
  Filetype.path_of_cmo_file file
  |> compile_to_javascript
       ?effects
       ?use_js_string
       ~flags:([ "--disable"; "header" ] @ flags)
       ~pretty
       ~sourcemap

let compile_ocaml_to_cmo ?(debug = true) file =
  let file = Filetype.path_of_ocaml_file file in
  let out_file = swap_extention file ~ext:"cmo" in
  let (stdout : string) =
    exec_to_string_exn
      ~fail:true
      ~cmd:
        (Format.sprintf
           "%s -c %s %s -o %s"
           ocamlc
           (if debug then "-g" else "")
           file
           out_file)
      ()
  in
  print_string stdout;
  Filetype.cmo_file_of_path out_file

let compile_ocaml_to_bc ?(debug = true) ?(unix = false) file =
  let file = Filetype.path_of_ocaml_file file in
  let out_file = swap_extention file ~ext:"bc" in
  let (stdout : string) =
    exec_to_string_exn
      ~fail:true
      ~cmd:
        (Format.sprintf
           "%s -no-check-prims %s %s %s -o %s"
           ocamlc
           (if debug then "-g" else "")
           (if unix then "-I +unix unix.cma" else "")
           file
           out_file)
      ()
  in
  print_string stdout;
  Filetype.bc_file_of_path out_file

let compile_lib list name =
  let out_file = swap_extention name ~ext:"cma" in
  let (stdout : string) =
    exec_to_string_exn
      ~fail:true
      ~cmd:
        (Format.sprintf
           "%s -g -a %s -o %s"
           ocamlc
           (String.concat ~sep:" " (List.map ~f:Filetype.path_of_cmo_file list))
           out_file)
      ()
  in
  print_string stdout;
  Filetype.cmo_file_of_path out_file

let program_to_string ?(compact = false) p =
  let buffer = Buffer.create 17 in
  let pp = Jsoo.Pretty_print.to_buffer buffer in
  Jsoo.Pretty_print.set_compact pp compact;
  let (_ : Jsoo.Source_map.info) = Jsoo.Js_output.program pp p in
  (* This final comment should help to keep merge-confict inside
     {| .. |}, allowing to resolve confict with [dune promote]. *)
  Buffer.add_string buffer "//end\n";
  Buffer.contents buffer

let expression_to_string ?(compact = false) e =
  let module J = Jsoo.Javascript in
  let p = [ J.Expression_statement e, J.N ] in
  program_to_string ~compact p

class find_variable_declaration r n =
  object
    inherit Jsoo.Js_traverse.map as super

    method! variable_declaration k v =
      (match v with
      | DeclIdent (Jsoo.Javascript.S { name = Utf8 name; _ }, _) when String.equal name n
        -> r := v :: !r
      | _ -> ());
      super#variable_declaration k v
  end

let find_variable program n =
  let r = ref [] in
  let o = new find_variable_declaration r n in
  ignore (o#program program);
  match !r with
  | [ DeclIdent (_, Some (expression, _)) ] -> expression
  | _ -> raise Not_found

let print_var_decl program n =
  let r = ref [] in
  let o = new find_variable_declaration r n in
  ignore (o#program program);
  print_string (Format.sprintf "var %s = " n);
  match !r with
  | [ DeclIdent (_, Some (expression, _)) ] ->
      print_string (expression_to_string expression)
  | _ -> print_endline "not found"

class find_function_declaration r n =
  object
    inherit Jsoo.Js_traverse.map as super

    method! statement s =
      let open Jsoo.Javascript in
      (match s with
      | Variable_statement (_, l) ->
          List.iter l ~f:(function
            | DeclIdent
                ( (S { name = Utf8 name; _ } as id)
                , Some ((EFun (_, fun_decl) | EArrow (fun_decl, _, _)), _) ) -> (
                let fd = id, fun_decl in
                match n with
                | None -> r := fd :: !r
                | Some n -> if String.equal name n then r := fd :: !r else ())
            | _ -> ())
      | Function_declaration (name, fun_decl) -> (
          match name, n with
          | _, None -> r := (name, fun_decl) :: !r
          | S { name = Utf8 s; _ }, Some n ->
              if String.equal s n then r := (name, fun_decl) :: !r else ()
          | _ -> ())
      | _ -> ());
      super#statement s
  end

let print_program p = print_string (program_to_string p)

let find_function program n =
  let r = ref [] in
  let o = new find_function_declaration r (Some n) in
  ignore (o#program program);
  match !r with
  | [ (_, fd) ] -> fd
  | _ -> raise Not_found

let print_fun_decl program n =
  let r = ref [] in
  let o = new find_function_declaration r n in
  ignore (o#program program);
  let module J = Jsoo.Javascript in
  match !r with
  | [ (n, fd) ] ->
      print_string (program_to_string [ J.Function_declaration (n, fd), J.N ])
  | [] -> print_endline "not found"
  | l -> print_endline (Format.sprintf "%d functions found" (List.length l))

let compile_and_run_bytecode ?unix s =
  with_temp_dir ~f:(fun () ->
      s
      |> Filetype.ocaml_text_of_string
      |> Filetype.write_ocaml ~name:"test.ml"
      |> compile_ocaml_to_bc ?unix
      |> run_bytecode
      |> print_endline)

let compile_and_run
    ?debug
    ?pretty
    ?(skip_modern = false)
    ?(flags = [])
    ?effects
    ?use_js_string
    ?unix
    s =
  with_temp_dir ~f:(fun () ->
      let bytecode_file =
        s
        |> Filetype.ocaml_text_of_string
        |> Filetype.write_ocaml ~name:"test.ml"
        |> compile_ocaml_to_bc ?debug ?unix
      in
      let output_without_stdlib_modern =
        compile_bc_to_javascript
          ?pretty
          ~flags
          ?effects
          ?use_js_string
          ?sourcemap:debug
          bytecode_file
        |> run_javascript
      in
      print_endline output_without_stdlib_modern;
      if not skip_modern
      then
        let output_with_stdlib_modern =
          compile_bc_to_javascript
            ~flags:(flags @ [ "+stdlib_modern.js" ])
            ?effects
            ?use_js_string
            ?sourcemap:debug
            bytecode_file
          |> run_javascript
        in
        if not (String.equal output_without_stdlib_modern output_with_stdlib_modern)
        then (
          print_endline "Output was different with stdlib_modern.js:";
          print_endline "===========================================";
          print_string output_with_stdlib_modern;
          print_endline "==========================================="))

let compile_and_parse_whole_program
    ?(debug = true)
    ?pretty
    ?flags
    ?effects
    ?use_js_string
    ?unix
    s =
  with_temp_dir ~f:(fun () ->
      s
      |> Filetype.ocaml_text_of_string
      |> Filetype.write_ocaml ~name:"test.ml"
      |> compile_ocaml_to_bc ?unix ~debug
      |> compile_bc_to_javascript ?pretty ?flags ?effects ?use_js_string ~sourcemap:debug
      |> parse_js)

let compile_and_parse ?(debug = true) ?pretty ?flags ?effects ?use_js_string s =
  with_temp_dir ~f:(fun () ->
      s
      |> Filetype.ocaml_text_of_string
      |> Filetype.write_ocaml ~name:"test.ml"
      |> compile_ocaml_to_cmo ~debug
      |> compile_cmo_to_javascript ?pretty ?flags ?effects ?use_js_string ~sourcemap:debug
      |> parse_js)

let normalize_path s =
  String.map
    ~f:(function
      | '\\' -> '/' (* Normalize windows path for the tests *)
      | x -> x)
    s
