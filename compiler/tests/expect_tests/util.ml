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
module Jsoo = Js_of_ocaml_compiler

module Format: Format_intf.S = struct
  type ocaml_source = string
  type js_source = string

  type ocaml_file = string
  type js_file = string
  type cmo_file = string
  type bc_file = string

  let read_file file =
    let channel_to_string c_in =
      let good_round_number = 1024 in
      let buffer = Buffer.create good_round_number in
      let rec loop () =
        Buffer.add_channel buffer c_in good_round_number;
        loop ()
      in
      (try loop () with End_of_file -> ());
      Buffer.contents buffer
    in
    let channel = open_in file in
    let res = channel_to_string channel in
    close_in channel;
    res

  let write_file ~suffix contents =
    let temp_file = Filename.temp_file "jsoo_test" suffix in
    let channel = open_out temp_file in
    Printf.fprintf channel "%s" contents;
    close_out channel;
    temp_file

  let read_js = read_file
  let read_ocaml = read_file

  let write_js = write_file ~suffix:".js"
  let write_ocaml = write_file ~suffix:".ml"

  let id x = x

  let js_source_of_string = id
  let ocaml_source_of_string = id
  let string_of_js_source = id
  let string_of_ocaml_source = id

  let path_of_ocaml_file = id
  let path_of_js_file = id
  let path_of_cmo_file = id
  let path_of_bc_file = id

  let ocaml_file_of_path = id
  let js_file_of_path = id
  let cmo_file_of_path = id
  let bc_file_of_path = id
end

let parse_js file =
  let open Jsoo.Parse_js in
  file
  |> Format.read_js
  |> Format.string_of_js_source
  |> lexer_from_string
  |> parse

let channel_to_string c_in =
  let good_round_number = 1024 in
  let buffer = Buffer.create good_round_number in
  let rec loop () =
    Buffer.add_channel buffer c_in good_round_number;
    loop ()
  in
  (try loop () with End_of_file -> ());
  Buffer.contents buffer

let exec_to_string_exn ~cmd =
  let proc_result_ok std_out =
    let open Unix in
    function
    | WEXITED 0 -> ()
    | WEXITED i -> print_endline std_out; failwith (Stdlib.Format.sprintf "process exited with error code %d" i)
    | WSIGNALED i -> print_endline std_out; failwith (Stdlib.Format.sprintf "process signaled with signal number %d" i)
    | WSTOPPED i -> print_endline std_out; failwith (Stdlib.Format.sprintf "process stopped with signal number %d" i)
  in
  let proc_in = Unix.open_process_in cmd in
  let results = channel_to_string proc_in in
  proc_result_ok results (Unix.close_process_in proc_in);
  results

let run_javascript file =
  exec_to_string_exn ~cmd:(Stdlib.Format.sprintf "node %s" (Format.path_of_js_file file))

let compile_to_javascript ~pretty file =
  let out_file = Filename.temp_file "jsoo_test" ".js" in
  let extra_args = if pretty then "--pretty" else "" in
  let cmd =
    (Stdlib.Format.sprintf "../../js_of_ocaml.exe %s %s -o %s" extra_args file out_file) in
  let stdout = exec_to_string_exn ~cmd in
  print_string stdout;
  (* this print shouldn't do anything, so if something weird happens, we'll get the results
     here *)
  Format.js_file_of_path out_file

let compile_bc_to_javascript ?(pretty=true) file =
  Format.path_of_bc_file file
  |> compile_to_javascript ~pretty

let compile_cmo_to_javascript ?(pretty=true) file =
  Format.path_of_cmo_file file
  |> compile_to_javascript ~pretty

let compile_ocaml_to_cmo file =
  let out_file = Filename.temp_file "jsoo_test" ".cmo" in
  let _ = exec_to_string_exn ~cmd:(
    Stdlib.Format.sprintf "ocamlfind ocamlc -c -g %s -o %s" (Format.path_of_ocaml_file file)
      out_file) in
  Format.cmo_file_of_path out_file

let compile_ocaml_to_bc file =
  let out_file = Filename.temp_file "jsoo_test" ".bc" in
  let _ = exec_to_string_exn ~cmd:(
    Stdlib.Format.sprintf "ocamlfind ocamlc -g -linkpkg -package unix %s -o %s" (Format.path_of_ocaml_file file)
      out_file) in
  Format.bc_file_of_path out_file

type find_result =

  { expressions : Jsoo.Javascript.expression list
  ; statements : Jsoo.Javascript.statement list
  ; var_decls : Jsoo.Javascript.variable_declaration list }

type finder_fun =
  { expression : Jsoo.Javascript.expression -> unit
  ; statement : Jsoo.Javascript.statement -> unit
  ; variable_decl : Jsoo.Javascript.variable_declaration -> unit }

class finder ff =
  object
    inherit Jsoo.Js_traverse.map as super

    method! variable_declaration v =
      ff.variable_decl v;
      super#variable_declaration v

    method! expression x =
      ff.expression x;
      super#expression x

    method! statement s =
      ff.statement s;
      super#statement s
  end

let find_javascript
    ?(expression = fun _ -> false)
    ?(statement = fun _ -> false)
    ?(var_decl = fun _ -> false)
    program =
  let expressions, statements, var_decls = ref [], ref [], ref [] in
  let append r v = r := v :: !r in
  let expression a = if expression a then append expressions a in
  let statement a = if statement a then append statements a in
  let variable_decl a = if var_decl a then append var_decls a in
  let t = {expression; statement; variable_decl} in
  let trav = new finder t in
  ignore (trav#program program);
  {statements = !statements; expressions = !expressions; var_decls = !var_decls}

let expression_to_string ?(compact = false) e =
  let module J = Jsoo.Javascript in
  let e = [J.Statement (J.Expression_statement e), J.N] in
  let buffer = Buffer.create 17 in
  let pp = Jsoo.Pretty_print.to_buffer buffer in
  Jsoo.Pretty_print.set_compact pp compact;
  Jsoo.Js_output.program pp e;
  Buffer.contents buffer

let print_var_decl program n =
  let {var_decls; _} =
    find_javascript
      ~var_decl:(function
        | Jsoo.Javascript.S {name; _}, _ when name = n -> true
        | _ -> false)
      program
  in
  print_string (Stdlib.Format.sprintf "var %s = " n);
  match var_decls with
  | [(_, Some (expression, _))] -> print_string (expression_to_string expression)
  | _ -> print_endline "not found"
