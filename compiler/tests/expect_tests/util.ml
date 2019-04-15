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
open Js_of_ocaml_compiler
open Js_of_ocaml_compiler.Stdlib

let parse_js source = Parse_js.(parse (lexer_from_string source))

let rev_prop accessor name value =
  let past = accessor () in
  if past = value
  then fun () -> ()
  else if value
  then (
    Config.Flag.enable name;
    fun () -> Config.Flag.disable name)
  else (
    Config.Flag.disable name;
    fun () -> Config.Flag.enable name)

let print_compiled_js ?(pretty = true) cmo_channel =
  let program, _, debug_data, _ =
    Parse_bytecode.from_channel ~debug:`Names cmo_channel
  in
  let buffer = Buffer.create 100 in
  let pp = Pretty_print.to_buffer buffer in
  let silence_compiler () =
    let prev = !Stdlib.quiet in
    Stdlib.quiet := true;
    fun () -> Stdlib.quiet := prev
  in
  let props =
    if pretty
    then
      Config.Flag.
        [ rev_prop shortvar "shortvar" false
        ; rev_prop pretty "pretty" true
        ; silence_compiler () ]
    else
      Config.Flag.
        [ rev_prop shortvar "shortvar" true
        ; rev_prop pretty "pretty" false
        ; silence_compiler () ]
  in
  (try Driver.f pp debug_data program
   with e ->
     List.iter props ~f:(fun f -> f ());
     raise e);
  List.iter props ~f:(fun f -> f ());
  Buffer.contents buffer

let compile_ocaml_to_bytecode source =
  let temp_file = Filename.temp_file "jsoo_test" ".ml" in
  let out = open_out temp_file in
  Printf.fprintf out "%s" source;
  close_out out;
  let prev_debug = !Clflags.debug in
  Clflags.debug := true;
  Compile.implementation Format.std_formatter temp_file temp_file;
  Clflags.debug := prev_debug;
  open_in (Format.sprintf "%s.cmo" temp_file)

type find_result =
  { expressions : Javascript.expression list
  ; statements : Javascript.statement list
  ; var_decls : Javascript.variable_declaration list }

type finder_fun =
  { expression : Javascript.expression -> unit
  ; statement : Javascript.statement -> unit
  ; variable_decl : Javascript.variable_declaration -> unit }

class finder ff =
  object
    inherit Js_traverse.map as super

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
  let e = [Javascript.Statement (Javascript.Expression_statement e), Javascript.N] in
  let buffer = Buffer.create 17 in
  let pp = Pretty_print.to_buffer buffer in
  Pretty_print.set_compact pp compact;
  Js_output.program pp e;
  Buffer.contents buffer
