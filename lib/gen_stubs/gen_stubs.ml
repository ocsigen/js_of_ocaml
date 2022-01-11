(* Js_of_ocaml
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

module String_set = Set.Make (String)

let print_stub s =
  Printf.printf
    "void %s () {\n\
    \  fprintf(stderr, \"Unimplemented Javascript primitive %s!\\n\");\n\
    \  exit(1);\n\
     }\n"
    s
    s

let () =
  let mls = ref [] in
  let ignores = ref [] in
  Arg.parse
    [ "--ignore", String (fun x -> ignores := x :: !ignores), "" ]
    (fun ml -> if not (Filename.check_suffix ml ".pp.ml") then mls := ml :: !mls)
    "generate dummy js stubs";
  let externals = ref String_set.empty in
  let value_description _mapper desc =
    let l = List.filter (fun x -> x.[0] <> '%') desc.Parsetree.pval_prim in
    externals := List.fold_right String_set.add l !externals;
    desc
  in
  let ignores = List.fold_right String_set.add !ignores String_set.empty in
  let mapper = { Ast_mapper.default_mapper with value_description } in
  List.iter
    (fun ml ->
      let in_ = open_in ml in
      (try
         Location.input_name := ml;
         let lex = Lexing.from_channel in_ in
         let impl = Parse.implementation lex in
         let (_ : Parsetree.structure) = mapper.structure mapper impl in
         ()
       with exn -> Location.report_exception Format.std_formatter exn);
      close_in_noerr in_)
    !mls;
  let externals = String_set.diff !externals ignores in
  set_binary_mode_out stdout true;
  print_endline "#include <stdlib.h>";
  print_endline "#include <stdio.h>";
  String_set.iter print_stub externals
