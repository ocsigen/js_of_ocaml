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
    {|
void %s () {
  caml_fatal_error("Unimplemented Javascript primitive %s!");
}
|}
    s
    s

let () =
  let mls = ref [] in
  let except_mls = ref [] in
  Arg.parse
    [ "--except", Rest (fun ml -> except_mls := ml :: !except_mls), "" ]
    (fun ml -> mls := ml :: !mls)
    "generate dummy js stubs";
  let real_ml ml = not (Filename.check_suffix ml ".pp.ml") in
  let get_externals l =
    let l = List.filter real_ml l in
    let externals = ref String_set.empty in
    let value_description _mapper desc =
      let l = List.filter (fun x -> x.[0] <> '%') desc.Parsetree.pval_prim in
      externals := List.fold_right String_set.add l !externals;
      desc
    in
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
      l;
    !externals
  in
  let mls = get_externals !mls in
  let except_mls = get_externals !except_mls in
  let externals = String_set.diff mls except_mls in
  set_binary_mode_out stdout true;
  print_endline "#include <caml/misc.h>";
  String_set.iter print_stub externals
