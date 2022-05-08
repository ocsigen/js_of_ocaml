(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2020 Hugo Heuzard
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

open Js_of_ocaml_compiler.Stdlib

let read_file f =
  try
    let ic = open_in_bin f in
    let n = in_channel_length ic in
    let s = Bytes.create n in
    really_input ic s 0 n;
    close_in ic;
    Bytes.unsafe_to_string s
  with e ->
    failwith (Printf.sprintf "Cannot read content of %s.\n%s" f (Printexc.to_string e))

let to_ident s =
  String.map (String.uncapitalize_ascii s) ~f:(function
      | 'a' .. 'z' as c -> c
      | 'A' .. 'Z' as c -> c
      | '0' .. '9' as c -> c
      | _ -> '_')

let () =
  match Array.to_list Sys.argv with
  | [] -> assert false
  | _ :: rest ->
      let rest = List.sort_uniq ~compare:String.compare rest in
      let fragments =
        List.map rest ~f:(fun f -> f, Js_of_ocaml_compiler.Linker.Fragment.parse_file f)
      in
      (* load all files to make sure they are valid *)
      List.iter [ true; false ] ~f:(fun js_string ->
          (if js_string
          then Js_of_ocaml_compiler.Config.Flag.enable
          else Js_of_ocaml_compiler.Config.Flag.disable)
            "use-js-string";
          List.iter Js_of_ocaml_compiler.Target_env.all ~f:(fun target_env ->
              Js_of_ocaml_compiler.Linker.reset ();
              List.iter fragments ~f:(fun (filename, frags) ->
                  Js_of_ocaml_compiler.Linker.load_fragments ~target_env ~filename frags);
              let linkinfos = Js_of_ocaml_compiler.Linker.init () in
              let prov = Js_of_ocaml_compiler.Linker.get_provided () in
              let _linkinfos, missing =
                Js_of_ocaml_compiler.Linker.resolve_deps ~linkall:true linkinfos prov
              in
              Js_of_ocaml_compiler.Linker.check_deps ();
              assert (StringSet.is_empty missing)));
      (* generation *)
      List.iter fragments ~f:(fun (f, _fragments) ->
          let name = Filename.basename f in
          let content = read_file f in
          Printf.printf
            "let %s = Js_of_ocaml_compiler.Builtins.register ~name:\"%s\" ~content:\"%s\"\n"
            (to_ident (Filename.chop_extension name))
            (String.escaped name)
            (String.escaped content))
