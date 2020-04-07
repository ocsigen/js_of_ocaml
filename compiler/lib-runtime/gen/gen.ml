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

let () =
  match Array.to_list Sys.argv with
  | [] -> assert false
  | _ :: rest ->
      Js_of_ocaml_compiler.Linker.load_files rest;
      let linkinfos = Js_of_ocaml_compiler.Linker.init () in
      let prov = Js_of_ocaml_compiler.Linker.get_provided () in
      let _linkinfos, missing =
        Js_of_ocaml_compiler.Linker.resolve_deps ~linkall:true linkinfos prov
      in
      assert (StringSet.is_empty missing);
      List.iter rest ~f:(fun f ->
          let name = Filename.basename f in
          let content = read_file f in
          Printf.printf
            "let %s = Store.register \"%s\" \"%s\"\n"
            (Filename.chop_extension name)
            (String.escaped name)
            (String.escaped content))
