(* Js_of_ocaml
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

let dump_file file =
  let buf = Bytes.create 1024 in
  let ic = open_in file in
  let rec loop () =
    let len = input ic buf 0 (Bytes.length buf) in
    if len = 0
    then ()
    else (
      output stdout buf 0 len;
      loop ())
  in
  loop ()

let () =
  match Sys.ocaml_release with
  | { extra = Some (Plus, "ox"); _ } -> dump_file "toplevel_expect_test.ml-oxcaml"
  | { major; minor; _ } -> (
      match major, minor with
      | 4, min ->
          assert (min >= 11);
          dump_file "toplevel_expect_test.ml-4.11"
      | 5, 0 | 5, 1 | 5, 2 -> dump_file "toplevel_expect_test.ml-4.11"
      | 5, 3 -> dump_file "toplevel_expect_test.ml-5.3"
      | 5, 4 -> dump_file "toplevel_expect_test.ml-5.4"
      | _ -> failwith ("unsupported version " ^ Sys.ocaml_version))
