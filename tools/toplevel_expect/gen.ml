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

let split_on_char sep s =
  let r = ref [] in
  let j = ref (String.length s) in
  for i = String.length s - 1 downto 0 do
    if String.unsafe_get s i = sep
    then (
      r := String.sub s (i + 1) (!j - i - 1) :: !r;
      j := i)
  done;
  String.sub s 0 !j :: !r

let () =
  let version = Sys.ocaml_version in
  let maj, min =
    match split_on_char '.' version with
    | maj :: min :: _ -> int_of_string maj, int_of_string min
    | _ -> assert false
  in
  match maj, min with
  | 4, min ->
      assert (min >= 11);
      dump_file "toplevel_expect_test.ml-4.11"
  | 5, 0 | 5, 1 | 5, 2 -> dump_file "toplevel_expect_test.ml-4.11"
  | 5, _ -> dump_file "toplevel_expect_test.ml-5.3"
  | _ -> failwith ("unsupported version " ^ Sys.ocaml_version)
