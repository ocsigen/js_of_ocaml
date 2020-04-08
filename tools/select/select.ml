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

let version_match = function
  | "default" -> true
  | version ->
      let len = min (String.length Sys.ocaml_version) (String.length version) in
      String.sub Sys.ocaml_version 0 len = version

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
  let rec select_first i =
    if i >= Array.length Sys.argv
    then failwith "select.exe failed to select a file."
    else
      let file = Sys.argv.(i) in
      match split_on_char '-' file with
      | [ _; version ] ->
          if version_match version then dump_file file else select_first (succ i)
      | _ -> invalid_arg "select.exe"
  in
  select_first 1
