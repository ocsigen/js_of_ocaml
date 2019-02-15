(* Js_of_ocaml compiler
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

let rec find_in_path paths name =
  match paths with
  | [] -> raise Not_found
  | path :: rem ->
      let file = Filename.concat path name in
      if Sys.file_exists file then file else find_in_path rem name

let find_in_path paths name =
  if name = "" || name = "."
  then raise Not_found
  else if Filename.is_relative name
  then find_in_path paths name
  else if Sys.file_exists name
  then name
  else raise Not_found

let absolute_path f =
  if Filename.is_relative f then Filename.concat (Sys.getcwd ()) f else f

let read_file f =
  try
    let ic = open_in_bin f in
    let n = in_channel_length ic in
    let s = Bytes.create n in
    really_input ic s 0 n; close_in ic; Bytes.unsafe_to_string s
  with e ->
    failwith (Printf.sprintf "Cannot read content of %s.\n%s" f (Printexc.to_string e))
