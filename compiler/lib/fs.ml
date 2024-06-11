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
open! Stdlib

let rec find_in_path_rec paths name =
  match paths with
  | [] -> None
  | path :: rem ->
      let file = Filename.concat path name in
      if Sys.file_exists file then Some file else find_in_path_rec rem name

let find_in_path paths name =
  if Sys.file_exists name
  then Some name
  else if Filename.is_implicit name && not (String.equal name ".")
  then find_in_path_rec paths name
  else None

let rec concat dir filename =
  match String.drop_prefix ~prefix:"../" filename with
  | None -> Filename.concat dir filename
  | Some filename -> concat (Filename.dirname dir) filename

let absolute_path f = if Filename.is_relative f then concat (Sys.getcwd ()) f else f

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

let write_file ~name ~contents =
  let ch = open_out_bin name in
  output_string ch contents;
  close_out ch

let remove_file file = try Sys.remove file with Sys_error _ -> ()

let gen_file file f =
  let f_tmp =
    Filename.temp_file_name
      ~temp_dir:(Filename.dirname file)
      (Filename.basename file)
      ".tmp"
  in
  try
    let res = f f_tmp in
    remove_file file;
    Sys.rename f_tmp file;
    res
  with exc ->
    remove_file f_tmp;
    raise exc

let with_intermediate_file name f =
  Fun.protect ~finally:(fun () -> remove_file name) (fun () -> f name)
