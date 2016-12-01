(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2014 Hugo Heuzard
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

let verbose = ref false

let input_s ic size =
  let b = Bytes.create size in
  really_input ic b 0 size;
  Bytes.unsafe_to_string b

let unit_of_cma filename =
  let ic = open_in_bin filename in
  let len_magic_number = String.length Config.cma_magic_number in
  let magic_number = input_s ic len_magic_number in
  if magic_number <> Config.cma_magic_number
  then failwith "not a cma file";
  let toc_pos = input_binary_int ic in
  seek_in ic toc_pos;
  let lib = (input_value ic : Cmo_format.library) in
  close_in ic;
  List.map (fun u -> u.Cmo_format.cu_name) lib.Cmo_format.lib_units

let read_cmi ~dir cmi =
  let with_name file =
    let cmi_path =
      if Filename.is_relative file
      then Filename.concat dir file
      else file in
    if Sys.file_exists cmi_path
    then
      begin
        (* if !verbose then Format.eprintf "include %s@." cmi_path; *)
        cmi_path
      end
    else raise Not_found
  in
  try with_name (Js_of_ocaml_compiler.Util.uncapitalize_ascii cmi)
  with Not_found ->
  try with_name (Js_of_ocaml_compiler.Util.capitalize_ascii cmi)
  with Not_found ->
    Format.eprintf "Could not find cmi %s or %s in %s@."
      (Js_of_ocaml_compiler.Util.capitalize_ascii cmi)
      (Js_of_ocaml_compiler.Util.uncapitalize_ascii cmi) dir;
    raise Not_found

let cmis_of_cma ~dir cma_path =
  let cma_path =
    if Filename.is_relative cma_path
    then Filename.concat dir cma_path
    else cma_path
  in
  let contains = unit_of_cma cma_path in
  let dir = Filename.dirname cma_path in
  Js_of_ocaml_compiler.Util.filter_map (fun s -> try Some (read_cmi ~dir (s ^ ".cmi")) with Not_found -> None) contains


let cmis_of_package pkg : string list =
  try
    let dir = Findlib.package_directory pkg in
    let fs : string list ref = ref [] in
    let add filename = fs:=filename::!fs in
    let archive = try
        Findlib.package_property ["byte"] pkg "archive"
      with exc ->
        if pkg = "stdlib"
        then "stdlib.cma"
        else raise exc in

    let l = Js_of_ocaml_compiler.Util.split_char ' ' archive in
    List.iter (fun x ->
      if Filename.check_suffix x ".cmo"
      then
        let u = Filename.chop_suffix x ".cmo" in
        add (read_cmi ~dir (u ^ ".cmi"))
      else if Filename.check_suffix x ".cma"
      then List.iter add (cmis_of_cma ~dir x)
      else if Filename.check_suffix x ".cmi"
      then add (read_cmi ~dir (Filename.chop_suffix x ".cmi"))
      else Format.eprintf "Wrong extention for archive %s@." x
    ) l;
    !fs
  with exn -> Format.eprintf "Error for package %s@." pkg;
    raise exn


let kind s =
  if Filename.check_suffix s ".cmi"
  then `Cmi s
  else if Filename.check_suffix s ".cmo"
  then `Cmi s
  else if Filename.check_suffix s ".cma"
  then `Cma s
  else `Pkg s

let cmis files =
  List.fold_left (fun fs file ->
    match kind file with
    | `Pkg pkg -> cmis_of_package pkg @ fs
    | `Cmi s -> read_cmi ~dir:"." s :: fs
    | `Cma s -> cmis_of_cma ~dir:"." s @ fs) [] files
