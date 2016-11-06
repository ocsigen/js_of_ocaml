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

let cmis_of_package pkg =
  try
    let dir = Findlib.package_directory pkg in
    let fs = ref [] in
    let add filename = fs:=filename::!fs in
    let read_cmi unit =
      let with_name file =
        let cmi_path = Filename.concat dir file ^ ".cmi" in
        if Sys.file_exists cmi_path
        then
          begin
            (* if !verbose then Format.eprintf "include %s@." cmi_path; *)
            add cmi_path
          end
        else raise Not_found
      in
      try with_name (String.uncapitalize unit)
      with Not_found ->
        try with_name (String.capitalize unit)
        with Not_found ->
          Format.eprintf "Not_found: %s in %s@." unit dir
    in
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
          read_cmi u
        else if Filename.check_suffix x ".cma"
        then
          let cma_path = Filename.concat dir x in
          let contains = unit_of_cma cma_path in
          List.iter (read_cmi) contains
        else if Filename.check_suffix x ".cmi"
        then read_cmi (Filename.chop_suffix x ".cmi")
        else Format.eprintf "Wrong extention for archive %s@." x
      ) l;
    !fs
  with exn -> Format.eprintf "Error for package %s@." pkg;
    raise exn

let cmis_of_packages pkgs =
  List.fold_left (fun fs pkg -> cmis_of_package pkg @ fs ) [] pkgs
