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

open! Stdlib

let expand_path exts real virt =
  let rec loop realfile virtfile acc =
    if try Sys.is_directory realfile with _ -> false
    then
      let l = Array.to_list (Sys.readdir realfile) |> List.sort ~cmp:String.compare in
      List.fold_left l ~init:acc ~f:(fun acc s ->
          loop (Filename.concat realfile s) (Filename.concat virtfile s) acc)
    else
      try
        let exmatch =
          try
            let b = Filename.basename realfile in
            let i = String.rindex b '.' in
            let e = String.sub b ~pos:(i + 1) ~len:(String.length b - i - 1) in
            List.mem e ~set:exts
          with Not_found -> List.mem "" ~set:exts
        in
        if List.is_empty exts || exmatch then (virtfile, realfile) :: acc else acc
      with exc ->
        warn "ignoring %s: %s@." realfile (Printexc.to_string exc);
        acc
  in
  loop real virt []

let list_files name paths =
  let name, virtname =
    match String.lsplit2 name ~on:':' with
    | Some (src, dest) ->
        if String.length dest > 0 && not (Char.equal dest.[0] '/')
        then failwith (Printf.sprintf "path '%s' for file '%s' must be absolute" dest src);
        let virtname =
          if Char.equal dest.[String.length dest - 1] '/'
          then dest ^ Filename.basename src
          else dest
        in
        src, virtname
    | None ->
        (* by default, files are store in /static/ directory *)
        name, "/static/" ^ Filename.basename name
  in
  let name, exts (* extensions filter *) =
    match String.lsplit2 name ~on:'=' with
    | Some (name, exts) -> name, String.split_char ~sep:',' exts
    | None -> name, []
  in
  let file =
    if Filename.is_relative name
    then
      try Findlib.find_in_findlib_paths paths name
      with Not_found -> failwith (Printf.sprintf "file '%s' not found" name)
    else name
  in
  expand_path exts file virtname

let find_cmi paths base =
  let name, filename =
    try
      let name = String.uncapitalize_ascii base ^ ".cmi" in
      name, Findlib.find_in_findlib_paths paths name
    with Not_found ->
      let name = String.capitalize_ascii base ^ ".cmi" in
      name, Findlib.find_in_findlib_paths paths name
  in
  Filename.concat "/static/cmis" name, filename

let instr_of_name_content prim ~name ~content =
  let open Code in
  let prim =
    match prim with
    | `create_file -> "jsoo_create_file"
    | `create_file_extern -> "jsoo_create_file_extern"
  in
  Let
    ( Var.fresh ()
    , Prim (Extern prim, [ Pc (NativeString name); Pc (NativeString content) ]) )

let embed_file ~name ~filename =
  instr_of_name_content `create_file_extern ~name ~content:(Fs.read_file filename)

let init () = Code.(Let (Var.fresh (), Prim (Extern "caml_fs_init", [])))

let f ~prim ~cmis ~files ~paths =
  let cmi_files, missing_cmis =
    StringSet.fold
      (fun s (acc, missing) ->
        try
          let name, filename = find_cmi paths s in
          (name, Fs.read_file filename) :: acc, missing
        with Not_found -> acc, s :: missing)
      cmis
      ([], [])
  in
  if not (List.is_empty missing_cmis)
  then (
    warn "Some OCaml interface files were not found.@.";
    warn "Use [-I dir_of_cmis] option to bring them into scope@.";
    (* [`ocamlc -where`/expunge in.byte out.byte moduleA moduleB ... moduleN] *)
    List.iter missing_cmis ~f:(fun nm -> warn "  %s@." nm));
  let other_files =
    List.map files ~f:(fun f ->
        List.map (list_files f paths) ~f:(fun (name, filename) ->
            name, Fs.read_file filename))
    |> List.concat
  in
  List.map (other_files @ cmi_files) ~f:(fun (name, content) ->
      instr_of_name_content prim ~name ~content)
