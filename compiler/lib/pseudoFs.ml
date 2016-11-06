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

let expand_path exts real virt =
  let rec loop realfile virtfile acc =
    if try Sys.is_directory realfile with _ -> false
    then
      Array.fold_left (fun acc s ->
          loop (Filename.concat realfile s) (Filename.concat virtfile s) acc)
        acc (Sys.readdir realfile)
    else
      try
        let exmatch =
          try
            let b = Filename.basename realfile in
            let i = String.rindex b '.' in
            let e = String.sub b (i+1) (String.length b - i - 1) in
            List.mem e exts
          with Not_found -> List.mem "" exts
        in
        if exts = [] || exmatch
        then (virtfile, realfile) :: acc
        else acc
      with exc ->
	Util.warn "ignoring %s: %s@." realfile (Printexc.to_string exc);
 acc
  in loop real virt []

let list_files name paths =
  let name,virtname =
      let i = try Some (String.index name ':') with Not_found -> None in
      match i with
      | Some i ->
        let dest = String.sub name (i + 1) (String.length name - i - 1) in
        let src  = String.sub name 0 i in
        if String.length dest > 0 && dest.[0] <> '/'
        then failwith (Printf.sprintf "path '%s' for file '%s' must be absolute" dest src);
        let virtname =
          if dest.[String.length dest - 1] = '/'
          then dest ^ (Filename.basename src)
          else dest in
        src,virtname
      | None  ->
        (* by default, files are store in /static/ directory *)
        name,"/static/"^(Filename.basename name)
  in
  let name, exts (* extensions filter *) =
    try
      let i = String.index name '=' in
      let exts = String.sub name (i + 1) (String.length name - i - 1) in
      let n = String.sub name 0 i in
      let exts = Util.split_char ',' exts in
      n,exts
    with Not_found ->
      name,[] in
  let file =
    try
      Util.find_in_findlib_paths paths name
    with Not_found ->
      failwith (Printf.sprintf "file '%s' not found" name)
  in
  expand_path exts file virtname

let cmi_dir = "/cmis"

let find_cmi paths base =
  try
    let name = Util.uncapitalize_ascii base ^ ".cmi" in
    Filename.concat cmi_dir name, Util.find_in_findlib_paths paths name
  with Not_found ->
    let name = Util.capitalize_ascii base ^ ".cmi" in
    Filename.concat cmi_dir name, Util.find_in_findlib_paths paths name


open Util
open Code

let read name filename =
  let content = Util.read_file filename in
  (Pc (IString name),Pc (IString content))

let program_of_files l =
  let fs = List.map (fun (name,filename) ->
      read name filename) l in
  let body =
    List.map (fun (n, c) ->
        Let(Var.fresh (), Prim(Extern "caml_fs_register_extern", [n;c]))) fs in
  let pc = 0 in
  let blocks = AddrMap.add pc {params=[];
                               handler=None;
                               body=[];
                               branch=Stop} AddrMap.empty in
  let p = pc, blocks, pc+1 in
  Code.prepend p body

let make_body prim cmis files paths =
  let fs, missing = StringSet.fold (fun s (acc,missing) ->
      try
        let name, filename = find_cmi paths s in
        read name filename :: acc, missing
      with Not_found ->
        acc, s :: missing
  ) cmis ([],[]) in
  begin if missing <> [] then (
    Util.warn "Some OCaml interface files were not found.@.";
    Util.warn "Use [-I dir_of_cmis] option to bring them into scope@.";
    (* [`ocamlc -where`/expunge in.byte out.byte moduleA moduleB ... moduleN] *)
    List.iter (fun nm -> Util.warn "  %s@." nm) missing
  )
  end;
  let fs = List.fold_left (fun acc f ->
      let l = list_files f paths in
      List.fold_left (fun acc (n,fn) -> read n fn :: acc) acc l
    ) fs files
  in
  let body = List.map (fun (n, c) -> Let(Var.fresh (), Prim(Extern prim, [n;c]))) fs in
  body

let f p cmis files paths =
  let body = make_body "caml_fs_register" cmis files paths in
  Code.prepend p body

let f_empty cmis files paths =
  let body = make_body "caml_fs_register_extern" cmis files paths in
  let pc = 0 in
  let blocks = AddrMap.add pc {params=[];
                          handler=None;
                          body=[];
                               branch=Stop} AddrMap.empty in
  let p = pc, blocks, pc+1 in
  Code.prepend p body
