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
            let i = String.rindex realfile '.' in
            let e = String.sub realfile (i+1) (String.length realfile - i - 1) in
            List.mem e exts
          with Not_found -> List.mem "" exts
        in
        if exts = [] || exmatch
        then (virtfile, realfile) :: acc
        else acc
      with exc ->
        Format.eprintf "ignoring %s: %s@." realfile (Printexc.to_string exc);
        acc
  in loop real virt []

let list_files name paths =
  let name,dir = try
      let i = String.index name ':' in
      let d = String.sub name (i + 1) (String.length name - i - 1) in
      let n = String.sub name 0 i in
      if String.length d > 0 && d.[0] <> '/'
      then failwith (Printf.sprintf "path '%s' for file '%s' must be absolute" d n);
      let d =
        if d.[String.length d - 1] <> '/'
        then d^Filename.dir_sep
        else d in
      n,d
    with Not_found ->
      (* by default, files are store in /static/ directory *)
      name,"/static/" in
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
      Util.find_in_paths paths name
    with Not_found ->
      failwith (Printf.sprintf "file '%s' not found" name)
  in
  expand_path exts file name

let cmi_dir = "/cmis"

let find_cmi paths base =
  try
    let name = String.uncapitalize base ^ ".cmi" in
    Filename.concat cmi_dir name, Util.find_in_paths paths name
  with Not_found ->
    let name = String.capitalize base ^ ".cmi" in
    Filename.concat cmi_dir name, Util.find_in_paths paths name


open Util
open Code

let read name filename =
  let content = Util.read_file filename in
  (Pc (IString name),Pc (IString content))

let make_body prim cmis files paths =
  let fs = StringSet.fold (fun s acc ->
      try
        let name, filename = find_cmi paths s in
        read name filename :: acc
      with Not_found ->
        failwith (Printf.sprintf "interface file '%s' not found" s)
    ) cmis [] in
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

let f_sep cmis files paths =
  let body = make_body "caml_fs_register_extern" cmis files paths in
  let pc = 0 in
  let blocks = AddrMap.add pc {params=[];
                          handler=None;
                          body=[];
                               branch=Stop} AddrMap.empty in
  let p = pc, blocks, pc+1 in
  Code.prepend p body
