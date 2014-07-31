(* #use "topfind" *)
(* #require "findlib" *)
(* #require "js_of_ocaml.compiler" *)
(* #require "compiler-libs.common" *)
let verbose = ref false
let prefix = ref "/cmis"
let output = ref None
let rec scan_args acc = function
  | "-verbose"::xs -> verbose:=true; scan_args acc xs
  | "-prefix"::y :: xs -> prefix := y; scan_args acc xs
  | "-o"::name::xs -> output := Some name; scan_args acc xs
  | x :: xs -> scan_args (x::acc) xs
  | [] -> List.rev acc

let cma_contains filename =
  let ic = open_in_bin filename in
  let len_magic_number = String.length Config.cma_magic_number in
  let magic_number = String.create len_magic_number in
  really_input ic magic_number 0 len_magic_number;
  if magic_number <> Config.cma_magic_number
  then failwith "not a cma file";
  let toc_pos = input_binary_int ic in
  seek_in ic toc_pos;
  let lib = (input_value ic : Cmo_format.library) in
  close_in ic;
  List.map (fun u -> u.Cmo_format.cu_name) lib.Cmo_format.lib_units

let fetch pkg =
  try
    let dir = Findlib.package_directory pkg in
    let fs = ref [] in
    let add name filename = fs:=(Filename.concat !prefix name,filename)::!fs in
    let read_cmi unit =
      let with_name file =
        let cmi_path = Filename.concat dir file ^ ".cmi" in
        if Sys.file_exists cmi_path
        then
          begin
            if !verbose then Format.eprintf "include %s@." cmi_path;
            add (Filename.basename cmi_path) cmi_path
          end
        else raise Not_found
      in
      try with_name (String.uncapitalize unit)
      with Not_found ->
        try with_name (String.capitalize unit)
        with Not_found ->
          Format.eprintf "Not_found: %s in %s@." unit dir
    in
    let archive = Findlib.package_property ["byte"] pkg "archive" in
    let l = Compiler.Util.split_char ' ' archive in
    List.iter (fun x ->
        if Filename.check_suffix x ".cmo"
        then
          let u = Filename.chop_suffix x ".cmo" in
          read_cmi u
        else if Filename.check_suffix x ".cma"
        then
          let cma_path = Filename.concat dir x in
          let contains = cma_contains cma_path in
          List.iter (read_cmi) contains
        else if Filename.check_suffix x ".cmi"
        then read_cmi (Filename.chop_suffix x ".cmi")
        else Format.eprintf "Wrong extention for archive %s@." x
      ) l;
    !fs
  with exn -> Format.eprintf "Error for package %s@." pkg;
    raise exn
let args =
  let args = List.tl (Array.to_list (Sys.argv)) in
  let args = scan_args [] args in
  let all = List.fold_left (fun fs pkg -> fetch pkg @ fs ) [] args in
  let program = Compiler.PseudoFs.program_of_files all in
  let oc = match !output,args with
    | Some x,_ -> open_out x
    | None, [x] -> open_out (x ^ ".cmis.js")
    | None,_ -> failwith "no output" in
  let pfs_fmt = Compiler.Pretty_print.to_out_channel oc in
  Compiler.Driver.f pfs_fmt (fun _ -> None) program
