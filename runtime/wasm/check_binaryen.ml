let min_version = 119

let () =
  let tmp = Filename.temp_file "binaryen" ".version" in
  let exit_code = Sys.command (Printf.sprintf "wasm-merge --version > %s 2>&1" tmp) in
  let line =
    if exit_code = 0
    then (
      let ic = open_in tmp in
      let l = try Some (input_line ic) with End_of_file -> None in
      close_in ic;
      l)
    else None
  in
  Sys.remove tmp;
  match line with
  | Some line -> (
      match Scanf.sscanf line "wasm-merge version %d" Fun.id with
      | version when version >= min_version -> ()
      | version ->
          Printf.eprintf
            "Error: wasm-merge version %d is too old. Binaryen >= %d is required.\n"
            version
            min_version;
          exit 1
      | exception Scanf.Scan_failure _ ->
          Printf.eprintf
            "Error: could not parse wasm-merge version from %S. Binaryen >= %d is \
             required.\n"
            line
            min_version;
          exit 1)
  | None ->
      Printf.eprintf
        "Error: wasm-merge not found. Binaryen >= %d is required.\n"
        min_version;
      exit 1
