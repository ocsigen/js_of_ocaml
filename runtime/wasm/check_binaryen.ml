(* Keep in sync with compiler/lib-wasm/binaryen.ml *)
let min_version = 120

let () =
  let tmp = Filename.temp_file "binaryen" ".version" in
  let exit_code = Sys.command (Printf.sprintf "wasm-opt --version > %s 2>&1" tmp) in
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
      (* An unusable wasm-opt is treated as missing, as the compiler does *)
      match Scanf.sscanf line "wasm-opt version %d" Fun.id with
      | version when version >= min_version -> print_endline "optimized"
      | version ->
          Printf.eprintf
            "Warning: wasm-opt version %d is too old (Binaryen >= %d is needed): the \
             runtime is not optimized.\n"
            version
            min_version;
          print_endline "unoptimized"
      | exception (Scanf.Scan_failure _ | Failure _ | End_of_file) ->
          Printf.eprintf
            "Warning: could not parse wasm-opt version from %S (Binaryen >= %d is \
             needed): the runtime is not optimized.\n"
            line
            min_version;
          print_endline "unoptimized")
  | None ->
      (* Binaryen is optional: without it, the runtime is not optimized. We
         do not warn, since this is run at each build; the compiler warns
         when linking a program instead. *)
      print_endline "unoptimized"
