let read_file ic = really_input_string ic (in_channel_length ic)

(* Keep the two variables below in sync with function build_runtime in
   ../compile.ml *)

let default_flags = []

let interesting_runtimes = [ [] ]

let name_runtime standard l =
  let flags =
    List.filter_map
      (fun (k, v) ->
        match v with
        | `S s -> Some s
        | `B b -> if b then Some k else None)
      l
  in
  String.concat "-" ("runtime" :: (if standard then [ "standard" ] else flags)) ^ ".wasm"

let print_flags f flags =
  Format.fprintf
    f
    "@[<2>[ %a ]@]"
    (Format.pp_print_list
       ~pp_sep:(fun f () -> Format.fprintf f ";@ ")
       (fun f (k, v) ->
         Format.fprintf
           f
           "@[\"%s\",@ %a@]"
           k
           (fun f v ->
             match v with
             | `S s -> Format.fprintf f "Wat_preprocess.String \"%s\"" s
             | `B b ->
                 Format.fprintf f "Wat_preprocess.Bool %s" (if b then "true" else "false"))
           v))
    flags

let () =
  let () = set_binary_mode_out stdout true in
  Format.printf "open Wasm_of_ocaml_compiler@.";
  Format.printf
    "let js_runtime = \"%s\"@."
    (String.escaped (read_file (open_in_bin Sys.argv.(1))));
  Format.printf
    "let dependencies = \"%s\"@."
    (String.escaped (read_file (open_in_bin Sys.argv.(2))));
  let wat_files, runtimes =
    List.partition
      (fun f -> Filename.check_suffix f ".wat")
      (Array.to_list (Array.sub Sys.argv 3 (Array.length Sys.argv - 3)))
  in
  Format.printf
    "let wat_files = [%a]@."
    (Format.pp_print_list (fun f file ->
         Format.fprintf
           f
           "\"%s\", \"%s\"; "
           Filename.(chop_suffix (basename file) ".wat")
           (String.escaped (read_file (open_in_bin file)))))
    wat_files;
  Format.printf
    "let precompiled_runtimes = [%a]@."
    (Format.pp_print_list (fun f (standard, flags) ->
         let flags = flags @ default_flags in
         let name = name_runtime standard flags in
         match List.find_opt (fun file -> Filename.basename file = name) runtimes with
         | None -> failwith ("Missing runtime " ^ name)
         | Some file ->
             Format.fprintf
               f
               "%a, \"%s\"; "
               print_flags
               flags
               (String.escaped (read_file (open_in_bin file)))))
    (List.mapi (fun i flags -> i = 0, flags) interesting_runtimes)
