let read_file ic = really_input_string ic (in_channel_length ic)

(* Keep the two variables below in sync with function build_runtime in
   ../compile.ml *)

let default_flags = []

let interesting_runtimes = [ [ "use-js-string", false ]; [ "use-js-string", true ] ]

let name_runtime l =
  let flags = List.filter_map (fun (k, v) -> if v then Some k else None) l in
  String.concat "-" ("runtime" :: (if flags = [] then [ "standard" ] else flags))
  ^ ".wasm"

let print_flags f flags =
  Format.fprintf
    f
    "@[<2>[ %a ]@]"
    (Format.pp_print_list
       ~pp_sep:(fun f () -> Format.fprintf f ";@ ")
       (fun f (k, v) ->
         Format.fprintf f "@[\"%s\",@ %s@]" k (if v then "true" else "false")))
    flags

let () =
  let () = set_binary_mode_out stdout true in
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
    (Format.pp_print_list (fun f flags ->
         let flags = flags @ default_flags in
         let name = name_runtime flags in
         match List.find_opt (fun file -> Filename.basename file = name) runtimes with
         | None -> failwith ("Missing runtime " ^ name)
         | Some file ->
             Format.fprintf
               f
               "%a, \"%s\"; "
               print_flags
               flags
               (String.escaped (read_file (open_in_bin file)))))
    interesting_runtimes
