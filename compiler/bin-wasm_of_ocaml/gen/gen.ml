let read_file ic = really_input_string ic (in_channel_length ic)

let () =
  let () = set_binary_mode_out stdout true in
  Format.printf
    "let js_runtime = \"%s\"@."
    (String.escaped (read_file (open_in_bin Sys.argv.(1))));
  Format.printf
    "let dependencies = \"%s\"@."
    (String.escaped (read_file (open_in_bin Sys.argv.(2))));
  let a = Array.sub Sys.argv 3 (Array.length Sys.argv - 3) in
  Format.printf
    "let wat_files = [%a]@."
    (Format.pp_print_list (fun f file ->
         Format.fprintf
           f
           "\"%s\", \"%s\"; "
           Filename.(chop_suffix (basename file) ".wat")
           (String.escaped (read_file (open_in_bin file)))))
    (Array.to_list a)
