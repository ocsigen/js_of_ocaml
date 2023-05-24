let read_file ic = really_input_string ic (in_channel_length ic)

let () =
  let () = set_binary_mode_out stdout true in
  Format.printf
    "let runtime = \"%s\"@."
    (String.escaped (read_file (open_in Sys.argv.(1))))
