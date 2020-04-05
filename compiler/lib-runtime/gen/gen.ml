let read_file f =
  try
    let ic = open_in_bin f in
    let n = in_channel_length ic in
    let s = Bytes.create n in
    really_input ic s 0 n;
    close_in ic;
    Bytes.unsafe_to_string s
  with e ->
    failwith (Printf.sprintf "Cannot read content of %s.\n%s" f (Printexc.to_string e))

let () =
  match Array.to_list Sys.argv with
  | [] -> assert false
  | _ :: rest ->
      List.iter
        (fun f ->
          let name = Filename.chop_extension (Filename.basename f) in
          let content = read_file f in
          Printf.printf "let %s = \"%s\"\n" name (String.escaped content))
        rest
