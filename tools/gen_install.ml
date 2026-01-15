let rec loop fmt dir base =
  Sys.readdir dir
  |> Array.iter (fun f ->
      let fullname = Filename.concat dir f in
      let nbase = Filename.concat base f in
      if Sys.is_directory fullname
      then loop fmt fullname nbase
      else Format.fprintf fmt "  (%s as %s)\n" fullname nbase)

let () =
  let fmt = Format.std_formatter in
  Format.fprintf fmt "(\n";
  loop fmt Sys.argv.(1) "";
  Format.fprintf fmt "\n)\n";
  ()
