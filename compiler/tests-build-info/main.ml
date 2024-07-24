let version =
  match Build_info.V1.version () with
  | None -> "unknown"
  | Some v -> Build_info.V1.Version.to_string v

let () =
  print_endline "Version:";
  match String.split_on_char '-' version with
  | [ tag; plus; _commit; dirty ] ->
      Printf.printf "%s-%s-%s-%s\n%!" tag plus "xxxxx" dirty
  | [ tag; plus; _commit ] -> Printf.printf "%s-%s-%s\n%!" tag plus "xxxxx"
  | [ x ] -> print_endline x
  | _ -> Printf.printf "unexpected: %s\n%!" version

let () =
  print_endline "Sites:";
  List.iter print_endline Mysites.Sites.tests
