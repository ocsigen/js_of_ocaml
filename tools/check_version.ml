open Pervasives

let parse_version s =
  let rec aux s =
    try
      let (i,rest) = Scanf.sscanf s ".%i%s" (fun i s -> i,s) in
      i::(aux rest)
    with
      | _ -> []
  in
  let (major,minor) = Scanf.sscanf s " %i%s" (fun i s -> i,s) in
  major::(aux minor)

let rec get_version_line () =
  let s = Pervasives.input_line Pervasives.stdin in
  try
    Scanf.sscanf s "version: %s" parse_version
  with
    | _ -> get_version_line ()

let check () =
  let version_to_match = parse_version (Sys.argv.(1)) in
  try
    if version_to_match <= get_version_line ()
    then exit 1
    else exit 0
  with
    | End_of_file ->
      print_endline "malformed input: no version number";
      exit 0

let () = check ()
