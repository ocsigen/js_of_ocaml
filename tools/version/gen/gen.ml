let version = Sys.argv.(1)

let git_version = Sys.argv.(2)

let version =
  let ic = open_in version in
  let version = try input_line ic with End_of_file -> "" in
  close_in ic;
  version

let git_version =
  let ic = open_in git_version in
  let git_version = try input_line ic with End_of_file -> "" in
  close_in ic;
  git_version

let drop_prefix ~prefix s =
  let plen = String.length prefix in
  if plen > String.length s
  then None
  else
    try
      for i = 0 to String.length prefix - 1 do
        if not (Char.equal s.[i] prefix.[i]) then raise Exit
      done;
      Some (String.sub s plen (String.length s - plen))
    with Exit -> None

let git_version =
  match drop_prefix ~prefix:(version ^ "-") git_version with
  | Some v -> v
  | None -> git_version

let () = Printf.printf {|
let s = "%s"
let git_version = "%s"
|} version git_version
