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

let () = Printf.printf {|
let s = "%s"
let git_version = "%s"
|} version git_version
