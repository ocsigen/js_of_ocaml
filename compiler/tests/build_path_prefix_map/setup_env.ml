let () =
  let pwd = Unix.getcwd () in
  let env = [Format.sprintf "BUILD_PATH_PREFIX_MAP=/root=%s" pwd] in
  let prog, args =
    match Array.to_list Sys.argv with
    | _current :: prog :: args -> prog, args
    | _ -> failwith "Specify a program to run"
  in
  (* Format.eprintf "+ %s %s\n%!" prog (String.concat " " args); *)
  let pid =
    Unix.create_process_env
      prog
      (Array.of_list (prog :: args))
      (Array.of_list env)
      Unix.stdin
      Unix.stdout
      Unix.stderr
  in
  let _pid, status = Unix.waitpid [] pid in
  match status with
  | WEXITED 0 -> exit 0
  | _ -> failwith "Unexpected"
