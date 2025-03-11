let extra_args_for_wasoo =
  [ "--experimental-wasm-imported-strings"
  ; "--experimental-wasm-stack-switching"
  ; "--stack-size=10000"
  ]

let extra_args_for_jsoo = []

let env = Unix.environment ()

let env =
  Array.map
    (fun e ->
      if String.starts_with ~prefix:"PATH=" e
      then
        let path_sep = if Sys.win32 then ';' else ':' in
        let path = String.sub e 5 (String.length e - 5) in
        match String.split_on_char path_sep path with
        | _drop :: paths ->
            let paths = String.concat (String.make 1 path_sep) paths in
            "PATH=" ^ paths
        | _ -> e
      else e)
    env

let args =
  match Array.to_list Sys.argv with
  | exe :: argv ->
      let argv =
        match argv with
        | file :: _ when Filename.check_suffix file ".wasm.js" ->
            extra_args_for_wasoo @ argv
        | _ -> extra_args_for_jsoo @ argv
      in
      Array.of_list (exe :: argv)
  | [] -> assert false

let () =
  if Sys.win32
  then
    let pid =
      Unix.create_process_env "node.exe" args env Unix.stdin Unix.stdout Unix.stderr
    in
    match Unix.waitpid [] pid with
    | _, WEXITED n -> exit n
    | _, WSIGNALED _ -> exit 9
    | _, WSTOPPED _ -> exit 9
  else Unix.execvpe "node" args env
