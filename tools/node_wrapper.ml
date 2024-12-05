let extra_args_for_wasoo =
  [ "--experimental-wasm-imported-strings"
  ; "--experimental-wasm-stack-switching"
  ; "--stack-size=10000"
  ]

let () =
  match Unix.getenv "PATH" with
  | exception Not_found -> assert false
  | path -> (
      let sep = if Sys.win32 then ';' else ':' in
      match String.split_on_char sep path with
      | _drop :: paths -> Unix.putenv "PATH" (String.concat (String.make 1 sep) paths)
      | [] -> assert false)

let args =
  match Array.to_list Sys.argv with
  | exe :: argv ->
      let argv =
        match argv with
        | file :: _ when Filename.check_suffix file ".wasm.js" ->
            extra_args_for_wasoo @ argv
        | _ -> argv
      in
      Array.of_list (exe :: argv)
  | [] -> assert false

let () = Unix.execvp "node" args
