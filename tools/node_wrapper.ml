let extra_args_for_wasoo =
  [ "--experimental-wasm-imported-strings"
  ; "--experimental-wasm-stack-switching"
  ; "--stack-size=10000"
  ]

let path_sep, exe_ext = if Sys.win32 then ';', ".exe" else ':', ""

let () =
  match Unix.getenv "PATH" with
  | exception Not_found -> assert false
  | path -> (
      match String.split_on_char path_sep path with
      | _drop :: paths ->
          Unix.putenv "PATH" (String.concat (String.make 1 path_sep) paths)
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

let () = Unix.execvp ("node" ^ exe_ext) args
