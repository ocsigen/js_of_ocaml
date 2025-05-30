let wizard_args =
  [ "-ext:stack-switching"; "-ext:legacy-eh"; "-stack-size=2M"; "--dir=."; "--dir=/tmp" ]

let wasmtime_args =
  [ (* "-C"; "collector=null"; *) "-W=all-proposals=y"; "--dir=."; "--dir=/tmp" ]

let wasmedge_args =
  [ "--enable-gc"
  ; "--enable-exception-handling"
  ; "--enable-tail-call"
  ; "--dir=."
  ; "--dir=/tmp"
  ]

let extra_args_for_wasoo =
  [ "--experimental-wasm-imported-strings"
  ; "--experimental-wasm-stack-switching"
  ; "--experimental-wasm-exnref"
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

let environment_args () =
  List.filter
    (fun e -> not (String.contains e ','))
    (Array.to_list (Array.map (fun e -> "--env=" ^ e) env))

let wasm_file file =
  Filename.concat (Filename.chop_extension file ^ ".assets") "code.wasm"

let common_args file argv = environment_args () @ (wasm_file file :: List.tl argv)

let shell_cmd cmd argv =
  let script = List.hd argv in
  Unix.chdir (Filename.dirname script);
  cmd, Filename.basename script :: "--" :: List.tl argv

let exe, args =
  match Array.to_list Sys.argv with
  | exe :: argv ->
      let exe', argv =
        match argv with
        | file :: _ when Filename.check_suffix file ".wasm.js" -> (
            match Node_wrapper_per_engine.engine with
            | "wizard" -> "wizeng.x86-linux", wizard_args @ common_args file argv
            | "wizard-fast" -> "wizeng.x86-64-linux", wizard_args @ common_args file argv
            | "wasmtime" -> "wasmtime", wasmtime_args @ common_args file argv
            | "wasmedge" -> "wasmedge", wasmedge_args @ common_args file argv
            | ("jsc" | "d8" | "sm") as cmd -> shell_cmd cmd argv
            | _ -> "node", extra_args_for_wasoo @ argv)
        | _ -> "node", extra_args_for_jsoo @ argv
      in
      exe', Array.of_list (exe :: argv)
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
  else Unix.execvpe exe args env
