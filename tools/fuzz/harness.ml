(* Running commands, temporary directories, parallel campaigns and reports. *)

type target =
  | Js
  | Wasm

type config =
  { jsoo : string
  ; wasmoo : string option
  ; out : string
  ; start : int
  ; count : int
  ; jobs : int
  ; interp : target list  (** the compilers supporting [--debug interp] *)
  }

(****)

let read_file f = In_channel.with_open_bin f In_channel.input_all

let write_file f s =
  Out_channel.with_open_bin f (fun oc -> Out_channel.output_string oc s)

let rec mkdir_p d =
  if not (Sys.file_exists d)
  then (
    mkdir_p (Filename.dirname d);
    try Sys.mkdir d 0o755 with Sys_error _ when Sys.is_directory d -> ())

let rec remove_tree path =
  match Unix.lstat path with
  | { st_kind = S_DIR; _ } ->
      Array.iter (fun f -> remove_tree (Filename.concat path f)) (Sys.readdir path);
      Unix.rmdir path
  | _ -> Sys.remove path
  | exception Unix.Unix_error (ENOENT, _, _) -> ()

let with_temp_dir f =
  let dir = Filename.temp_dir "jsoo_fuzz" "" in
  Fun.protect ~finally:(fun () -> remove_tree dir) (fun () -> f dir)

let lines s = String.split_on_char '\n' s

let last n s =
  let l = String.length s in
  if l <= n then s else "..." ^ String.sub s (l - n) n

let contains s sub =
  let n = String.length sub and l = String.length s in
  let rec loop i = i + n <= l && (String.sub s i n = sub || loop (i + 1)) in
  loop 0

(****)

type status =
  | Exited of int
  | Signaled of int
  | Timeout

type result =
  { status : status
  ; stdout : string
  ; stderr : string
  }

let ok r = r.status = Exited 0

let status_to_string = function
  | Exited n -> Printf.sprintf "exit %d" n
  | Signaled n -> Printf.sprintf "signal %d" n
  | Timeout -> "timeout"

(* Optional command prefix for the compilers and node, to cap their memory
   use, for instance [systemd-run --user --scope -q -p MemoryMax=3G]. *)
let wrapper =
  lazy
    (match Sys.getenv_opt "JSOO_FUZZ_WRAPPER" with
    | None | Some "" -> []
    | Some s -> List.filter (fun s -> s <> "") (String.split_on_char ' ' s))

(* Run [prog args] in [cwd], with no input, and capture its outputs. The
   command runs in its own process group, killed on timeout. *)
let run ?(wrap = true) ?(timeout = 600.) ~cwd prog args =
  let argv = (if wrap then Lazy.force wrapper else []) @ (prog :: args) in
  let out_file = Filename.temp_file "jsoo_fuzz" ".out" in
  let err_file = Filename.temp_file "jsoo_fuzz" ".err" in
  flush_all ();
  let pid = Unix.fork () in
  (if pid = 0
   then
     try
       ignore (Unix.setsid ());
       Sys.chdir cwd;
       let null = Unix.openfile "/dev/null" [ O_RDONLY ] 0 in
       let out = Unix.openfile out_file [ O_WRONLY; O_TRUNC ] 0 in
       let err = Unix.openfile err_file [ O_WRONLY; O_TRUNC ] 0 in
       Unix.dup2 null Unix.stdin;
       Unix.dup2 out Unix.stdout;
       Unix.dup2 err Unix.stderr;
       Unix.execvp (List.hd argv) (Array.of_list argv)
     with _ -> Unix._exit 127);
  let deadline = Unix.gettimeofday () +. timeout in
  let rec wait () =
    match Unix.waitpid [ WNOHANG ] pid with
    | 0, _ ->
        if Unix.gettimeofday () > deadline
        then (
          (try Unix.kill (-pid) Sys.sigkill with Unix.Unix_error _ -> ());
          ignore (Unix.waitpid [] pid);
          Timeout)
        else (
          Unix.sleepf 0.01;
          wait ())
    | _, WEXITED n -> Exited n
    | _, (WSIGNALED n | WSTOPPED n) -> Signaled n
    | exception Unix.Unix_error (EINTR, _, _) -> wait ()
  in
  let status = wait () in
  let stdout = read_file out_file and stderr = read_file err_file in
  Sys.remove out_file;
  Sys.remove err_file;
  { status; stdout; stderr }

(* ocamlc through ocamlfind, so that packages can be used *)
let ocamlc ~cwd args = run ~wrap:false ~timeout:120. ~cwd "ocamlfind" ("ocamlc" :: args)

let node ?(native_effects = false) ~cwd file =
  run
    ~timeout:120.
    ~cwd
    "node"
    ((if native_effects then [ "--experimental-wasm-wasmfx" ] else []) @ [ file ])

(* Lines "N: text" of a test output *)
let numbered_lines s =
  List.filter_map
    (fun l ->
      match String.index_opt l ':' with
      | Some i when i > 0 && i + 1 < String.length l && l.[i + 1] = ' ' -> (
          match int_of_string_opt (String.sub l 0 i) with
          | Some n -> Some (n, String.sub l (i + 2) (String.length l - i - 2))
          | None -> None)
      | _ -> None)
    (lines s)

(****)

(* Whether the compiler supports [--debug interp] (the IR reference
   interpreter, which checks that no pass changes the program behavior) *)
let supports_interp compiler =
  with_temp_dir (fun dir ->
      write_file (Filename.concat dir "t.ml") "let () = print_string \"\"\n";
      ok (ocamlc ~cwd:dir [ "t.ml"; "-o"; "t.byte" ])
      && ok (run ~cwd:dir compiler [ "--debug"; "interp"; "t.byte"; "-o"; "t.js" ]))

(****)

(* Random compiler flags *)

let target_name = function
  | Js -> "js"
  | Wasm -> "wasm"

let compiler config target =
  match target, config.wasmoo with
  | Js, _ -> config.jsoo
  | Wasm, Some c -> c
  | Wasm, None -> assert false

(* Set from the command line: [--no-js], extra flags given to every
   compilation, and extra options disabled at random *)
let test_js = ref true

let extra_flags = ref (fun (_ : target) -> ([] : string list))

let extra_options = ref (fun (_ : target) -> ([] : string list))

let targets config =
  (if !test_js then [ Js ] else [])
  @ if Option.is_some config.wasmoo then [ Wasm ] else []

let js_disable =
  [ "inline"
  ; "staticeval"
  ; "deadcode"
  ; "globaldeadcode"
  ; "optcall"
  ; "share"
  ; "compact"
  ; "constant-sinking"
  ; "var-coalescing"
  ; "excwrap"
  ; "shortvar"
  ]

let js_enable = [ "es6"; "pretty"; "debuginfo"; "stable_var"; "callgen" ]

let js_params =
  [ "inlining-limit", [ "0"; "1"; "10"; "50"; "150"; "500"; "10000" ]
  ; "tc_depth", [ "0"; "1"; "2"; "50" ]
  ; "switch_size", [ "1"; "2"; "3"; "60" ]
  ; "lifting-threshold", [ "1"; "2"; "50" ]
  ; "lifting-baseline", [ "0"; "1"; "3" ]
  ]

let wasm_disable = [ "inline"; "deadcode"; "globaldeadcode"; "optcall" ]

let wasm_enable = [ "debuginfo"; "pretty" ]

let wasm_params =
  [ "inlining-limit", [ "0"; "1"; "10"; "50"; "150"; "500"; "10000" ]
  ; "switch_size", [ "1"; "2"; "3"; "60" ]
  ]

(* Optimization flags (not the effect mode). [exclude] lists the options
   that the test cannot handle. *)
let random_flags ?(exclude = []) ?(p = 0.15) st target =
  let disable, enable, params =
    match target with
    | Js -> js_disable, js_enable, js_params
    | Wasm -> wasm_disable, wasm_enable, wasm_params
  in
  let disable = disable @ !extra_options target in
  let flags = ref [ "--opt"; string_of_int (Rng.range st 1 3) ] in
  let add l = flags := !flags @ l in
  if Rng.chance st 0.6
  then (
    List.iter
      (fun o ->
        if (not (List.mem o exclude)) && Rng.chance st p then add [ "--disable"; o ])
      disable;
    List.iter
      (fun o ->
        if (not (List.mem o exclude)) && Rng.chance st 0.2 then add [ "--enable"; o ])
      enable;
    List.iter
      (fun (o, v) ->
        if Rng.chance st 0.3
        then add [ "--set"; Printf.sprintf "%s=%s" o (Rng.pick st v) ])
      params);
  !flags @ !extra_flags target

let effect_modes = function
  | Js -> [ "disabled"; "cps"; "double-translation" ]
  | Wasm -> [ "jspi"; "cps"; "double-translation"; "native"; "disabled" ]

(* An effect mode; [handlers]: the program installs effect handlers *)
let random_effects st ~handlers target =
  match target, handlers with
  | Js, true -> Rng.pick st [ "cps"; "double-translation" ]
  | Js, false ->
      Rng.pick st [ "disabled"; "disabled"; "disabled"; "cps"; "double-translation" ]
  | Wasm, true -> Rng.pick st [ "jspi"; "cps"; "double-translation"; "native" ]
  | Wasm, false ->
      Rng.pick st [ "jspi"; "jspi"; "cps"; "double-translation"; "native"; "disabled" ]

(****)

(* Standard library compiled once per target and effect mode, for separate
   compilation *)
let stdlib_dir =
  lazy
    (let r = run ~wrap:false ~cwd:"." "ocamlfind" [ "ocamlc"; "-where" ] in
     String.trim r.stdout)

let separate_cache config target effects =
  let dir =
    Filename.concat
      config.out
      (Printf.sprintf ".cache/%s-%s" (target_name target) effects)
  in
  let compiler = compiler config target in
  let stdlib f = Filename.concat (Lazy.force stdlib_dir) f in
  let ext =
    match target with
    | Js -> ".js"
    | Wasm -> ".wasmo"
  in
  if not (Sys.file_exists (Filename.concat dir "done"))
  then (
    mkdir_p dir;
    let effects = "--effects=" ^ effects in
    let steps =
      match target with
      | Js ->
          [ [ "build-runtime"; effects; "-o"; "runtime.js" ]
          ; [ effects; stdlib "stdlib.cma"; "-o"; "stdlib.js" ]
          ; [ effects; stdlib "std_exit.cmo"; "-o"; "std_exit.js" ]
          ]
      | Wasm ->
          [ [ "build-runtime"; effects; "-o"; "runtime.wasma" ]
          ; [ "compile"; effects; stdlib "stdlib.cma"; "-o"; "stdlib.wasma" ]
          ; [ "compile"; effects; stdlib "std_exit.cmo"; "-o"; "std_exit.wasmo" ]
          ]
    in
    List.iter
      (fun args ->
        let r = run ~timeout:1200. ~cwd:dir compiler args in
        if not (ok r)
        then
          failwith
            (Printf.sprintf
               "cannot build the standard library: %s %s\n%s"
               compiler
               (String.concat " " args)
               r.stderr))
      steps;
    write_file (Filename.concat dir "done") "");
  ( Filename.concat
      dir
      (match target with
      | Js -> "runtime.js"
      | Wasm -> "runtime.wasma")
  , Filename.concat
      dir
      ("stdlib"
      ^
      match target with
      | Js -> ".js"
      | Wasm -> ".wasma")
  , Filename.concat dir ("std_exit" ^ ext) )

(****)

(* Problems found for a seed are saved in OUT/SEED/: the sources needed to
   reproduce them and report.txt *)
(* The seeds reported on by this process. A seed can fail in several
   configurations: the first report replaces the one of an earlier run,
   the following ones are appended to it. *)
let reported = Hashtbl.create 17

let save_report config ~seed ~files ~report =
  let dir = Filename.concat config.out (string_of_int seed) in
  mkdir_p dir;
  List.iter (fun (name, contents) -> write_file (Filename.concat dir name) contents) files;
  let file = Filename.concat dir "report.txt" in
  if Hashtbl.mem reported seed
  then write_file file (read_file file ^ "\n========\n\n" ^ report)
  else (
    Hashtbl.replace reported seed ();
    write_file file report)

let print_problem ~seed msg = Printf.printf "problem: seed %d: %s\n%!" seed msg

(* Run [job seed] for each seed, in [config.jobs] processes. Each job
   returns a list of outcomes, which are counted. *)
let campaign config (job : int -> string list) =
  mkdir_p config.out;
  let counts = Hashtbl.create 17 in
  let running = Hashtbl.create 17 in
  let collect () =
    let pid, _ = Unix.wait () in
    match Hashtbl.find_opt running pid with
    | None -> ()
    | Some file ->
        Hashtbl.remove running pid;
        let outcomes =
          if Sys.file_exists file
          then (
            let l = List.filter (fun s -> s <> "") (lines (read_file file)) in
            Sys.remove file;
            l)
          else [ "crashed" ]
        in
        List.iter
          (fun o ->
            Hashtbl.replace
              counts
              o
              (1 + Option.value ~default:0 (Hashtbl.find_opt counts o)))
          outcomes
  in
  for seed = config.start to config.start + config.count - 1 do
    while Hashtbl.length running >= config.jobs do
      collect ()
    done;
    let file = Filename.temp_file "jsoo_fuzz" ".outcomes" in
    flush_all ();
    let pid = Unix.fork () in
    if pid = 0
    then (
      let outcomes =
        try job seed with e -> [ "harness error: " ^ Printexc.to_string e ]
      in
      write_file file (String.concat "\n" outcomes);
      flush_all ();
      Unix._exit 0);
    Hashtbl.replace running pid file
  done;
  while Hashtbl.length running > 0 do
    collect ()
  done;
  let l = Hashtbl.fold (fun k v acc -> (v, k) :: acc) counts [] in
  List.iter (fun (n, o) -> Printf.printf "%7d %s\n" n o) (List.sort compare l |> List.rev)
