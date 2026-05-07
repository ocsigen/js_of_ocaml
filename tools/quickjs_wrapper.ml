(* Wrapper around the QuickJS-NG `qjs` binary, used by the `quickjs`
   dune profile to stand in for `node` on PATH when running the test
   suite. The binary can be overridden with the JSOO_QUICKJS_BIN env
   variable. *)

(* `--std` exposes QuickJS's `std` / `os` / `scriptArgs` globals so the
   runtime can fall back to them when Node's `process` is not present.
   `--stack-size` raises QuickJS's default 256 KiB engine stack to a
   value comparable to Node, which trips up tests that recurse deeply. *)
let extra_args = [ "--std"; "--stack-size"; "32000000" ]

let quickjs_bin = try Sys.getenv "JSOO_QUICKJS_BIN" with Not_found -> "qjs"

let env = Unix.environment ()

let args =
  match Array.to_list Sys.argv with
  | _exe :: argv -> Array.of_list ((quickjs_bin :: extra_args) @ argv)
  | [] -> assert false

let () =
  if Sys.win32
  then
    let pid =
      Unix.create_process_env quickjs_bin args env Unix.stdin Unix.stdout Unix.stderr
    in
    match Unix.waitpid [] pid with
    | _, WEXITED n -> exit n
    | _, WSIGNALED _ -> exit 9
    | _, WSTOPPED _ -> exit 9
  else Unix.execvpe quickjs_bin args env
