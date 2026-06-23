(* [Sys.command] runs a shell command and returns its exit code. It is
   implemented on the js, wasm (node) and native backends, but not under
   WASI or QuickJS (no process execution). *)

let%expect_test ("Sys.command exit code" [@when (not wasi) && not quickjs]) =
  Printf.printf "%d\n" (Sys.command "exit 42");
  Printf.printf "%d\n" (Sys.command "exit 0");
  [%expect {|
    42
    0
    |}]
