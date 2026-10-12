(* TEST
 include unix;
 libwin32unix;
 {
   bytecode;
 }{
   native;
 }
*)

(* Prefer the controlling terminal. Without one (CI, editors, tools
   running the tests from a pipe), open the master side of a fresh
   pseudo-terminal through /dev/ptmx: isatty is true for it on Linux and
   macOS, and it needs no terminal attached to the process. On Windows
   the console is CONIN$. Catch every exception: the js_of_ocaml runtime
   reports a path outside any mounted device as a Sys_error rather than
   a Unix_error. *)
let console =
  let rec first_open = function
    | [] -> failwith "no tty available"
    | path :: rest -> (
        try Unix.(openfile path [O_RDWR] 0) with _ -> first_open rest)
  in
  first_open [ "/dev/tty"; "/dev/ptmx"; "CONIN$" ]
in
Printf.printf "tty = %b\n" (Unix.isatty console);
(* In_channel.isatty / Out_channel.isatty exercise caml_sys_isatty,
   a different primitive from Unix.isatty (caml_unix_isatty). *)
Printf.printf "In_channel.isatty = %b\n"
  (In_channel.isatty (Unix.in_channel_of_descr console));
Printf.printf "Out_channel.isatty = %b\n"
  (Out_channel.isatty (Unix.out_channel_of_descr console))
