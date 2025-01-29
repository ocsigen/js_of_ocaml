(* TEST
 include unix;
 hasunix;
 {
   bytecode;
 }{
   native;
 }
*)

open Printf

let shouldpass msg fn arg =
  try
    ignore (fn arg); printf "%s: passed (no error)\n" msg
  with Unix.Unix_error(err, _, _) ->
    printf "%s: FAILED (error %s)\n" msg (Unix.error_message err)

let shouldfail msg fn arg =
  try
    ignore (fn arg); printf "%s: FAILED (no error raised)\n" msg
  with Unix.Unix_error(err, _, _) ->
    printf "%s: passed (error raised)\n" msg

let _ =
  (* Files *)
  begin
    let fd = Unix.(openfile "file.tmp"
                            [O_WRONLY;O_CREAT;O_TRUNC;O_SHARE_DELETE] 0o666) in
    shouldpass "File 1" Unix.in_channel_of_descr fd;
    shouldpass "File 2" Unix.out_channel_of_descr fd;
    Unix.close fd
  end;
  (* Whatever is connected to standard descriptors; hopefully a terminal *)
  begin
    shouldpass "stdin" Unix.in_channel_of_descr Unix.stdin;
    shouldpass "stderr" Unix.out_channel_of_descr Unix.stderr
  end;
  (* A closed file descriptor should now fail *)
  begin
    let fd = Unix.(openfile "file.tmp"
                            [O_WRONLY;O_CREAT;O_TRUNC;O_SHARE_DELETE] 0o666) in
    Unix.close fd;
    shouldfail "Closed file 1" Unix.in_channel_of_descr fd;
    shouldfail "Closed file 2" Unix.out_channel_of_descr fd
  end;
  Sys.remove "file.tmp";
  (* Send something to stdout, but don't flush and don't close the channel.
     This tests proper auto-flushing at exit of channels created by
     Unix.out_channel_of_descr.  (PR#11384) *)
  flush stdout;
  let oc = Unix.out_channel_of_descr Unix.stdout in
  output_string oc "Test completed normally\n"
  (* End of test *)
