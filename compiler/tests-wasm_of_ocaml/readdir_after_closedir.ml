(* Regression test: reading a directory handle after [Unix.closedir]
   must fail cleanly rather than parse dirents out of the freed buffer.

   In the WASI runtime, [closedir] freed the entry buffer but left the
   read position ($pos/$available) untouched.  When entries were still
   buffered -- e.g. when a single [readdir] had been performed on a
   directory with several entries -- a subsequent [readdir] computed an
   entry pointer of [buffer + pos = 0 + pos] and parsed a "dirent" out
   of low linear memory, returning a garbage file name instead of
   raising.  After the fix, the read position is reset on close, so the
   next [readdir] falls through to [fd_readdir] on the closed fd and
   fails with [EBADF]. *)

let dir = "readdir_after_closedir_test"

let files = [ "a"; "b"; "c"; "d"; "e" ]

let cleanup () =
  List.iter
    (fun n -> try Unix.unlink (Filename.concat dir n) with Unix.Unix_error _ -> ())
    files;
  try Unix.rmdir dir with Unix.Unix_error _ -> ()

let () =
  cleanup ();
  Unix.mkdir dir 0o755;
  List.iter (fun n -> close_out (open_out (Filename.concat dir n))) files;
  let h = Unix.opendir dir in
  (* Read a single entry; with several files the rest stay buffered. *)
  ignore (Unix.readdir h : string);
  Unix.closedir h;
  let result =
    match Unix.readdir h with
    | exception Unix.Unix_error (Unix.EBADF, _, _) -> Ok ()
    | exception End_of_file ->
        Error "readdir after closedir raised End_of_file (expected EBADF)"
    | name ->
        Error (Printf.sprintf "readdir after closedir returned %S (expected EBADF)" name)
  in
  cleanup ();
  match result with
  | Ok () -> ()
  | Error msg -> failwith msg
