(* Regression test for the O_APPEND mask fix in [caml_unix_open] (wasm
   runtime).  The post-open seek tested bit 4 ([O_RDWR]) instead of bit 8
   ([O_APPEND]), so opening an existing file with [Unix.openfile [O_APPEND]]
   left the runtime's tracked offset at 0 instead of EOF -- observable
   through [Unix.lseek _ SEEK_CUR] and [pos_out] -- while [O_RDWR] files
   wrongly started at EOF. *)

let path = "unix_open_append_test.dat"

let () =
  (try Sys.remove path with Sys_error _ -> ());
  let fd = Unix.openfile path [ Unix.O_WRONLY; Unix.O_CREAT ] 0o644 in
  assert (Unix.write_substring fd "abc" 0 3 = 3);
  Unix.close fd;
  (* O_APPEND: the descriptor must start at the end of the file. *)
  let fd = Unix.openfile path [ Unix.O_WRONLY; Unix.O_APPEND ] 0o644 in
  assert (Unix.lseek fd 0 Unix.SEEK_CUR = 3);
  Unix.close fd;
  (* No O_APPEND: the descriptor must start at the beginning. *)
  let fd = Unix.openfile path [ Unix.O_RDWR ] 0o644 in
  assert (Unix.lseek fd 0 Unix.SEEK_CUR = 0);
  Unix.close fd;
  Sys.remove path
