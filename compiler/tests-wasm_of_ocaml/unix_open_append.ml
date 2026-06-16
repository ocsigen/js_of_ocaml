(* [Unix.openfile [O_APPEND]] must match native semantics:
   - the file offset starts at 0 right after opening (it is *not* moved to EOF
     at open time -- [lseek _ SEEK_CUR] returns 0), and
   - every [write] goes to the end of the file, even after [lseek] moves the
     offset backwards; the offset is then left at EOF. *)

let path = "unix_open_append_test.dat"

let read_all () =
  let ic = open_in_bin path in
  let s = really_input_string ic (in_channel_length ic) in
  close_in ic;
  s

let () =
  (try Sys.remove path with Sys_error _ -> ());
  let fd = Unix.openfile path [ Unix.O_WRONLY; Unix.O_CREAT ] 0o644 in
  assert (Unix.write_substring fd "abc" 0 3 = 3);
  Unix.close fd;
  (* O_APPEND: the offset starts at the beginning, not at EOF. *)
  let fd = Unix.openfile path [ Unix.O_WRONLY; Unix.O_APPEND ] 0o644 in
  assert (Unix.lseek fd 0 Unix.SEEK_CUR = 0);
  (* A write appends and leaves the offset at EOF. *)
  assert (Unix.write_substring fd "de" 0 2 = 2);
  assert (Unix.lseek fd 0 Unix.SEEK_CUR = 5);
  (* Seeking backwards does not stop writes from appending. *)
  assert (Unix.lseek fd 0 Unix.SEEK_SET = 0);
  assert (Unix.write_substring fd "Z" 0 1 = 1);
  assert (Unix.lseek fd 0 Unix.SEEK_CUR = 6);
  Unix.close fd;
  assert (read_all () = "abcdeZ");
  (* No O_APPEND: the offset starts at the beginning. *)
  let fd = Unix.openfile path [ Unix.O_RDWR ] 0o644 in
  assert (Unix.lseek fd 0 Unix.SEEK_CUR = 0);
  Unix.close fd;
  Sys.remove path
