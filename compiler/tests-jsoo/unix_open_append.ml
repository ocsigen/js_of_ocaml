let%expect_test "openfile O_APPEND offset" =
  (* [Unix.openfile [O_APPEND]] must match native semantics:
     - the file offset starts at 0 right after opening (it is *not* moved to
       EOF at open time -- [lseek _ SEEK_CUR] returns 0), and
     - every [write] goes to the end of the file, even after [lseek] moves the
       offset backwards; the offset is then left at EOF. *)
  let path = Filename.temp_file "jsoo_open_append" ".dat" in
  let read_all () =
    let ic = open_in_bin path in
    let s = really_input_string ic (in_channel_length ic) in
    close_in ic;
    s
  in
  let fd = Unix.openfile path [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o644 in
  ignore (Unix.write_substring fd "abc" 0 3 : int);
  Unix.close fd;
  (* O_APPEND: the offset starts at the beginning, not at EOF. *)
  let fd = Unix.openfile path [ Unix.O_WRONLY; Unix.O_APPEND ] 0o644 in
  Printf.printf "after open: %d\n" (Unix.lseek fd 0 Unix.SEEK_CUR);
  (* A write appends and leaves the offset at EOF. *)
  ignore (Unix.write_substring fd "de" 0 2 : int);
  Printf.printf "after write: %d\n" (Unix.lseek fd 0 Unix.SEEK_CUR);
  (* Seeking backwards does not stop writes from appending. *)
  ignore (Unix.lseek fd 0 Unix.SEEK_SET : int);
  ignore (Unix.write_substring fd "Z" 0 1 : int);
  Printf.printf "after append past seek: %d\n" (Unix.lseek fd 0 Unix.SEEK_CUR);
  Unix.close fd;
  Printf.printf "contents: %s\n" (read_all ());
  (* No O_APPEND: the offset starts at the beginning. *)
  let fd = Unix.openfile path [ Unix.O_RDWR ] 0o644 in
  Printf.printf "rdwr open: %d\n" (Unix.lseek fd 0 Unix.SEEK_CUR);
  Unix.close fd;
  Sys.remove path;
  [%expect
    {|
    after open: 0
    after write: 5
    after append past seek: 6
    contents: abcdeZ
    rdwr open: 0
    |}]

let%expect_test "O_RDWR O_APPEND shares one offset between read and write" =
  (* read and write share a single file offset. An appending write moves that
     shared offset to EOF (as native does before each write), so a read after
     a write resumes from the new EOF -- it does not keep an independent read
     cursor. *)
  let path = Filename.temp_file "jsoo_rdwr_append" ".dat" in
  let fd = Unix.openfile path [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o644 in
  ignore (Unix.write_substring fd "abc" 0 3 : int);
  Unix.close fd;
  let fd = Unix.openfile path [ Unix.O_RDWR; Unix.O_APPEND ] 0o644 in
  let buf = Bytes.create 8 in
  let n = Unix.read fd buf 0 1 in
  Printf.printf
    "read %d %S, offset %d\n"
    n
    (Bytes.sub_string buf 0 n)
    (Unix.lseek fd 0 Unix.SEEK_CUR);
  (* Appending write jumps the shared offset to EOF. *)
  ignore (Unix.write_substring fd "X" 0 1 : int);
  Printf.printf "offset after write %d\n" (Unix.lseek fd 0 Unix.SEEK_CUR);
  (* Read resumes from EOF -> nothing. *)
  let n = Unix.read fd buf 0 8 in
  Printf.printf "read at eof %d\n" n;
  (* Seeking back lets the read see earlier bytes again. *)
  ignore (Unix.lseek fd 1 Unix.SEEK_SET : int);
  let n = Unix.read fd buf 0 1 in
  Printf.printf "read after seek %d %S\n" n (Bytes.sub_string buf 0 n);
  Unix.close fd;
  Sys.remove path;
  [%expect
    {|
    read 1 "a", offset 1
    offset after write 4
    read at eof 0
    read after seek 1 "b"
    |}]
