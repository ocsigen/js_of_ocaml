let%expect_test "Unix.error_message" =
  Printf.printf "%s\n" (String.lowercase_ascii (Unix.error_message ENOENT));
  [%expect {| no such file or directory |}]

let%expect_test "Unix.times" =
  let t = Unix.times () in
  let t' = Unix.times () in
  let cmp v v' = v' >= v && v' <= v +. 0.1 in
  if
    cmp t.tms_utime t'.tms_utime
    && cmp t.tms_stime t'.tms_stime
    && cmp t.tms_cutime t'.tms_cutime
    && cmp t.tms_cstime t'.tms_cstime
  then Printf.printf "OK\n";
  [%expect {| OK |}]

let%expect_test "Unix.link" =
  let tmp = Filename.temp_file "a" "txt" in
  let ch = open_out tmp in
  output_string ch "test\n";
  close_out ch;
  let tmp' = Filename.temp_file "a" "txt" in
  Unix.unlink tmp';
  Unix.link tmp tmp';
  let ch = open_in tmp' in
  Format.printf "%s\n" (input_line ch);
  close_in ch;
  let ch = open_out tmp' in
  output_string ch "abcd\n";
  close_out ch;
  let ch = open_in tmp in
  Format.printf "%s\n" (input_line ch);
  close_in ch;
  Unix.unlink tmp;
  Unix.unlink tmp';
  [%expect {|
    test
    abcd
  |}]

let%expect_test "Unix.readlink" =
  let tmp' = Filename.temp_file "a" "txt" in
  Unix.unlink tmp';
  Unix.symlink "abcdefgh" tmp';
  Format.printf "%s\n" (Unix.readlink tmp');
  [%expect {| abcdefgh |}]

let%expect_test "Unix.single_write" =
  let s = "abcd efgh ijkl mnop qrst uvwx" in
  let b = Bytes.of_string s in
  let tmp = Filename.temp_file "a" "txt" in
  let fd = Unix.openfile tmp [ O_WRONLY; O_CREAT; O_TRUNC ] 0o666 in
  let n = Unix.single_write fd b 0 (Bytes.length b) in
  Unix.close fd;
  let ch = open_in tmp in
  let s' = really_input_string ch n in
  Printf.printf "%b %b\n" (n > 0) (s' = String.sub s 0 n);
  [%expect {| true true |}]

let%expect_test "Unix.read" =
  let tmp = Filename.temp_file "a" "txt" in
  let fd = Unix.openfile tmp [ O_RDONLY ] 0o666 in
  (try Printf.printf "write: %d\n" (Unix.write fd (Bytes.create 8) 0 8)
   with Unix.Unix_error (_, _, _) -> Printf.printf "write failed\n");
  Unix.close fd;
  Unix.unlink tmp;
  (try Printf.printf "read: %d\n" (Unix.read fd (Bytes.create 8) 0 8)
   with Unix.Unix_error (err, _, _) ->
     Printf.printf "%s\n" (String.lowercase_ascii (Unix.error_message err)));
  [%expect {|
    write failed
    bad file descriptor
 |}]

let%expect_test "Unix.read with offset" =
  let tmp = Filename.temp_file "a" "txt" in
  let fd = Unix.openfile tmp [ O_WRONLY; O_CREAT; O_TRUNC ] 0o666 in
  let data = "abcdefghijklmnopqrstuvwxyz" in
  let _ = Unix.write_substring fd data 0 (String.length data) in
  Unix.close fd;
  let fd = Unix.openfile tmp [ O_RDONLY ] 0o666 in
  (* Read first 5 bytes *)
  let buf = Bytes.create 5 in
  let n = Unix.read fd buf 0 5 in
  Printf.printf "read %d: %s\n" n (Bytes.sub_string buf 0 n);
  (* Read next 5 bytes *)
  let n = Unix.read fd buf 0 5 in
  Printf.printf "read %d: %s\n" n (Bytes.sub_string buf 0 n);
  (* Seek to position 20 and read *)
  let _ = Unix.lseek fd 20 SEEK_SET in
  let n = Unix.read fd buf 0 5 in
  Printf.printf "read %d: %s\n" n (Bytes.sub_string buf 0 n);
  Unix.close fd;
  Unix.unlink tmp;
  [%expect {|
    read 5: abcde
    read 5: fghij
    read 5: uvwxy
    |}]

let%expect_test "Unix.write large buffer" =
  let len = 70_000 in
  let tmp = Filename.temp_file "a" "txt" in
  let fd = Unix.openfile tmp [ O_WRONLY; O_CREAT; O_TRUNC ] 0o666 in
  let n = Unix.write fd (Bytes.make len 'x') 0 len in
  Unix.close fd;
  let st = Unix.stat tmp in
  Unix.unlink tmp;
  Printf.printf "%d %d\n" n st.st_size;
  [%expect {| 70000 70000 |}]

let%expect_test "Unix.LargeFile.fstat" =
  let tmp = Filename.temp_file "a" "txt" in
  let fd = Unix.openfile tmp [ O_WRONLY; O_CREAT; O_TRUNC ] 0o666 in
  let data = String.make 1234 'z' in
  let _ = Unix.write_substring fd data 0 (String.length data) in
  let st = Unix.LargeFile.fstat fd in
  Printf.printf "size: %Ld\n" st.st_size;
  Unix.close fd;
  Unix.unlink tmp;
  [%expect {| size: 1234 |}]

let%expect_test "Unix.symlink to_dir" =
  let tmp = Filename.temp_file "a" "txt" in
  Unix.unlink tmp;
  (try
     Unix.symlink ~to_dir:true "some/target" tmp;
     ignore (Unix.readlink tmp);
     Printf.printf "ok\n";
     Unix.unlink tmp
   with Unix.Unix_error (err, _, _) ->
     Printf.printf "error: %s\n" (String.lowercase_ascii (Unix.error_message err)));
  [%expect {| ok |}]

let%expect_test "Unix.getenv" =
  Printf.printf "%s\n" (Sys.getenv "FOO");
  [%expect {| bar |}]

(* Sizes must not be truncated to 32 bits, ftruncate must not move the
   tracked fd offset, and st_perm must not include the file-type bits. *)
let%expect_test "truncate and stat" =
  let f = Filename.temp_file "jsoo_trunc" ".dat" in
  Unix.LargeFile.truncate f 5_000_000_000L;
  Printf.printf "%Ld\n" (Unix.LargeFile.stat f).Unix.LargeFile.st_size;
  let fd = Unix.openfile f [ Unix.O_RDWR ] 0o644 in
  Unix.LargeFile.ftruncate fd 3_000_000_000L;
  Printf.printf "%Ld\n" (Unix.LargeFile.fstat fd).Unix.LargeFile.st_size;
  ignore (Unix.lseek fd 100 Unix.SEEK_SET);
  Unix.ftruncate fd 50;
  Printf.printf "%d\n" (Unix.lseek fd 0 Unix.SEEK_CUR);
  Unix.close fd;
  Unix.chmod f 0o644;
  Printf.printf "0o%o\n" (Unix.stat f).Unix.st_perm;
  Sys.remove f;
  [%expect
    {|
    705032704
    0
    50
    0o100644
    |}]
