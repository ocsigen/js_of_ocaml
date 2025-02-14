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

let on_windows = Sys.os_type = "Win32"

let%expect_test "Unix.chmod / Unix.fchmod / Unix.access" =
  let tmp = Filename.temp_file "a" "txt" in
  let test ?(ok_on_windows = false) flags =
    try
      Unix.access tmp flags;
      if on_windows && ok_on_windows
      then Printf.printf "denied (success on Windows)\n"
      else Printf.printf "success\n"
    with
    | Unix.Unix_error ((EPERM | EACCES), _, _) ->
        if (not on_windows) && ok_on_windows
        then Printf.printf "denied (success on Windows)\n"
        else Printf.printf "denied\n"
    | Unix.Unix_error (ENOENT, _, _) -> Printf.printf "absent\n"
  in
  let touch perms =
    Unix.chmod tmp 0o600;
    Unix.unlink tmp;
    let fd = Unix.openfile tmp [ O_WRONLY; O_CREAT; O_TRUNC ] perms in
    Unix.close fd
  in
  let test_perms set =
    set 0o200;
    test ~ok_on_windows:true [ R_OK ];
    test [ W_OK ];
    test ~ok_on_windows:true [ R_OK; W_OK ];
    [%expect
      {|
      denied (success on Windows)
      success
      denied (success on Windows)
      |}];
    set 0o400;
    test [ R_OK ];
    test [ W_OK ];
    test [ R_OK; W_OK ];
    [%expect {|
    success
    denied
    denied |}];
    set 0o600;
    test [ R_OK ];
    test [ W_OK ];
    test [ R_OK; W_OK ];
    [%expect {|
    success
    success
    success |}];
    set 0o000;
    test ~ok_on_windows:true [ R_OK ];
    test [ W_OK ];
    test [ R_OK; W_OK ];
    [%expect {|
      denied (success on Windows)
      denied
      denied
      |}]
  in
  test [ F_OK ];
  [%expect {|
    success |}];
  Unix.chmod tmp 0o600;
  Unix.unlink tmp;
  test [ F_OK ];
  [%expect {|
    absent |}];
  let fd = Unix.openfile tmp [ O_WRONLY; O_CREAT; O_TRUNC ] 0o666 in
  test [ F_OK ];
  [%expect {|
    success |}];
  if not on_windows then test_perms (Unix.fchmod fd);
  Unix.close fd;
  test_perms (Unix.chmod tmp);
  test_perms touch;
  Unix.chmod tmp 0o600;
  Unix.unlink tmp

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

let%expect_test "Unix.getenv" =
  Printf.printf "%s\n" (Sys.getenv "FOO");
  [%expect {| bar |}]
