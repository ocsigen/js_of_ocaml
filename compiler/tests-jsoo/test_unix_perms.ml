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
