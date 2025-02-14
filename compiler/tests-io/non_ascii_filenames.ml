let test f v =
  try
    ignore (f v);
    Printf.printf "  %s: PASS\n" v
  with Sys_error _ | Not_found | Exit -> Printf.printf "  %s: FAIL\n" v

let () =
  Printf.printf "opening files\n";
  test open_in "/static/accentué";
  test open_in "/static/accentu�";
  test open_in "accentué";
  test open_in "accentu�";
  Printf.printf "reading directories\n";
  let check_file d =
    let a = Sys.readdir d in
    if not (Array.exists (fun x -> x = "accentué") a) then raise Not_found
  in
  test check_file ".";
  test check_file "/static";
  Printf.printf "current working directory\n";
  let test_chdir d =
    Sys.mkdir d 0o777;
    Sys.chdir d;
    if Filename.basename (Sys.getcwd ()) <> "répertoire" then raise Exit
  in
  test test_chdir "./répertoire";
  test test_chdir "/static/répertoire"
