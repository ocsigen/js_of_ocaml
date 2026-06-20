(* These tests create symlinks whose target is an absolute path (returned by
   [Filename.temp_file]). WASI's capability model rejects such symlinks with
   EPERM, so they live in their own library, disabled under the wasi profile.
   Symlinks with relative targets (exercised in test_unix.ml) work under WASI
   and stay there. *)

(* Sys.is_directory follows symlinks (uses stat, not lstat), like native:
   a symlink to a directory is a directory, a symlink to a file is not. *)
let%expect_test "is_directory follows symlinks" =
  let d = Filename.temp_file "jsoo_symdir" "" in
  Sys.remove d;
  Unix.mkdir d 0o755;
  let f = Filename.temp_file "jsoo_symfile" ".dat" in
  let ld = Filename.temp_file "jsoo_lnd" "" in
  Sys.remove ld;
  let lf = Filename.temp_file "jsoo_lnf" "" in
  Sys.remove lf;
  Unix.symlink d ld;
  Unix.symlink f lf;
  Printf.printf "symlink->dir: %b\n" (Sys.is_directory ld);
  Printf.printf "symlink->file: %b\n" (Sys.is_directory lf);
  Sys.remove ld;
  Sys.remove lf;
  Sys.remove f;
  Unix.rmdir d;
  [%expect {|
    symlink->dir: true
    symlink->file: false
    |}]

(* Unix.utimes follows symlinks (it is utimes, not lutimes): setting the
   times through a symlink updates the target. *)
let%expect_test "utimes follows symlinks" =
  let f = Filename.temp_file "jsoo_utf" ".dat" in
  let l = Filename.temp_file "jsoo_utl" "" in
  Sys.remove l;
  Unix.symlink f l;
  Unix.utimes l 12345.0 12345.0;
  Printf.printf "%b\n" ((Unix.stat f).Unix.st_mtime = 12345.0);
  Sys.remove l;
  Sys.remove f;
  [%expect {| true |}]
