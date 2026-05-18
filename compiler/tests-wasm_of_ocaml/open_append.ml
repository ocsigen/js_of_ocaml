(* Regression test for the O_APPEND mask fix in [caml_sys_open] (wasm
   runtime).  The post-open seek used bit 4, which is never set by any flag in
   the table, so the wasm runtime never updated its tracked file offset to
   EOF and [pos_out] returned 0 after opening an existing file in append
   mode. *)

let path = "open_append_test.dat"

let () =
  (try Sys.remove path with Sys_error _ -> ());
  let oc = open_out_bin path in
  output_string oc "abc";
  close_out oc;
  let oc = open_out_gen [ Open_wronly; Open_append; Open_binary ] 0o644 path in
  assert (pos_out oc = 3);
  output_string oc "def";
  assert (pos_out oc = 6);
  close_out oc;
  let ic = open_in_bin path in
  let s = really_input_string ic (in_channel_length ic) in
  close_in ic;
  Sys.remove path;
  assert (s = "abcdef")
