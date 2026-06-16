(* [Open_append] must match native channel semantics:
   - the channel position starts at 0 right after opening (it is *not* moved to
     EOF at open time), and
   - every write goes to the end of the file, even after [seek_out] moves the
     position backwards, while [pos_out] keeps tracking the logical position. *)

let path = "open_append_test.dat"

let () =
  (try Sys.remove path with Sys_error _ -> ());
  let oc = open_out_bin path in
  output_string oc "abc";
  close_out oc;
  let oc = open_out_gen [ Open_wronly; Open_append; Open_binary ] 0o644 path in
  (* Native: the position starts at the beginning, not at EOF. *)
  assert (pos_out oc = 0);
  output_string oc "def";
  assert (pos_out oc = 3);
  (* Seeking backwards does not stop writes from appending. *)
  seek_out oc 0;
  assert (pos_out oc = 0);
  output_string oc "Z";
  assert (pos_out oc = 1);
  close_out oc;
  let ic = open_in_bin path in
  let s = really_input_string ic (in_channel_length ic) in
  close_in ic;
  Sys.remove path;
  assert (s = "abcdefZ")
