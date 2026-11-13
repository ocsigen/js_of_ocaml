(* TEST
 reference = "${test_source_directory}/ext_ptr_bytecode.reference";
 include stdlib_stable;
 include stdlib_upstream_compatible;
 bytecode;
*)

(* External pointer primitives cannot be implemented on bytecode (they
   dereference raw addresses), so they fail at runtime with a clear message.
   The same goes for pointers whose base is [Null]. *)

external get_ext_ptr
  : ('a : any).
  int64_u @ local -> 'a @ local
  = "%unsafe_get_ext_ptr"
[@@layout_poly]

external set_ext_ptr
  : ('a : any).
  int64_u @ local -> 'a @ local -> unit
  = "%unsafe_set_ext_ptr"
[@@layout_poly]

type nothing = |

external get_ptr
  : ('a : any).
  #(nothing or_null * int64_u) @ local -> 'a @ local
  = "%unsafe_get_ptr"
[@@layout_poly]

external set_ptr
  : ('a : any).
  #(nothing or_null * int64_u) @ local -> 'a @ local -> unit
  = "%unsafe_set_ptr"
[@@layout_poly]

let test name f =
  match f () with
  | () -> Printf.printf "%s: unexpectedly returned\n" name
  | exception Failure msg -> Printf.printf "%s: Failure: %s\n" name msg

let () =
  test "get_ext_ptr" (fun () -> ignore (get_ext_ptr #0L : int));
  test "set_ext_ptr" (fun () -> set_ext_ptr #0L 0);
  test "get_ptr (Null base)" (fun () -> ignore (get_ptr #(Null, #0L) : int));
  test "set_ptr (Null base)" (fun () -> set_ptr #(Null, #0L) 0)
