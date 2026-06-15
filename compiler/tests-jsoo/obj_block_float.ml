(* Regression test for [caml_obj_block] with the double-array tag, run on
   both the JS and Wasm runtimes.

   Float arrays have a dedicated representation in the wasm runtime
   ([$float_array], distinct from a generic block). [Obj.new_block] with
   the double-array tag used to build a generic block there, so the first
   float-array primitive on the result trapped instead of working as in
   the native and JS runtimes. *)

let () =
  let o = Obj.new_block Obj.double_array_tag 3 in
  assert (Obj.tag o = Obj.double_array_tag);
  let a : float array = Obj.obj o in
  assert (Array.length a = 3);
  a.(0) <- 1.5;
  a.(1) <- 2.5;
  a.(2) <- 3.5;
  assert (a.(0) = 1.5);
  assert (a.(1) = 2.5);
  assert (a.(2) = 3.5)
