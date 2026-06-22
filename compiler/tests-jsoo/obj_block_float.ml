(* Regression test for [caml_obj_block] with the double-array tag, run on
   the native, JS and Wasm runtimes.

   Float arrays have a dedicated representation in the wasm runtime
   ([$float_array], distinct from a generic block). [Obj.new_block] with
   the double-array tag used to build a generic block there, so the first
   float-array primitive on the result trapped instead of working as in
   the native and JS runtimes. *)

let%expect_test "Obj double_array_tag block" =
  (* [Obj.new_block double_array_tag n] takes [n] as a word count on the
     native and bytecode backends (a float spans [64 / Sys.word_size] words,
     i.e. 2 on 32-bit), but as an element count on the js and wasm backends.
     Size the block so it holds 3 floats on every backend. *)
  let words_per_float =
    match Sys.backend_type with
    | Sys.Other _ -> 1
    | Sys.Native | Sys.Bytecode -> 64 / Sys.word_size
  in
  let o = Obj.new_block Obj.double_array_tag (3 * words_per_float) in
  assert (Obj.tag o = Obj.double_array_tag);
  let a : float array = Obj.obj o in
  assert (Array.length a = 3);
  a.(0) <- 1.5;
  a.(1) <- 2.5;
  a.(2) <- 3.5;
  assert (a.(0) = 1.5);
  assert (a.(1) = 2.5);
  assert (a.(2) = 3.5)
