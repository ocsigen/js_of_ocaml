(* Condition variables must agree between the js and wasm runtimes: each is a
   distinct value (not all physically equal), and in the single-threaded
   runtime Condition.wait is a no-op rather than raising. Not run natively,
   where Condition.wait would block forever. *)

(* Condition/Mutex are in the threads library on OCaml < 5; gating the body
   here (rather than via dune's build_if) keeps `make all` from compiling and
   failing the module on older compilers. *)
[@@@if ocaml_version >= (5, 0, 0)]

let%expect_test "Condition.create/signal/broadcast" =
  assert (Condition.create () != Condition.create ());
  let m = Mutex.create () in
  let c = Condition.create () in
  Mutex.lock m;
  Condition.wait c m;
  Mutex.unlock m;
  Condition.signal c;
  Condition.broadcast c;
  print_endline "ok";
  [%expect {| ok |}]
