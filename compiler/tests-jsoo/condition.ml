(* Condition variables must agree between the js and wasm runtimes: each is a
   distinct value (not all physically equal), and in the single-threaded
   runtime Condition.wait is a no-op rather than raising. Not run natively,
   where Condition.wait would block forever. *)

let () =
  assert (Condition.create () != Condition.create ());
  let m = Mutex.create () in
  let c = Condition.create () in
  Mutex.lock m;
  Condition.wait c m;
  Mutex.unlock m;
  Condition.signal c;
  Condition.broadcast c;
  print_endline "ok"
