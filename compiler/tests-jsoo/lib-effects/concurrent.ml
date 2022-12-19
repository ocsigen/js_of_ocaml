(* Demonstrate the concurrent scheduler
   ------------------------------------
    Spawn binary tree of tasks in depth-first order

        ************
         Fiber tree
        ************
              0
            /  \
           1    2
          / \  / \
         3   4 5  6
*)

let log = Printf.printf

let rec f id depth =
  log "Starting number %i\n%!" id;
  if depth > 0
  then (
    log "Forking number %i\n%!" ((id * 2) + 1);
    Sched.fork (fun () -> f ((id * 2) + 1) (depth - 1));
    log "Forking number %i\n%!" ((id * 2) + 2);
    Sched.fork (fun () -> f ((id * 2) + 2) (depth - 1)))
  else (
    log "Yielding in number %i\n%!" id;
    Sched.yield ();
    log "Resumed number %i\n%!" id);
  log "Finishing number %i\n%!" id

let%expect_test _ =
  Sched.run (fun () -> f 0 2);
  [%expect
    {|
  Starting number 0
  Forking number 1
  Starting number 1
  Forking number 3
  Starting number 3
  Yielding in number 3
  Forking number 2
  Starting number 2
  Forking number 5
  Starting number 5
  Yielding in number 5
  Forking number 4
  Starting number 4
  Yielding in number 4
  Resumed number 3
  Finishing number 3
  Finishing number 0
  Forking number 6
  Starting number 6
  Yielding in number 6
  Resumed number 5
  Finishing number 5
  Finishing number 1
  Resumed number 4
  Finishing number 4
  Finishing number 2
  Resumed number 6
  Finishing number 6 |}]
