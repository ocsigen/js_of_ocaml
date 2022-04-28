let%expect_test _ =
  let d = Domain.spawn (fun () -> 1 + 2) in
  print_int (Domain.join d);
  [%expect.unreachable];
  let d = Domain.spawn (fun () -> 1 + 2) in
  let d_id = Domain.get_id d in
  let id = Domain.self () in
  Printf.printf "d_id: %d\n" (d_id :> int);
  Printf.printf "self id: %d\n" (id :> int);
  [%expect.unreachable]
[@@expect.uncaught_exn {| (Failure "TypeError: runtime.caml_ml_condition_new is not a function") |}]

type _ Effect.t += A : int Effect.t

let er = ref Not_found

let%expect_test _ =
  print_int (Effect.perform A);
  [%expect.unreachable]
[@@expect.uncaught_exn {| (Failure "PERFORM not implemented") |}]

let%expect_test _ =
  let f () = Effect.perform A in
  if Random.int 2 < 1 then print_int (f ()) else print_int (f () + 1);
  [%expect.unreachable]
[@@expect.uncaught_exn {| (Failure "PERFORM not implemented") |}]
