[@@@ocaml.alert "-unsafe_parallelism-unsafe_multidomain-do_not_spawn_domains"]

let%expect_test _ =
  let d = Domain.spawn (fun () -> 1 + 2) in
  print_int (Domain.join d);
  [%expect {| 3 |}];
  let d = Domain.spawn (fun () -> 1 + 2) in
  let d_id = Domain.get_id d in
  let id = Domain.self () in
  Printf.printf "d_id: %d\n" (d_id :> int);
  Printf.printf "self id: %d\n" (id :> int);
  let res = Domain.join d in
  Printf.printf "result: %d\n" res;
  [%expect {|
    d_id: 2
    self id: 0
    result: 3 |}]

type _ Effect.t += A : int Effect.t

let handle comp =
  Effect.Deep.try_with
    comp
    ()
    { effc =
        (fun (type a) (e : a Effect.t) ->
          match e with
          | A ->
              Some
                (fun (k : (a, _) Effect.Deep.continuation) -> Effect.Deep.continue k 42)
          | _ -> None)
    }

let er = ref Not_found

let%expect_test _ =
  handle (fun () -> print_int (Effect.perform A));
  [%expect {| 42 |}]

let%expect_test _ =
  let f () = Effect.perform A in
  handle (fun () ->
      if Random.int 2 < 1 then print_int (1 + f ()) else print_int (f () + 1));
  [%expect {| 43 |}]

let%expect_test "domain body raising" =
  (match
     let d = Domain.spawn (fun () -> raise Not_found) in
     Domain.join d
   with
  | (_ : int) -> print_endline "no exn"
  | exception Not_found -> print_endline "Not_found"
  | exception e -> print_endline ("other: " ^ Printexc.to_string e));
  (* the main domain id must be restored after a raising body *)
  Printf.printf "self id: %d\n" (Domain.self () :> int);
  (* spawning still works afterwards *)
  let d = Domain.spawn (fun () -> 7) in
  Printf.printf "result: %d\n" (Domain.join d);
  [%expect {|
    Not_found
    self id: 3
    result: 7
    |}]
