let%expect_test _ =Integration_util.compile_and_run {|
  let i = ref 0
  let log_success () = print_endline "Success!"
  let log_failure = Printf.printf "Failure! %s"

  let side_effect yes label =
    if yes
    then (
      Printf.printf "Side effect: %s\n%!" label;
      incr i);
    0

  let _ = side_effect false "this is only to avoid inlining"

  let f =
    match side_effect true "Should only see this once" with
    | 0 | 1 | 2 -> Printf.printf "Please don't optimize this away\n%!"
    | _ -> Printf.printf "Or this\n%!"

  let _ = if !i = 1 then log_success () else log_failure "side effect computed twice"
  |};
  [%expect {|
    Side effect: Should only see this once
    Please don't optimize this away
    Success! |}]
