  let%expect_test _ = Integration_util.compile_and_run {|
    let log_success () = print_endline "Success!"
    let log_failure = Printf.printf "Failure! %s"

    let _ =
      let rec odd x = if x = 0 then false else even (x - 1)
      and even x = if x = 0 then true else odd (x - 1) in
      assert (odd 1 <> even 1);
      try
        ignore (odd 5000);
        log_success ()
      with _ -> log_failure "too much recursion"
    |};
    [%expect {| Success! |}]

