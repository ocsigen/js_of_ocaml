
let%expect_test _ = Integration_util.compile_and_run {|
    let _ =
      let r = ref 0.0 in
      for _ = 1 to 100 do
        r := !r -. (-1.0 *. !r)
      done;
      ();
    print_endline "Success!"
  |};
  [%expect {| Success! |}]
