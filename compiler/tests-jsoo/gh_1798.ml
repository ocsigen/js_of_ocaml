let%expect_test _ =
  (match Marshal.to_buffer (Bytes.create 1024) 0 1024 10 [] with
  | _ -> print_endline "success"
  | exception e ->
      print_endline (Printexc.to_string e);
      print_endline "failure");
  [%expect {|
    success
 |}]
