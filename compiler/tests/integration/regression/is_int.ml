let%expect_test _ = Integration_util.compile_and_run {|
    let r = ref false
    let f x = match Obj.is_int x with
              | true -> r := true; true
              | false -> r := false; false

    let print_bool b = print_endline (string_of_bool b)
    let () =
      print_string "[not (is_int 1)]: ";
      print_bool (not (f (Obj.repr 1)));
      print_string "[is_int (1,2,3)]: ";
      print_bool (f (Obj.repr (1, 2, 3)))
  |};
  [%expect {|
    [not (is_int 1)]: false
    [is_int (1,2,3)]: false
  |}]
