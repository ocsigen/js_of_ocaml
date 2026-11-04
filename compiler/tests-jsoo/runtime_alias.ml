external f : int -> int = "jsoo_test_alias"

let%expect_test "primitive called through an alias" =
  print_int (f 41);
  [%expect {| 42 |}]
