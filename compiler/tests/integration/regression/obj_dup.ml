let%expect_test _ = Integration_util.compile_and_run {|
    let print_bool b = print_endline (string_of_bool b)
    let () =
      let s = "Hello" in
      let s' : string = Obj.obj (Obj.dup (Obj.repr s)) in
      print_bool (s = s');
      print_bool (s != s')

    let () =
      let s = Bytes.of_string "Hello" in
      let s' : bytes = Obj.obj (Obj.dup (Obj.repr s)) in
      print_bool (s = s');
      print_bool (s != s');
      Bytes.set s' 1 'a';
      print_bool (s <> s')
  |};
  [%expect {|
    true
    true
    true
    true
    true
  |}]

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
