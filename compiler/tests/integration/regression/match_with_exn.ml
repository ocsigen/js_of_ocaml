let%expect_test _ = Integration_util.compile_and_run {|
  exception A
  exception B of int

  let a_exn () = raise A

  (* Make sure that [a] doesn't look constant *)
  let a () = if Random.int 1 + 1 = 0 then 2 else 4

  let b_exn () = raise (B 2)

  (* https://github.com/ocsigen/js_of_ocaml/issues/400
   * match .. with exception is no compiled properly *)
  let () =
    assert (
      try
        match a () with
        | exception (A | B _) -> true
        | _n -> b_exn ()
      with B _ -> true);
  print_endline "Success!"
|};
  [%expect "Success!"]
