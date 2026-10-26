(* Every expectation below is wrong on purpose. *)

let%expect_test "guarded variant, parenthesized" =
  print_string "actual";
  [%expect ({| stale |} [@when ocaml_version >= (4, 0, 0)])];
  [%expect {| default |}]

let%expect_test "guarded multiline variant, parenthesized" =
  print_string "line 1\nline 2";
  [%expect ({|
    stale 1
    stale 2
    |} [@when ocaml_version >= (4, 0, 0)])];
  [%expect {| default |}]

let%expect_test "guarded variant, regular string" =
  print_string "actual";
  [%expect ("stale" [@when ocaml_version >= (4, 0, 0)])];
  [%expect {| default |}]

let%expect_test "default variant" =
  print_string "actual";
  [%expect ({| other |} [@when ocaml_version < (4, 0, 0)])];
  [%expect {| stale |}]

let%expect_test "empty" =
  print_string "actual";
  [%expect]

let () = Ppx_devtools_expect_runtime.exit ()
