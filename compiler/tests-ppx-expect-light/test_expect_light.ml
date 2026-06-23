let%expect_test "hello" =
  print_string "hello world";
  [%expect {| hello world |}]

let%expect_test "two points" =
  print_string "first";
  [%expect {| first |}];
  print_string "second";
  [%expect {| second |}]

let%expect_test "output accessor" =
  print_string "grab me";
  let s = [%expect.output] in
  print_string (String.uppercase_ascii s);
  [%expect {| GRAB ME |}]

let%expect_test ("native skip" [@tags "native-only"]) =
  print_string "only on native";
  [%expect {| only on native |}]

let%expect_test "conditional backend" =
  print_string
    (match Sys.backend_type with
    | Native | Bytecode -> "is native"
    | Other _ -> "not native");
  [%expect ({| not native |} [@when js || wasm])];
  [%expect {| is native |}]

let%expect_test "conditional version" =
  Printf.printf "ocaml %s" (if true then "modern" else "ancient");
  [%expect ({| ocaml ancient |} [@when ocaml_version < (4, 0, 0)])];
  [%expect {| ocaml modern |}]

let%expect_test "multiline" =
  print_string "line1\nline2\n  indented\n";
  [%expect {|
    line1
    line2
      indented |}]

(* Gated out on every supported OCaml: must never run (its body would fail). *)
let%expect_test "gated away" =
  print_string "should never be checked";
  [%expect {| deliberately wrong |}]
[@@if ocaml_version < (4, 0, 0)]
