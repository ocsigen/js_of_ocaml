(* Regression test for [re_replacement_text], the replacement-text engine
   behind [Str.global_replace] / [Str.replace_matched].

   A backreference to a group that does not exist, or that did not take
   part in the match, must raise [Failure] on every backend. The wasm
   runtime used to trap with an out-of-bounds access because the group
   bound check was off by one (see runtime/wasm/str.wat). *)

let show ~repl ~re s =
  match Str.global_replace (Str.regexp re) repl s with
  | r -> Printf.printf "%S\n" r
  | exception Failure msg -> Printf.printf "Failure: %s\n" msg

let%expect_test "backreference past the last group" =
  show ~repl:"\\1" ~re:"a" "banana";
  [%expect {| Failure: Str.replace: reference to unmatched group |}];
  show ~repl:"\\2" ~re:{|\(a\)|} "banana";
  [%expect {| Failure: Str.replace: reference to unmatched group |}];
  show ~repl:"\\9" ~re:"abc" "abc";
  [%expect {| Failure: Str.replace: reference to unmatched group |}]

let%expect_test "backreference to an unmatched group" =
  show ~repl:"\\1" ~re:{|\(a\)\|\(b\)|} "b";
  [%expect {| Failure: Str.replace: reference to unmatched group |}]

let%expect_test "valid backreferences" =
  show ~repl:{|[\1]|} ~re:{|\(a\)|} "banana";
  [%expect {| "b[a]n[a]n[a]" |}];
  show ~repl:"\\0!" ~re:"a" "banana";
  [%expect {| "ba!na!na!" |}]
