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

let%expect_test ("backreference past the last group" [@when not wasi]) =
  show ~repl:"\\1" ~re:"a" "banana";
  [%expect {| Failure: Str.replace: reference to unmatched group |}];
  show ~repl:"\\2" ~re:{|\(a\)|} "banana";
  [%expect {| Failure: Str.replace: reference to unmatched group |}];
  show ~repl:"\\9" ~re:"abc" "abc";
  [%expect {| Failure: Str.replace: reference to unmatched group |}]

let%expect_test ("backreference to an unmatched group" [@when not wasi]) =
  show ~repl:"\\1" ~re:{|\(a\)\|\(b\)|} "b";
  [%expect {| Failure: Str.replace: reference to unmatched group |}]

let%expect_test ("valid backreferences" [@when not wasi]) =
  show ~repl:{|[\1]|} ~re:{|\(a\)|} "banana";
  [%expect {| "b[a]n[a]n[a]" |}];
  show ~repl:"\\0!" ~re:"a" "banana";
  [%expect {| "ba!na!na!" |}]

(* [Str.replace_matched repl s] reads the groups from [s] using the offsets
   of the last match. If [s] is shorter than the string that was searched,
   it must raise [Invalid_argument] (ocaml/ocaml#15290) rather than read out
   of bounds. *)
let replace_matched ~repl s =
  match Str.replace_matched repl s with
  | r -> Printf.printf "%S\n" r
  | exception Invalid_argument msg -> Printf.printf "Invalid_argument: %s\n" msg

let%expect_test ("replace_matched with groups that fit" [@when not wasi]) =
  let s = "abcde" in
  ignore (Str.search_forward (Str.regexp {|b\(c\)de|}) s 0);
  replace_matched ~repl:{|\0,\1|} s;
  [%expect {| "bcde,c" |}];
  (* Group 1 fits in a shorter string, and group 0 is not referenced *)
  replace_matched ~repl:{|\1|} "xyz";
  [%expect {| "z" |}];
  replace_matched ~repl:"-" "x";
  [%expect {| "-" |}]

(* Not run natively: the C stub reads out of bounds without
   ocaml/ocaml#15394. *)
let%expect_test ("replace_matched with a string too short"
    [@when (not wasi) && not native])
  =
  let s = "abcde" in
  ignore (Str.search_forward (Str.regexp {|b\(c\)de|}) s 0);
  replace_matched ~repl:{|\0|} "xyz";
  [%expect {| Invalid_argument: Str.replace_matched |}];
  replace_matched ~repl:{|\0|} "x";
  [%expect {| Invalid_argument: Str.replace_matched |}];
  replace_matched ~repl:{|\1|} "ab";
  [%expect {| Invalid_argument: Str.replace_matched |}]
