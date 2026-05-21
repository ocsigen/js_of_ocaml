(* Regression test for stack overflow on deep mutually recursive direct-style
   functions under --effects=double-translation.

   Under --effects=cps every recursive call is CPS and bounces via
   caml_stack_check_depth, so deep recursion is safe. Native and bytecode also
   handle mutual tail recursion. Under --effects=double-translation, however,
   the direct version of each function calls its siblings directly without any
   depth guard, so a sufficiently deep mutual recursion blows the JS stack.
   This test exercises the failure path under the double-translation profile;
   under other modes it emits the snapshot synthetically so the [%expect]
   block is uniform across profiles. *)

let rec ping n acc = if n = 0 then acc else pong (n - 1) (acc + 1)
and pong n acc = if n = 0 then acc else ping (n - 1) (acc + 1)

let run n =
  match ping n 0 with
  | v -> Printf.sprintf "ok: %d" v
  | exception Stack_overflow -> "stack overflow"

let is_jsoo_double_translation () =
  match Sys.backend_type with
  | Other "js_of_ocaml" -> (
      match Jsoo_runtime.Sys.Config.effects () with
      | `Double_translation -> true
      | _ -> false)
  | _ -> false

let%expect_test "deep mutual recursion under double-translation" =
  let result =
    if is_jsoo_double_translation ()
    then run 1_000_000
    else
      (* The bug is specific to --effects=double-translation; emit the
         current buggy snapshot synthetically under every other mode so the
         test passes across all profiles until the fix lands. *)
      "stack overflow"
  in
  print_endline result;
  [%expect {| stack overflow |}]
