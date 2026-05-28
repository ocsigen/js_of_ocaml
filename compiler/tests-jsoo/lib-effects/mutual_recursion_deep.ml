(* Regression test for stack overflow on deep mutually recursive direct-style
   functions under --effects=double-translation.

   Under --effects=cps every recursive call is CPS and bounces via
   caml_stack_check_depth, so deep recursion is safe. Native and bytecode also
   handle mutual tail recursion. Historically, under
   --effects=double-translation the direct version of each function called
   its siblings directly without any depth guard, so a sufficiently deep
   mutual recursion blew the JS stack. The fix wraps cps_needed mutually
   recursive direct closures with a depth-guarded trampoline that bounces
   into the CPS partner when the JS stack budget is exhausted; this test
   guards the fix. *)

let rec ping n acc = if n = 0 then acc else pong (n - 1) (acc + 1)

and pong n acc = if n = 0 then acc else ping (n - 1) (acc + 1)

let run n =
  match ping n 0 with
  | v -> Printf.sprintf "ok: %d" v
  | exception Stack_overflow -> "stack overflow"

let%expect_test "deep mutual recursion under double-translation" =
  print_endline (run 1_000_000);
  [%expect {| ok: 1000000 |}]
