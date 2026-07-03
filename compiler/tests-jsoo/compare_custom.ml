(* A custom block whose custom operations register a [compare] op (like
   zarith_stubs_js) must have that op used when the block is compared against
   an immediate: small zarith values are immediates and large ones custom
   blocks, so [compare (Z.of_int 5) big] has to inspect the values rather than
   blindly ordering the immediate before the block. *)

external register : unit -> unit = "test_zt_register"

external make : int -> Obj.t = "test_zt_make"

let%expect_test "compare immediate vs custom with a compare op" =
  register ();
  let small n : Obj.t = Obj.repr n in
  let p label f = Printf.printf "%s = %d\n" label (f ()) in
  (* immediate on the left *)
  p "cmp 5 (big -100)" (fun () -> compare (small 5) (make (-100)));
  p "cmp 5 (big 100)" (fun () -> compare (small 5) (make 100));
  (* immediate on the right *)
  p "cmp (big -100) 5" (fun () -> compare (make (-100)) (small 5));
  p "cmp (big 100) 5" (fun () -> compare (make 100) (small 5));
  (* equal values: the op returns 0 and comparison keeps going *)
  p "eq 5 (big 5)" (fun () -> if make 5 = small 5 then 1 else 0);
  [%expect
    {|
    cmp 5 (big -100) = 1
    cmp 5 (big 100) = -1
    cmp (big -100) 5 = -1
    cmp (big 100) 5 = 1
    eq 5 (big 5) = 1
    |}]
