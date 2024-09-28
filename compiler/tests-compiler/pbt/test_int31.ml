open! Js_of_ocaml_compiler.Stdlib
open QCheck2

let () = Printexc.record_backtrace false

let min_int31 = Int32.(neg (shift_left 1l 30))

let max_int31 = Int32.(sub (shift_left 1l 30) 1l)

let in_range i = Int32.(min_int31 <= i && i <= max_int31)

let in_range_i32 = Gen.(Int32.of_int <$> int_range (-(1 lsl 30)) ((1 lsl 30) - 1))

let out_of_range_int =
  let open Gen in
  oneof
    [ int_range (-(1 lsl 31)) (-(1 lsl 30) - 1); int_range (1 lsl 30) ((1 lsl 31) - 1) ]

let out_of_range_i32 = out_of_range_int |> Gen.map Int32.of_int

let t_corner =
  let open Gen in
  graft_corners in_range_i32 [ min_int31; max_int31 ] ()
  |> map Int31.of_int32_warning_on_overflow

let print_t t = Format.sprintf "%ld" (Int31.to_int32 t)

let%expect_test _ =
  Test.check_exn
  @@ Test.make
       ~count:1000
       ~name:"Int31.of_int32_warning_on_overflow: normal"
       in_range_i32
       (fun i -> Int32.equal Int31.(to_int32 (of_int32_warning_on_overflow i)) i);
  [%expect ""]

let%expect_test _ =
  Test.check_exn
  @@ Test.make
       ~count:1000
       ~name:"Int31.of_int_warning_on_overflow: normal"
       (Gen.map Int32.to_int in_range_i32)
       (fun i ->
         Int.equal (Int31.(to_int32 (of_int_warning_on_overflow i)) |> Int32.to_int) i);
  [%expect ""]

let%expect_test _ =
  Test.check_exn
  @@ Test.make
       ~count:1000
       ~name:"Int31.of_nativeint_warning_on_overflow: normal"
       (Gen.map Nativeint.of_int32 in_range_i32)
       (fun i ->
         Nativeint.equal
           (Int31.(to_int32 (of_nativeint_warning_on_overflow i)) |> Nativeint.of_int32)
           i);
  [%expect ""]

let%expect_test _ =
  let i = Gen.(generate1 (no_shrink out_of_range_i32)) in
  let i_trunc = Int32.(shift_right (shift_left i 1) 1) in
  ignore (Int31.of_int32_warning_on_overflow i);
  let output = [%expect.output] in
  let expected =
    Format.sprintf
      "Warning: integer overflow: integer 0x%lx (%ld) truncated to 0x%lx (%ld); the \
       generated code might be incorrect.@."
      i
      i
      i_trunc
      i_trunc
  in
  if not (String.equal output expected)
  then Format.printf "Unexpected output string@.%s@.Expected:@.%s@." output expected;
  [%expect ""]

let%expect_test _ =
  let i = Gen.(generate1 (no_shrink out_of_range_int)) in
  let i_trunc = Int32.(shift_right (shift_left (of_int i) 1) 1) in
  ignore (Int31.of_int_warning_on_overflow i);
  let output = [%expect.output] in
  let expected =
    Format.sprintf
      "Warning: integer overflow: integer 0x%x (%d) truncated to 0x%lx (%ld); the \
       generated code might be incorrect.@."
      i
      i
      i_trunc
      i_trunc
  in
  if not (String.equal output expected)
  then Format.printf "Unexpected output string@.%s@.Expected:@.%s@." output expected;
  [%expect ""]

let%expect_test _ =
  let i = Gen.(generate1 (no_shrink (Nativeint.of_int <$> out_of_range_int))) in
  let i_trunc = Int32.(shift_right (shift_left (Nativeint.to_int32 i) 1) 1) in
  ignore (Int31.of_nativeint_warning_on_overflow i);
  let output = [%expect.output] in
  let expected =
    Format.sprintf
      "Warning: integer overflow: integer 0x%nx (%nd) truncated to 0x%lx (%ld); the \
       generated code might be incorrect.@."
      i
      i
      i_trunc
      i_trunc
  in
  if not (String.equal output expected)
  then Format.printf "Unexpected output string@.%s@.Expected:@.%s@." output expected;
  [%expect ""]

let modulus = Int32.(shift_left 1l 31)

let canonicalize x = if in_range x then x else Int32.(sub x modulus)

let canon_equal x y = Int32.( = ) (canonicalize x) (canonicalize y)

let%expect_test _ =
  Test.check_exn
  @@ Test.make ~count:1000 ~name:"Int31.neg" t_corner ~print:print_t (fun i ->
         let r_int31 = Int31.(neg i |> to_int32) in
         let r_int32 = Int32.neg (Int31.to_int32 i) in
         in_range r_int31 && canon_equal r_int31 r_int32);
  [%expect ""]

let binop_prop op_i31 op_i32 i j =
  let r_int31 = op_i31 i j |> Int31.to_int32 in
  let r_int32 = op_i32 (Int31.to_int32 i) (Int31.to_int32 j) in
  in_range r_int31 && canon_equal r_int31 r_int32

let binop_check ~count ~name op_i31 op_i32 =
  Test.check_exn
  @@ Test.make
       ~count
       ~name
       Gen.(tup2 t_corner t_corner)
       ~print:(Print.tup2 print_t print_t)
       (fun (i, j) -> binop_prop op_i31 op_i32 i j)

let%expect_test _ =
  binop_check ~count:1000 ~name:"Int31.add" Int31.add Int32.add;
  [%expect ""]

let%expect_test _ =
  binop_check ~count:1000 ~name:"Int31.sub" Int31.sub Int32.sub;
  [%expect ""]

let%expect_test _ =
  binop_check ~count:1000 ~name:"Int31.mul" Int31.mul Int32.mul;
  [%expect ""]

let%expect_test _ =
  Test.check_exn
  @@ Test.make
       ~count:1000
       ~name:"Int31.div"
       Gen.(tup2 t_corner t_corner)
       ~print:(Print.tup2 print_t print_t)
       (fun (i, j) ->
         try binop_prop Int31.div Int32.div i j
         with Division_by_zero -> Int32.equal (Int31.to_int32 j) 0l)

let%expect_test _ =
  Test.check_exn
  @@ Test.make
       ~count:1000
       ~name:"Int31.rem"
       Gen.(tup2 t_corner t_corner)
       ~print:(Print.tup2 print_t print_t)
       (fun (i, j) ->
         try binop_prop Int31.rem Int32.rem i j
         with Division_by_zero -> Int32.equal (Int31.to_int32 j) 0l)

let%expect_test _ =
  binop_check ~count:1000 ~name:"Int31.logand" Int31.logand Int32.logand;
  [%expect ""]

let%expect_test _ =
  binop_check ~count:1000 ~name:"Int31.logor" Int31.logor Int32.logor;
  [%expect ""]

let%expect_test _ =
  binop_check ~count:1000 ~name:"Int31.logxor" Int31.logxor Int32.logxor;
  [%expect ""]

let shift_op_prop op_i31 op_i32 x i =
  let r_int31 = op_i31 x i |> Int31.to_int32 in
  let r_int32 = op_i32 (Int31.to_int32 x) i in
  in_range r_int31 && canon_equal r_int31 r_int32

let%expect_test _ =
  Test.check_exn
  @@ Test.make
       ~count:1000
       ~name:"Int31.shift_left"
       Gen.(tup2 t_corner (int_bound 31))
       ~print:Print.(tup2 print_t int)
       (fun (x, i) -> shift_op_prop Int31.shift_left Int32.shift_left x i)

let%expect_test _ =
  Test.check_exn
  @@ Test.make
       ~count:1000
       ~name:"Int31.shift_right"
       Gen.(tup2 t_corner (int_bound 31))
       ~print:Print.(tup2 print_t int)
       (fun (x, i) -> shift_op_prop Int31.shift_right Int32.shift_right x i)

(* Logical implication *)
let ( --> ) p q = (not p) || q

let%expect_test _ =
  Test.check_exn
  @@ Test.make
       ~count:10_000
       ~name:"Int31.shift_right_logical"
       Gen.(tup2 t_corner (int_bound 31))
       ~print:Print.(tup2 print_t int)
       (fun (x, i) ->
         let r_int31 = Int31.shift_right_logical x i |> Int31.to_int32 in
         let x_int32 = Int31.to_int32 x in
         let r_int32 =
           if Int_replace_polymorphic_compare.(i = 0)
           then x_int32
           else Int32.(shift_right_logical (logand 0x7fffffffl x_int32) i)
         in
         (* The bits should be unchanged if the shift amount is zero, otherwise they should
            match the result of shifting the 31 lower bits of the canonical representation *)
         in_range r_int31
         && Int32.equal r_int31 r_int32
         && (Int.equal i 0 --> Int32.(r_int31 = x_int32)));
  [%expect ""]
