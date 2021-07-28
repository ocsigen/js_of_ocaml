(* TEST *)

(* modified glibc's fma() tests *)

let string_of_fpclass = function
  | Float.FP_normal -> "normal"
  | FP_subnormal -> "subnormal"
  | FP_zero -> "zero"
  | FP_infinite -> "infinite"
  | FP_nan -> "nan"

let error x y z r c =
  Printf.fprintf
    stdout
    "FAIL!\tfma (%h, %h, %h) returned %h (%s) instead of %h.\n"
    x
    y
    z
    c
    (string_of_fpclass (Float.classify_float c))
    (List.hd r)

let success () = print_endline "OK!"

let fma_test x y z r =
  let c = Float.fma x y z in
  if List.exists (fun i -> i = c) r then success () else error x y z r c

(* test case description:

   (string * float * float * float * float list)
    |        |       |       |       |
    id       |       |       |       IEEE compliant result in head,
             |       |       |       or, accepted fma emulation approximation
             |       |       |       results in tail (if any)
             |       |       |
             x       y       z   -> operands as in fma x y z
*)
let%expect_test _ =
  fma_test 0x1p+0 0x2p+0 0x3p+0 [ 0x5p+0 ];
  [%expect {| OK! |}];
  fma_test 0x1.4p+0 0xcp-4 0x1p-4 [ 0x1p+0 ];
  [%expect {| OK! |}];
  fma_test 0x0p+0 0x0p+0 0x0p+0 [ 0x0p+0 ];
  [%expect {| OK! |}];
  fma_test 0x0p+0 0x0p+0 ~-.0x0p+0 [ 0x0p+0 ];
  [%expect {| OK! |}];
  fma_test 0x0p+0 ~-.0x0p+0 0x0p+0 [ 0x0p+0 ];
  [%expect {| OK! |}];
  fma_test 0x0p+0 ~-.0x0p+0 ~-.0x0p+0 [ ~-.0x0p+0 ];
  [%expect {| OK! |}];
  fma_test ~-.0x0p+0 0x0p+0 0x0p+0 [ 0x0p+0 ];
  [%expect {| OK! |}];
  fma_test ~-.0x0p+0 0x0p+0 ~-.0x0p+0 [ ~-.0x0p+0 ];
  [%expect {| OK! |}];
  fma_test ~-.0x0p+0 ~-.0x0p+0 0x0p+0 [ 0x0p+0 ];
  [%expect {| OK! |}];
  fma_test ~-.0x0p+0 ~-.0x0p+0 ~-.0x0p+0 [ 0x0p+0 ];
  [%expect {| OK! |}];
  fma_test 0x1p+0 0x0p+0 0x0p+0 [ 0x0p+0 ];
  [%expect {| OK! |}];
  fma_test 0x1p+0 0x0p+0 ~-.0x0p+0 [ 0x0p+0 ];
  [%expect {| OK! |}];
  fma_test 0x1p+0 ~-.0x0p+0 0x0p+0 [ 0x0p+0 ];
  [%expect {| OK! |}];
  fma_test 0x1p+0 ~-.0x0p+0 ~-.0x0p+0 [ ~-.0x0p+0 ];
  [%expect {| OK! |}];
  fma_test ~-.0x1p+0 0x0p+0 0x0p+0 [ 0x0p+0 ];
  [%expect {| OK! |}];
  fma_test ~-.0x1p+0 0x0p+0 ~-.0x0p+0 [ ~-.0x0p+0 ];
  [%expect {| OK! |}];
  fma_test ~-.0x1p+0 ~-.0x0p+0 0x0p+0 [ 0x0p+0 ];
  [%expect {| OK! |}];
  fma_test ~-.0x1p+0 ~-.0x0p+0 ~-.0x0p+0 [ 0x0p+0 ];
  [%expect {| OK! |}];
  fma_test 0x0p+0 0x1p+0 0x0p+0 [ 0x0p+0 ];
  [%expect {| OK! |}];
  fma_test 0x0p+0 0x1p+0 ~-.0x0p+0 [ 0x0p+0 ];
  [%expect {| OK! |}];
  fma_test 0x0p+0 ~-.0x1p+0 0x0p+0 [ 0x0p+0 ];
  [%expect {| OK! |}];
  fma_test 0x0p+0 ~-.0x1p+0 ~-.0x0p+0 [ ~-.0x0p+0 ];
  [%expect {| OK! |}];
  fma_test ~-.0x0p+0 0x1p+0 0x0p+0 [ 0x0p+0 ];
  [%expect {| OK! |}];
  fma_test ~-.0x0p+0 0x1p+0 ~-.0x0p+0 [ ~-.0x0p+0 ];
  [%expect {| OK! |}];
  fma_test ~-.0x0p+0 ~-.0x1p+0 0x0p+0 [ 0x0p+0 ];
  [%expect {| OK! |}];
  fma_test ~-.0x0p+0 ~-.0x1p+0 ~-.0x0p+0 [ 0x0p+0 ];
  [%expect {| OK! |}];
  fma_test 0x1p+0 0x1p+0 ~-.0x1p+0 [ 0x0p+0 ];
  [%expect {| OK! |}];
  fma_test 0x1p+0 ~-.0x1p+0 0x1p+0 [ 0x0p+0 ];
  [%expect {| OK! |}];
  fma_test ~-.0x1p+0 0x1p+0 0x1p+0 [ 0x0p+0 ];
  [%expect {| OK! |}];
  fma_test ~-.0x1p+0 ~-.0x1p+0 ~-.0x1p+0 [ 0x0p+0 ];
  [%expect {| OK! |}];
  fma_test 0x0p+0 0x0p+0 0x1p+0 [ 0x1p+0 ];
  [%expect {| OK! |}];
  fma_test 0x0p+0 0x0p+0 0x2p+0 [ 0x2p+0 ];
  [%expect {| OK! |}];
  fma_test 0x0p+0 0x0p+0 0xf.fffffp+124 [ 0xf.fffffp+124 ];
  [%expect {| OK! |}];
  fma_test 0x0p+0 0x0p+0 0xf.ffffffffffff8p+1020 [ 0xf.ffffffffffff8p+1020 ];
  [%expect {| OK! |}];
  fma_test 0x0p+0 0x1p+0 0x1p+0 [ 0x1p+0 ];
  [%expect {| OK! |}];
  fma_test 0x1p+0 0x0p+0 0x1p+0 [ 0x1p+0 ];
  [%expect {| OK! |}];
  fma_test 0x0p+0 0x1p+0 0x2p+0 [ 0x2p+0 ];
  [%expect {| OK! |}];
  fma_test 0x1p+0 0x0p+0 0x2p+0 [ 0x2p+0 ];
  [%expect {| OK! |}];
  fma_test 0x0p+0 0x1p+0 0xf.fffffp+124 [ 0xf.fffffp+124 ];
  [%expect {| OK! |}];
  fma_test 0x0p+0 0x1p+0 0xf.ffffffffffff8p+1020 [ 0xf.ffffffffffff8p+1020 ];
  [%expect {| OK! |}];
  fma_test 0x1p+0 0x0p+0 0xf.fffffp+124 [ 0xf.fffffp+124 ];
  [%expect {| OK! |}];
  fma_test 0x1p+0 0x0p+0 0xf.ffffffffffff8p+1020 [ 0xf.ffffffffffff8p+1020 ];
  [%expect {| OK! |}];
  fma_test 0x4p-128 0x4p-128 0x0p+0 [ 0x1p-252 ];
  [%expect {| OK! |}];
  fma_test 0x4p-128 0x4p-1024 0x0p+0 [ 0x0p+0 ];
  [%expect {| OK! |}];
  fma_test 0x4p-128 0x8p-972 0x0p+0 [ 0x0p+0 ];
  [%expect {| OK! |}];
  fma_test 0x4p-1024 0x4p-128 0x0p+0 [ 0x0p+0 ];
  [%expect {| OK! |}];
  fma_test 0x4p-1024 0x4p-1024 0x0p+0 [ 0x0p+0 ];
  [%expect {| OK! |}];
  fma_test 0x4p-1024 0x8p-972 0x0p+0 [ 0x0p+0 ];
  [%expect {| OK! |}];
  fma_test 0x8p-972 0x4p-128 0x0p+0 [ 0x0p+0 ];
  [%expect {| OK! |}];
  fma_test 0x8p-972 0x4p-1024 0x0p+0 [ 0x0p+0 ];
  [%expect {| OK! |}];
  fma_test 0x8p-972 0x8p-972 0x0p+0 [ 0x0p+0 ];
  [%expect {| OK! |}];
  fma_test 0x4p-128 0x4p-128 ~-.0x0p+0 [ 0x1p-252 ];
  [%expect {| OK! |}];
  fma_test 0x4p-128 0x4p-1024 ~-.0x0p+0 [ 0x0p+0 ];
  [%expect {| OK! |}];
  fma_test 0x4p-128 0x8p-972 ~-.0x0p+0 [ 0x0p+0 ];
  [%expect {| OK! |}];
  fma_test 0x4p-1024 0x4p-128 ~-.0x0p+0 [ 0x0p+0 ];
  [%expect {| OK! |}];
  fma_test 0x4p-1024 0x4p-1024 ~-.0x0p+0 [ 0x0p+0 ];
  [%expect {| OK! |}];
  fma_test 0x4p-1024 0x8p-972 ~-.0x0p+0 [ 0x0p+0 ];
  [%expect {| OK! |}];
  fma_test 0x8p-972 0x4p-128 ~-.0x0p+0 [ 0x0p+0 ];
  [%expect {| OK! |}];
  fma_test 0x8p-972 0x4p-1024 ~-.0x0p+0 [ 0x0p+0 ];
  [%expect {| OK! |}];
  fma_test 0x8p-972 0x8p-972 ~-.0x0p+0 [ 0x0p+0 ];
  [%expect {| OK! |}];
  fma_test 0x4p-128 ~-.0x4p-128 0x0p+0 [ ~-.0x1p-252 ];
  [%expect {| OK! |}];
  fma_test 0x4p-128 ~-.0x4p-1024 0x0p+0 [ ~-.0x0p+0 ];
  [%expect {| OK! |}];
  fma_test 0x4p-128 ~-.0x8p-972 0x0p+0 [ ~-.0x0p+0 ];
  [%expect {| OK! |}];
  fma_test 0x4p-1024 ~-.0x4p-128 0x0p+0 [ ~-.0x0p+0 ];
  [%expect {| OK! |}];
  fma_test 0x4p-1024 ~-.0x4p-1024 0x0p+0 [ ~-.0x0p+0 ];
  [%expect {| OK! |}];
  fma_test 0x4p-1024 ~-.0x8p-972 0x0p+0 [ ~-.0x0p+0 ];
  [%expect {| OK! |}];
  fma_test 0x8p-972 ~-.0x4p-128 0x0p+0 [ ~-.0x0p+0 ];
  [%expect {| OK! |}];
  fma_test 0x8p-972 ~-.0x4p-1024 0x0p+0 [ ~-.0x0p+0 ];
  [%expect {| OK! |}];
  fma_test 0x8p-972 ~-.0x8p-972 0x0p+0 [ ~-.0x0p+0 ];
  [%expect {| OK! |}];
  fma_test 0x4p-128 ~-.0x4p-128 ~-.0x0p+0 [ ~-.0x1p-252 ];
  [%expect {| OK! |}];
  fma_test 0x4p-128 ~-.0x4p-1024 ~-.0x0p+0 [ ~-.0x0p+0 ];
  [%expect {| OK! |}];
  fma_test 0x4p-128 ~-.0x8p-972 ~-.0x0p+0 [ ~-.0x0p+0 ];
  [%expect {| OK! |}];
  fma_test 0x4p-1024 ~-.0x4p-128 ~-.0x0p+0 [ ~-.0x0p+0 ];
  [%expect {| OK! |}];
  fma_test 0x4p-1024 ~-.0x4p-1024 ~-.0x0p+0 [ ~-.0x0p+0 ];
  [%expect {| OK! |}];
  fma_test 0x4p-1024 ~-.0x8p-972 ~-.0x0p+0 [ ~-.0x0p+0 ];
  [%expect {| OK! |}];
  fma_test 0x8p-972 ~-.0x4p-128 ~-.0x0p+0 [ ~-.0x0p+0 ];
  [%expect {| OK! |}];
  fma_test 0x8p-972 ~-.0x4p-1024 ~-.0x0p+0 [ ~-.0x0p+0 ];
  [%expect {| OK! |}];
  fma_test 0x8p-972 ~-.0x8p-972 ~-.0x0p+0 [ ~-.0x0p+0 ];
  [%expect {| OK! |}];
  fma_test ~-.0x4p-128 0x4p-128 0x0p+0 [ ~-.0x1p-252 ];
  [%expect {| OK! |}];
  fma_test ~-.0x4p-128 0x4p-1024 0x0p+0 [ ~-.0x0p+0 ];
  [%expect {| OK! |}];
  fma_test ~-.0x4p-128 0x8p-972 0x0p+0 [ ~-.0x0p+0 ];
  [%expect {| OK! |}];
  fma_test ~-.0x4p-1024 0x4p-128 0x0p+0 [ ~-.0x0p+0 ];
  [%expect {| OK! |}];
  fma_test ~-.0x4p-1024 0x4p-1024 0x0p+0 [ ~-.0x0p+0 ];
  [%expect {| OK! |}];
  fma_test ~-.0x4p-1024 0x8p-972 0x0p+0 [ ~-.0x0p+0 ];
  [%expect {| OK! |}];
  fma_test ~-.0x8p-972 0x4p-128 0x0p+0 [ ~-.0x0p+0 ];
  [%expect {| OK! |}];
  fma_test ~-.0x8p-972 0x4p-1024 0x0p+0 [ ~-.0x0p+0 ];
  [%expect {| OK! |}];
  fma_test ~-.0x8p-972 0x8p-972 0x0p+0 [ ~-.0x0p+0 ];
  [%expect {| OK! |}];
  fma_test ~-.0x4p-128 0x4p-128 ~-.0x0p+0 [ ~-.0x1p-252 ];
  [%expect {| OK! |}];
  fma_test ~-.0x4p-128 0x4p-1024 ~-.0x0p+0 [ ~-.0x0p+0 ];
  [%expect {| OK! |}];
  fma_test ~-.0x4p-128 0x8p-972 ~-.0x0p+0 [ ~-.0x0p+0 ];
  [%expect {| OK! |}];
  fma_test ~-.0x4p-1024 0x4p-128 ~-.0x0p+0 [ ~-.0x0p+0 ];
  [%expect {| OK! |}];
  fma_test ~-.0x4p-1024 0x4p-1024 ~-.0x0p+0 [ ~-.0x0p+0 ];
  [%expect {| OK! |}];
  fma_test ~-.0x4p-1024 0x8p-972 ~-.0x0p+0 [ ~-.0x0p+0 ];
  [%expect {| OK! |}];
  fma_test ~-.0x8p-972 0x4p-128 ~-.0x0p+0 [ ~-.0x0p+0 ];
  [%expect {| OK! |}];
  fma_test ~-.0x8p-972 0x4p-1024 ~-.0x0p+0 [ ~-.0x0p+0 ];
  [%expect {| OK! |}];
  fma_test ~-.0x8p-972 0x8p-972 ~-.0x0p+0 [ ~-.0x0p+0 ];
  [%expect {| OK! |}];
  fma_test ~-.0x4p-128 ~-.0x4p-128 0x0p+0 [ 0x1p-252 ];
  [%expect {| OK! |}];
  fma_test ~-.0x4p-128 ~-.0x4p-1024 0x0p+0 [ 0x0p+0 ];
  [%expect {| OK! |}];
  fma_test ~-.0x4p-128 ~-.0x8p-972 0x0p+0 [ 0x0p+0 ];
  [%expect {| OK! |}];
  fma_test ~-.0x4p-1024 ~-.0x4p-128 0x0p+0 [ 0x0p+0 ];
  [%expect {| OK! |}];
  fma_test ~-.0x4p-1024 ~-.0x4p-1024 0x0p+0 [ 0x0p+0 ];
  [%expect {| OK! |}];
  fma_test ~-.0x4p-1024 ~-.0x8p-972 0x0p+0 [ 0x0p+0 ];
  [%expect {| OK! |}];
  fma_test ~-.0x8p-972 ~-.0x4p-128 0x0p+0 [ 0x0p+0 ];
  [%expect {| OK! |}];
  fma_test ~-.0x8p-972 ~-.0x4p-1024 0x0p+0 [ 0x0p+0 ];
  [%expect {| OK! |}];
  fma_test ~-.0x8p-972 ~-.0x8p-972 0x0p+0 [ 0x0p+0 ];
  [%expect {| OK! |}];
  fma_test ~-.0x4p-128 ~-.0x4p-128 ~-.0x0p+0 [ 0x1p-252 ];
  [%expect {| OK! |}];
  fma_test ~-.0x4p-128 ~-.0x4p-1024 ~-.0x0p+0 [ 0x0p+0 ];
  [%expect {| OK! |}];
  fma_test ~-.0x4p-128 ~-.0x8p-972 ~-.0x0p+0 [ 0x0p+0 ];
  [%expect {| OK! |}];
  fma_test ~-.0x4p-1024 ~-.0x4p-128 ~-.0x0p+0 [ 0x0p+0 ];
  [%expect {| OK! |}];
  fma_test ~-.0x4p-1024 ~-.0x4p-1024 ~-.0x0p+0 [ 0x0p+0 ];
  [%expect {| OK! |}];
  fma_test ~-.0x4p-1024 ~-.0x8p-972 ~-.0x0p+0 [ 0x0p+0 ];
  [%expect {| OK! |}];
  fma_test ~-.0x8p-972 ~-.0x4p-128 ~-.0x0p+0 [ 0x0p+0 ];
  [%expect {| OK! |}];
  fma_test ~-.0x8p-972 ~-.0x4p-1024 ~-.0x0p+0 [ 0x0p+0 ];
  [%expect {| OK! |}];
  fma_test ~-.0x8p-972 ~-.0x8p-972 ~-.0x0p+0 [ 0x0p+0 ];
  [%expect {| OK! |}];
  fma_test 0xf.fffffp+124 0xf.fffffp+124 0x4p-128 [ 0xf.ffffe000001p+252 ];
  [%expect {| OK! |}];
  fma_test 0xf.fffffp+124 0xf.fffffp+124 0x4p-1024 [ 0xf.ffffe000001p+252 ];
  [%expect {| OK! |}];
  fma_test 0xf.fffffp+124 0xf.fffffp+124 0x8p-972 [ 0xf.ffffe000001p+252 ];
  [%expect {| OK! |}];
  fma_test 0xf.fffffp+124 0xf.ffffffffffff8p+1020 0x4p-128 [ infinity ];
  [%expect {| OK! |}];
  fma_test 0xf.fffffp+124 0xf.ffffffffffff8p+1020 0x4p-1024 [ infinity ];
  [%expect {| OK! |}];
  fma_test 0xf.fffffp+124 0xf.ffffffffffff8p+1020 0x8p-972 [ infinity ];
  [%expect {| OK! |}];
  fma_test 0xf.ffffffffffff8p+1020 0xf.fffffp+124 0x4p-128 [ infinity ];
  [%expect {| OK! |}];
  fma_test 0xf.ffffffffffff8p+1020 0xf.fffffp+124 0x4p-1024 [ infinity ];
  [%expect {| OK! |}];
  fma_test 0xf.ffffffffffff8p+1020 0xf.fffffp+124 0x8p-972 [ infinity ];
  [%expect {| OK! |}];
  fma_test 0xf.ffffffffffff8p+1020 0xf.ffffffffffff8p+1020 0x4p-128 [ infinity ];
  [%expect {| OK! |}];
  fma_test 0xf.ffffffffffff8p+1020 0xf.ffffffffffff8p+1020 0x4p-1024 [ infinity ];
  [%expect {| OK! |}];
  fma_test 0xf.ffffffffffff8p+1020 0xf.ffffffffffff8p+1020 0x8p-972 [ infinity ];
  [%expect {| OK! |}];
  fma_test 0xf.fffffp+124 0xf.fffffp+124 ~-.0x4p-128 [ 0xf.ffffe000001p+252 ];
  [%expect {| OK! |}];
  fma_test 0xf.fffffp+124 0xf.fffffp+124 ~-.0x4p-1024 [ 0xf.ffffe000001p+252 ];
  [%expect {| OK! |}];
  fma_test 0xf.fffffp+124 0xf.fffffp+124 ~-.0x8p-972 [ 0xf.ffffe000001p+252 ];
  [%expect {| OK! |}];
  fma_test 0xf.fffffp+124 0xf.ffffffffffff8p+1020 ~-.0x4p-128 [ infinity ];
  [%expect {| OK! |}];
  fma_test 0xf.fffffp+124 0xf.ffffffffffff8p+1020 ~-.0x4p-1024 [ infinity ];
  [%expect {| OK! |}];
  fma_test 0xf.fffffp+124 0xf.ffffffffffff8p+1020 ~-.0x8p-972 [ infinity ];
  [%expect {| OK! |}];
  fma_test 0xf.ffffffffffff8p+1020 0xf.fffffp+124 ~-.0x4p-128 [ infinity ];
  [%expect {| OK! |}];
  fma_test 0xf.ffffffffffff8p+1020 0xf.fffffp+124 ~-.0x4p-1024 [ infinity ];
  [%expect {| OK! |}];
  fma_test 0xf.ffffffffffff8p+1020 0xf.fffffp+124 ~-.0x8p-972 [ infinity ];
  [%expect {| OK! |}];
  fma_test 0xf.ffffffffffff8p+1020 0xf.ffffffffffff8p+1020 ~-.0x4p-128 [ infinity ];
  [%expect {| OK! |}];
  fma_test 0xf.ffffffffffff8p+1020 0xf.ffffffffffff8p+1020 ~-.0x4p-1024 [ infinity ];
  [%expect {| OK! |}];
  fma_test 0xf.ffffffffffff8p+1020 0xf.ffffffffffff8p+1020 ~-.0x8p-972 [ infinity ];
  [%expect {| OK! |}];
  fma_test 0xf.fffffp+124 ~-.0xf.fffffp+124 0x4p-128 [ ~-.0xf.ffffe000001p+252 ];
  [%expect {| OK! |}];
  fma_test 0xf.fffffp+124 ~-.0xf.fffffp+124 0x4p-1024 [ ~-.0xf.ffffe000001p+252 ];
  [%expect {| OK! |}];
  fma_test 0xf.fffffp+124 ~-.0xf.fffffp+124 0x8p-972 [ ~-.0xf.ffffe000001p+252 ];
  [%expect {| OK! |}];
  fma_test 0xf.fffffp+124 ~-.0xf.ffffffffffff8p+1020 0x4p-128 [ ~-.infinity ];
  [%expect {| OK! |}];
  fma_test 0xf.fffffp+124 ~-.0xf.ffffffffffff8p+1020 0x4p-1024 [ ~-.infinity ];
  [%expect {| OK! |}];
  fma_test 0xf.fffffp+124 ~-.0xf.ffffffffffff8p+1020 0x8p-972 [ ~-.infinity ];
  [%expect {| OK! |}];
  fma_test 0xf.ffffffffffff8p+1020 ~-.0xf.fffffp+124 0x4p-128 [ ~-.infinity ];
  [%expect {| OK! |}];
  fma_test 0xf.ffffffffffff8p+1020 ~-.0xf.fffffp+124 0x4p-1024 [ ~-.infinity ];
  [%expect {| OK! |}];
  fma_test 0xf.ffffffffffff8p+1020 ~-.0xf.fffffp+124 0x8p-972 [ ~-.infinity ];
  [%expect {| OK! |}];
  fma_test 0xf.ffffffffffff8p+1020 ~-.0xf.ffffffffffff8p+1020 0x4p-128 [ ~-.infinity ];
  [%expect {| OK! |}];
  fma_test 0xf.ffffffffffff8p+1020 ~-.0xf.ffffffffffff8p+1020 0x4p-1024 [ ~-.infinity ];
  [%expect {| OK! |}];
  fma_test 0xf.ffffffffffff8p+1020 ~-.0xf.ffffffffffff8p+1020 0x8p-972 [ ~-.infinity ];
  [%expect {| OK! |}];
  fma_test 0xf.fffffp+124 ~-.0xf.fffffp+124 ~-.0x4p-128 [ ~-.0xf.ffffe000001p+252 ];
  [%expect {| OK! |}];
  fma_test 0xf.fffffp+124 ~-.0xf.fffffp+124 ~-.0x4p-1024 [ ~-.0xf.ffffe000001p+252 ];
  [%expect {| OK! |}];
  fma_test 0xf.fffffp+124 ~-.0xf.fffffp+124 ~-.0x8p-972 [ ~-.0xf.ffffe000001p+252 ];
  [%expect {| OK! |}];
  fma_test 0xf.fffffp+124 ~-.0xf.ffffffffffff8p+1020 ~-.0x4p-128 [ ~-.infinity ];
  [%expect {| OK! |}];
  fma_test 0xf.fffffp+124 ~-.0xf.ffffffffffff8p+1020 ~-.0x4p-1024 [ ~-.infinity ];
  [%expect {| OK! |}];
  fma_test 0xf.fffffp+124 ~-.0xf.ffffffffffff8p+1020 ~-.0x8p-972 [ ~-.infinity ];
  [%expect {| OK! |}];
  fma_test 0xf.ffffffffffff8p+1020 ~-.0xf.fffffp+124 ~-.0x4p-128 [ ~-.infinity ];
  [%expect {| OK! |}];
  fma_test 0xf.ffffffffffff8p+1020 ~-.0xf.fffffp+124 ~-.0x4p-1024 [ ~-.infinity ];
  [%expect {| OK! |}];
  fma_test 0xf.ffffffffffff8p+1020 ~-.0xf.fffffp+124 ~-.0x8p-972 [ ~-.infinity ];
  [%expect {| OK! |}];
  fma_test 0xf.ffffffffffff8p+1020 ~-.0xf.ffffffffffff8p+1020 ~-.0x4p-128 [ ~-.infinity ];
  [%expect {| OK! |}];
  fma_test 0xf.ffffffffffff8p+1020 ~-.0xf.ffffffffffff8p+1020 ~-.0x4p-1024 [ ~-.infinity ];
  [%expect {| OK! |}];
  fma_test 0xf.ffffffffffff8p+1020 ~-.0xf.ffffffffffff8p+1020 ~-.0x8p-972 [ ~-.infinity ];
  [%expect {| OK! |}];
  fma_test ~-.0xf.fffffp+124 0xf.fffffp+124 0x4p-128 [ ~-.0xf.ffffe000001p+252 ];
  [%expect {| OK! |}];
  fma_test ~-.0xf.fffffp+124 0xf.fffffp+124 0x4p-1024 [ ~-.0xf.ffffe000001p+252 ];
  [%expect {| OK! |}];
  fma_test ~-.0xf.fffffp+124 0xf.fffffp+124 0x8p-972 [ ~-.0xf.ffffe000001p+252 ];
  [%expect {| OK! |}];
  fma_test ~-.0xf.fffffp+124 0xf.ffffffffffff8p+1020 0x4p-128 [ ~-.infinity ];
  [%expect {| OK! |}];
  fma_test ~-.0xf.fffffp+124 0xf.ffffffffffff8p+1020 0x4p-1024 [ ~-.infinity ];
  [%expect {| OK! |}];
  fma_test ~-.0xf.fffffp+124 0xf.ffffffffffff8p+1020 0x8p-972 [ ~-.infinity ];
  [%expect {| OK! |}];
  fma_test ~-.0xf.ffffffffffff8p+1020 0xf.fffffp+124 0x4p-128 [ ~-.infinity ];
  [%expect {| OK! |}];
  fma_test ~-.0xf.ffffffffffff8p+1020 0xf.fffffp+124 0x4p-1024 [ ~-.infinity ];
  [%expect {| OK! |}];
  fma_test ~-.0xf.ffffffffffff8p+1020 0xf.fffffp+124 0x8p-972 [ ~-.infinity ];
  [%expect {| OK! |}];
  fma_test ~-.0xf.ffffffffffff8p+1020 0xf.ffffffffffff8p+1020 0x4p-128 [ ~-.infinity ];
  [%expect {| OK! |}];
  fma_test ~-.0xf.ffffffffffff8p+1020 0xf.ffffffffffff8p+1020 0x4p-1024 [ ~-.infinity ];
  [%expect {| OK! |}];
  fma_test ~-.0xf.ffffffffffff8p+1020 0xf.ffffffffffff8p+1020 0x8p-972 [ ~-.infinity ];
  [%expect {| OK! |}];
  fma_test ~-.0xf.fffffp+124 0xf.fffffp+124 ~-.0x4p-128 [ ~-.0xf.ffffe000001p+252 ];
  [%expect {| OK! |}];
  fma_test ~-.0xf.fffffp+124 0xf.fffffp+124 ~-.0x4p-1024 [ ~-.0xf.ffffe000001p+252 ];
  [%expect {| OK! |}];
  fma_test ~-.0xf.fffffp+124 0xf.fffffp+124 ~-.0x8p-972 [ ~-.0xf.ffffe000001p+252 ];
  [%expect {| OK! |}];
  fma_test ~-.0xf.fffffp+124 0xf.ffffffffffff8p+1020 ~-.0x4p-128 [ ~-.infinity ];
  [%expect {| OK! |}];
  fma_test ~-.0xf.fffffp+124 0xf.ffffffffffff8p+1020 ~-.0x4p-1024 [ ~-.infinity ];
  [%expect {| OK! |}];
  fma_test ~-.0xf.fffffp+124 0xf.ffffffffffff8p+1020 ~-.0x8p-972 [ ~-.infinity ];
  [%expect {| OK! |}];
  fma_test ~-.0xf.ffffffffffff8p+1020 0xf.fffffp+124 ~-.0x4p-128 [ ~-.infinity ];
  [%expect {| OK! |}];
  fma_test ~-.0xf.ffffffffffff8p+1020 0xf.fffffp+124 ~-.0x4p-1024 [ ~-.infinity ];
  [%expect {| OK! |}];
  fma_test ~-.0xf.ffffffffffff8p+1020 0xf.fffffp+124 ~-.0x8p-972 [ ~-.infinity ];
  [%expect {| OK! |}];
  fma_test ~-.0xf.ffffffffffff8p+1020 0xf.ffffffffffff8p+1020 ~-.0x4p-128 [ ~-.infinity ];
  [%expect {| OK! |}];
  fma_test ~-.0xf.ffffffffffff8p+1020 0xf.ffffffffffff8p+1020 ~-.0x4p-1024 [ ~-.infinity ];
  [%expect {| OK! |}];
  fma_test ~-.0xf.ffffffffffff8p+1020 0xf.ffffffffffff8p+1020 ~-.0x8p-972 [ ~-.infinity ];
  [%expect {| OK! |}];
  fma_test ~-.0xf.fffffp+124 ~-.0xf.fffffp+124 0x4p-128 [ 0xf.ffffe000001p+252 ];
  [%expect {| OK! |}];
  fma_test ~-.0xf.fffffp+124 ~-.0xf.fffffp+124 0x4p-1024 [ 0xf.ffffe000001p+252 ];
  [%expect {| OK! |}];
  fma_test ~-.0xf.fffffp+124 ~-.0xf.fffffp+124 0x8p-972 [ 0xf.ffffe000001p+252 ];
  [%expect {| OK! |}];
  fma_test ~-.0xf.fffffp+124 ~-.0xf.ffffffffffff8p+1020 0x4p-128 [ infinity ];
  [%expect {| OK! |}];
  fma_test ~-.0xf.fffffp+124 ~-.0xf.ffffffffffff8p+1020 0x4p-1024 [ infinity ];
  [%expect {| OK! |}];
  fma_test ~-.0xf.fffffp+124 ~-.0xf.ffffffffffff8p+1020 0x8p-972 [ infinity ];
  [%expect {| OK! |}];
  fma_test ~-.0xf.ffffffffffff8p+1020 ~-.0xf.fffffp+124 0x4p-128 [ infinity ];
  [%expect {| OK! |}];
  fma_test ~-.0xf.ffffffffffff8p+1020 ~-.0xf.fffffp+124 0x4p-1024 [ infinity ];
  [%expect {| OK! |}];
  fma_test ~-.0xf.ffffffffffff8p+1020 ~-.0xf.fffffp+124 0x8p-972 [ infinity ];
  [%expect {| OK! |}];
  fma_test ~-.0xf.ffffffffffff8p+1020 ~-.0xf.ffffffffffff8p+1020 0x4p-128 [ infinity ];
  [%expect {| OK! |}];
  fma_test ~-.0xf.ffffffffffff8p+1020 ~-.0xf.ffffffffffff8p+1020 0x4p-1024 [ infinity ];
  [%expect {| OK! |}];
  fma_test ~-.0xf.ffffffffffff8p+1020 ~-.0xf.ffffffffffff8p+1020 0x8p-972 [ infinity ];
  [%expect {| OK! |}];
  fma_test ~-.0xf.fffffp+124 ~-.0xf.fffffp+124 ~-.0x4p-128 [ 0xf.ffffe000001p+252 ];
  [%expect {| OK! |}];
  fma_test ~-.0xf.fffffp+124 ~-.0xf.fffffp+124 ~-.0x4p-1024 [ 0xf.ffffe000001p+252 ];
  [%expect {| OK! |}];
  fma_test ~-.0xf.fffffp+124 ~-.0xf.fffffp+124 ~-.0x8p-972 [ 0xf.ffffe000001p+252 ];
  [%expect {| OK! |}];
  fma_test ~-.0xf.fffffp+124 ~-.0xf.ffffffffffff8p+1020 ~-.0x4p-128 [ infinity ];
  [%expect {| OK! |}];
  fma_test ~-.0xf.fffffp+124 ~-.0xf.ffffffffffff8p+1020 ~-.0x4p-1024 [ infinity ];
  [%expect {| OK! |}];
  fma_test ~-.0xf.fffffp+124 ~-.0xf.ffffffffffff8p+1020 ~-.0x8p-972 [ infinity ];
  [%expect {| OK! |}];
  fma_test ~-.0xf.ffffffffffff8p+1020 ~-.0xf.fffffp+124 ~-.0x4p-128 [ infinity ];
  [%expect {| OK! |}];
  fma_test ~-.0xf.ffffffffffff8p+1020 ~-.0xf.fffffp+124 ~-.0x4p-1024 [ infinity ];
  [%expect {| OK! |}];
  fma_test ~-.0xf.ffffffffffff8p+1020 ~-.0xf.fffffp+124 ~-.0x8p-972 [ infinity ];
  [%expect {| OK! |}];
  fma_test ~-.0xf.ffffffffffff8p+1020 ~-.0xf.ffffffffffff8p+1020 ~-.0x4p-128 [ infinity ];
  [%expect {| OK! |}];
  fma_test ~-.0xf.ffffffffffff8p+1020 ~-.0xf.ffffffffffff8p+1020 ~-.0x4p-1024 [ infinity ];
  [%expect {| OK! |}];
  fma_test ~-.0xf.ffffffffffff8p+1020 ~-.0xf.ffffffffffff8p+1020 ~-.0x8p-972 [ infinity ];
  [%expect {| OK! |}];
  fma_test 0x2.fffp+12 0x1.000002p+0 0x1.ffffp-24 [ 0x2.fff006p+12 ];
  [%expect {| OK! |}];
  fma_test 0x1.fffp+0 0x1.00001p+0 ~-.0x1.fffp+0 [ 0x1.fffp-20 ];
  [%expect {| OK! |}];
  fma_test 0xc.d5e6fp+124 0x2.6af378p-128 ~-.0x1.f08948p+0 [ 0xd.da108p-28 ];
  [%expect {| OK! |}];
  fma_test 0x1.9abcdep+100 0x2.6af378p-128 ~-.0x3.e1129p-28 [ 0x1.bb421p-52 ];
  [%expect {| OK! |}];
  fma_test 0xf.fffffp+124 0x1.001p+0 ~-.0xf.fffffp+124 [ 0xf.fffffp+112 ];
  [%expect {| OK! |}];
  fma_test ~-.0xf.fffffp+124 0x1.fffffep+0 0xf.fffffp+124 [ ~-.0xf.ffffd000002p+124 ];
  [%expect {| OK! |}];
  fma_test 0xf.fffffp+124 0x2p+0 ~-.0xf.fffffp+124 [ 0xf.fffffp+124 ];
  [%expect {| OK! |}];
  fma_test 0x5p-128 0x8.00002p-4 0x1p-128 [ 0x3.80000ap-128 ];
  [%expect {| OK! |}];
  fma_test ~-.0x5p-128 0x8.00002p-4 ~-.0x1p-128 [ ~-.0x3.80000ap-128 ];
  [%expect {| OK! |}];
  fma_test 0x7.ffffep-128 0x8.00001p-4 0x8p-152 [ 0x3.ffffffffffep-128 ];
  [%expect {| OK! |}];
  fma_test ~-.0x7.ffffep-128 0x8.00001p-4 ~-.0x8p-152 [ ~-.0x3.ffffffffffep-128 ];
  [%expect {| OK! |}];
  fma_test 0x8p-152 0x8p-4 0x3.fffff8p-128 [ 0x3.fffffcp-128 ];
  [%expect {| OK! |}];
  fma_test ~-.0x8p-152 0x8p-4 ~-.0x3.fffff8p-128 [ ~-.0x3.fffffcp-128 ];
  [%expect {| OK! |}];
  fma_test 0x8p-152 0x8.8p-4 0x3.fffff8p-128 [ 0x3.fffffc4p-128 ];
  [%expect {| OK! |}];
  fma_test ~-.0x8p-152 0x8.8p-4 ~-.0x3.fffff8p-128 [ ~-.0x3.fffffc4p-128 ];
  [%expect {| OK! |}];
  fma_test 0x8p-152 0x8p-152 0x8p+124 [ 0x8p+124 ];
  [%expect {| OK! |}];
  fma_test 0x8p-152 ~-.0x8p-152 0x8p+124 [ 0x8p+124 ];
  [%expect {| OK! |}];
  fma_test 0x8p-152 0x8p-152 ~-.0x8p+124 [ ~-.0x8p+124 ];
  [%expect {| OK! |}];
  fma_test 0x8p-152 ~-.0x8p-152 ~-.0x8p+124 [ ~-.0x8p+124 ];
  [%expect {| OK! |}];
  fma_test 0x8p-152 0x8p-152 0x4p-128 [ 0x4p-128 ];
  [%expect {| OK! |}];
  fma_test 0x8p-152 ~-.0x8p-152 0x4p-128 [ 0x4p-128 ];
  [%expect {| OK! |}];
  fma_test 0x8p-152 0x8p-152 ~-.0x4p-128 [ ~-.0x4p-128 ];
  [%expect {| OK! |}];
  fma_test 0x8p-152 ~-.0x8p-152 ~-.0x4p-128 [ ~-.0x4p-128 ];
  [%expect {| OK! |}];
  fma_test 0x8p-152 0x8p-152 0x3.fffff8p-128 [ 0x3.fffff8p-128 ];
  [%expect {| OK! |}];
  fma_test 0x8p-152 ~-.0x8p-152 0x3.fffff8p-128 [ 0x3.fffff8p-128 ];
  [%expect {| OK! |}];
  fma_test 0x8p-152 0x8p-152 ~-.0x3.fffff8p-128 [ ~-.0x3.fffff8p-128 ];
  [%expect {| OK! |}];
  fma_test 0x8p-152 ~-.0x8p-152 ~-.0x3.fffff8p-128 [ ~-.0x3.fffff8p-128 ];
  [%expect {| OK! |}];
  fma_test 0x8p-152 0x8p-152 0x8p-152 [ 0x8p-152 ];
  [%expect {| OK! |}];
  fma_test 0x8p-152 ~-.0x8p-152 0x8p-152 [ 0x8p-152 ];
  [%expect {| OK! |}];
  fma_test 0x8p-152 0x8p-152 ~-.0x8p-152 [ ~-.0x8p-152 ];
  [%expect {| OK! |}];
  fma_test 0x8p-152 ~-.0x8p-152 ~-.0x8p-152 [ ~-.0x8p-152 ];
  [%expect {| OK! |}];
  fma_test 0xf.ffp-4 0xf.ffp-4 ~-.0xf.fep-4 [ 0x1p-24 ];
  [%expect {| OK! |}];
  fma_test 0xf.ffp-4 ~-.0xf.ffp-4 0xf.fep-4 [ ~-.0x1p-24 ];
  [%expect {| OK! |}];
  fma_test ~-.0xf.ffp-4 0xf.ffp-4 0xf.fep-4 [ ~-.0x1p-24 ];
  [%expect {| OK! |}];
  fma_test ~-.0xf.ffp-4 ~-.0xf.ffp-4 ~-.0xf.fep-4 [ 0x1p-24 ];
  [%expect {| OK! |}];
  fma_test 0x4.000008p-128 0x4.000008p-28 0x8p+124 [ 0x8p+124 ];
  [%expect {| OK! |}];
  fma_test 0x4.000008p-128 ~-.0x4.000008p-28 0x8p+124 [ 0x8p+124 ];
  [%expect {| OK! |}];
  fma_test 0x4.000008p-128 0x4.000008p-28 ~-.0x8p+124 [ ~-.0x8p+124 ];
  [%expect {| OK! |}];
  fma_test 0x4.000008p-128 ~-.0x4.000008p-28 ~-.0x8p+124 [ ~-.0x8p+124 ];
  [%expect {| OK! |}];
  fma_test 0x4.000008p-128 0x4.000008p-28 0x8p+100 [ 0x8p+100 ];
  [%expect {| OK! |}];
  fma_test 0x4.000008p-128 ~-.0x4.000008p-28 0x8p+100 [ 0x8p+100 ];
  [%expect {| OK! |}];
  fma_test 0x4.000008p-128 0x4.000008p-28 ~-.0x8p+100 [ ~-.0x8p+100 ];
  [%expect {| OK! |}];
  fma_test 0x4.000008p-128 ~-.0x4.000008p-28 ~-.0x8p+100 [ ~-.0x8p+100 ];
  [%expect {| OK! |}];
  fma_test
    0x2.fep+12
    0x1.0000000000001p+0
    0x1.ffep-48
    [ 0x2.fe00000000002p+12; 0x1.7f00000000002p+13 ];
  [%expect {| OK! |}];
  fma_test 0x1.fffp+0 0x1.0000000000001p+0 ~-.0x1.fffp+0 [ 0x1.fffp-52; 0x1p-51 ];
  [%expect {| OK! |}];
  fma_test 0x1.0000002p+0 0xf.fffffep-4 0x1p-300 [ 0x1p+0 ];
  [%expect {| OK! |}];
  fma_test 0x1.0000002p+0 0xf.fffffep-4 ~-.0x1p-300 [ 0xf.ffffffffffff8p-4; 0x1p+0 ];
  [%expect {| OK! |}];
  fma_test
    0xe.f56df7797f768p+1020
    0x3.7ab6fbbcbfbb4p-1024
    ~-.0x3.40bf1803497f6p+0
    [ 0x8.4c4b43de4ed2p-56; 0x1.095f287bc9da4p-53; 0x1.098p-53 ];
  [%expect {| OK! |}];
  fma_test
    0x1.deadbeef2feedp+900
    0x3.7ab6fbbcbfbb4p-1024
    ~-.0x6.817e300692fecp-124
    [ 0x1.0989687bc9da4p-176; 0x1.095f287bc9da4p-176; 0x1.098p-176 ];
  [%expect {| OK! |}];
  fma_test
    0xf.ffffffffffff8p+1020
    0x1.001p+0
    ~-.0xf.ffffffffffff8p+1020
    [ 0xf.ffffffffffff8p+1008; 0x1p+1012 ];
  [%expect {| OK! |}];
  fma_test
    ~-.0xf.ffffffffffff8p+1020
    0x1.fffffffffffffp+0
    0xf.ffffffffffff8p+1020
    [ ~-.0xf.fffffffffffe8p+1020 ];
  [%expect {| OK! |}];
  fma_test
    0xf.ffffffffffff8p+1020
    0x2p+0
    ~-.0xf.ffffffffffff8p+1020
    [ 0xf.ffffffffffff8p+1020 ];
  [%expect {| OK! |}];
  fma_test 0x5.a827999fcef3p-540 0x5.a827999fcef3p-540 0x0p+0 [ 0x0p+0 ];
  [%expect {| OK! |}];
  fma_test
    0x3.bd5b7dde5fddap-496
    0x3.bd5b7dde5fddap-496
    ~-.0xd.fc352bc352bap-992
    [ 0x1.0989687cp-1044; 0x0.000004277ca1fp-1022; 0x0.00000428p-1022 ];
  [%expect {| OK! |}];
  fma_test
    0x3.bd5b7dde5fddap-504
    0x3.bd5b7dde5fddap-504
    ~-.0xd.fc352bc352bap-1008
    [ 0x1.0988p-1060; 0x0.0000000004278p-1022; 0x0.000000000428p-1022 ];
  [%expect {| OK! |}];
  fma_test 0x8p-540 0x4p-540 0x4p-1076 [ 0x8p-1076 ];
  [%expect {| OK! |}];
  fma_test
    0x1.7fffff8p-968
    0x4p-108
    0x4p-1048
    [ 0x4.0000004p-1048; 0x0.0000010000002p-1022 ];
  [%expect {| OK! |}];
  fma_test
    0x2.8000008p-968
    0x4p-108
    0x4p-1048
    [ 0x4.000000cp-1048; 0x0.0000010000002p-1022 ];
  [%expect {| OK! |}];
  fma_test 0x2.8p-968 ~-.0x4p-108 ~-.0x4p-1048 [ ~-.0x4.0000008p-1048 ];
  [%expect {| OK! |}];
  fma_test
    ~-.0x2.33956cdae7c2ep-960
    0x3.8e211518bfea2p-108
    ~-.0x2.02c2b59766d9p-1024
    [ ~-.0x2.02c2b59767564p-1024 ];
  [%expect {| OK! |}];
  fma_test
    ~-.0x3.a5d5dadd1d3a6p-980
    ~-.0x2.9c0cd8c5593bap-64
    ~-.0x2.49179ac00d15p-1024
    [ ~-.0x2.491702717ed74p-1024 ];
  [%expect {| OK! |}];
  fma_test
    0x2.2a7aca1773e0cp-908
    0x9.6809186a42038p-128
    ~-.0x2.c9e356b3f0fp-1024
    [ ~-.0x2.c89d5c48eefa4p-1024; ~-.0x0.b22757123bbe8p-1022 ];
  [%expect {| OK! |}];
  fma_test
    ~-.0x3.ffffffffffffep-712
    0x3.ffffffffffffep-276
    0x3.fffffc0000ffep-984
    [ 0x2.fffffc0000ffep-984; 0x1.7ffffe00008p-983 ];
  [%expect {| OK! |}];
  fma_test 0x5p-1024 0x8.000000000001p-4 0x1p-1024 [ 0x3.8000000000004p-1024 ];
  [%expect {| OK! |}];
  fma_test ~-.0x5p-1024 0x8.000000000001p-4 ~-.0x1p-1024 [ ~-.0x3.8000000000004p-1024 ];
  [%expect {| OK! |}];
  fma_test 0x7.ffffffffffffp-1024 0x8.0000000000008p-4 0x4p-1076 [ 0x4p-1024 ];
  [%expect {| OK! |}];
  fma_test ~-.0x7.ffffffffffffp-1024 0x8.0000000000008p-4 ~-.0x4p-1076 [ ~-.0x4p-1024 ];
  [%expect {| OK! |}];
  fma_test 0x4p-1076 0x8p-4 0x3.ffffffffffffcp-1024 [ 0x4p-1024 ];
  [%expect {| OK! |}];
  fma_test ~-.0x4p-1076 0x8p-4 ~-.0x3.ffffffffffffcp-1024 [ ~-.0x4p-1024 ];
  [%expect {| OK! |}];
  fma_test 0x4p-1076 0x8.8p-4 0x3.ffffffffffffcp-1024 [ 0x4p-1024 ];
  [%expect {| OK! |}];
  fma_test ~-.0x4p-1076 0x8.8p-4 ~-.0x3.ffffffffffffcp-1024 [ ~-.0x4p-1024 ];
  [%expect {| OK! |}];
  fma_test 0x4p-1076 0x4p-1076 0x8p+1020 [ 0x8p+1020 ];
  [%expect {| OK! |}];
  fma_test 0x4p-1076 ~-.0x4p-1076 0x8p+1020 [ 0x8p+1020 ];
  [%expect {| OK! |}];
  fma_test 0x4p-1076 0x4p-1076 ~-.0x8p+1020 [ ~-.0x8p+1020 ];
  [%expect {| OK! |}];
  fma_test 0x4p-1076 ~-.0x4p-1076 ~-.0x8p+1020 [ ~-.0x8p+1020 ];
  [%expect {| OK! |}];
  fma_test 0x4p-1076 0x4p-1076 0x4p-1024 [ 0x4p-1024 ];
  [%expect {| OK! |}];
  fma_test 0x4p-1076 ~-.0x4p-1076 0x4p-1024 [ 0x4p-1024 ];
  [%expect {| OK! |}];
  fma_test 0x4p-1076 0x4p-1076 ~-.0x4p-1024 [ ~-.0x4p-1024 ];
  [%expect {| OK! |}];
  fma_test 0x4p-1076 ~-.0x4p-1076 ~-.0x4p-1024 [ ~-.0x4p-1024 ];
  [%expect {| OK! |}];
  fma_test 0x4p-1076 0x4p-1076 0x3.ffffffffffffcp-1024 [ 0x3.ffffffffffffcp-1024 ];
  [%expect {| OK! |}];
  fma_test 0x4p-1076 ~-.0x4p-1076 0x3.ffffffffffffcp-1024 [ 0x3.ffffffffffffcp-1024 ];
  [%expect {| OK! |}];
  fma_test 0x4p-1076 0x4p-1076 ~-.0x3.ffffffffffffcp-1024 [ ~-.0x3.ffffffffffffcp-1024 ];
  [%expect {| OK! |}];
  fma_test
    0x4p-1076
    ~-.0x4p-1076
    ~-.0x3.ffffffffffffcp-1024
    [ ~-.0x3.ffffffffffffcp-1024 ];
  [%expect {| OK! |}];
  fma_test 0x4p-1076 0x4p-1076 0x4p-1076 [ 0x4p-1076 ];
  [%expect {| OK! |}];
  fma_test 0x4p-1076 ~-.0x4p-1076 0x4p-1076 [ 0x4p-1076 ];
  [%expect {| OK! |}];
  fma_test 0x4p-1076 0x4p-1076 ~-.0x4p-1076 [ ~-.0x4p-1076 ];
  [%expect {| OK! |}];
  fma_test 0x4p-1076 ~-.0x4p-1076 ~-.0x4p-1076 [ ~-.0x4p-1076 ];
  [%expect {| OK! |}];
  fma_test
    0xf.ffffffffffff8p-4
    0xf.ffffffffffff8p-4
    ~-.0xf.ffffffffffffp-4
    [ 0x4p-108; 0x0p+0 ];
  [%expect {| OK! |}];
  fma_test
    0xf.ffffffffffff8p-4
    ~-.0xf.ffffffffffff8p-4
    0xf.ffffffffffffp-4
    [ ~-.0x4p-108; 0x0p+0 ];
  [%expect {| OK! |}];
  fma_test
    ~-.0xf.ffffffffffff8p-4
    0xf.ffffffffffff8p-4
    0xf.ffffffffffffp-4
    [ ~-.0x4p-108; 0x0p+0 ];
  [%expect {| OK! |}];
  fma_test
    ~-.0xf.ffffffffffff8p-4
    ~-.0xf.ffffffffffff8p-4
    ~-.0xf.ffffffffffffp-4
    [ 0x4p-108; 0x0p+0 ];
  [%expect {| OK! |}];
  fma_test 0x4.0000000000004p-1024 0x2.0000000000002p-56 0x8p+1020 [ 0x8p+1020 ];
  [%expect {| OK! |}];
  fma_test 0x4.0000000000004p-1024 ~-.0x2.0000000000002p-56 0x8p+1020 [ 0x8p+1020 ];
  [%expect {| OK! |}];
  fma_test 0x4.0000000000004p-1024 0x2.0000000000002p-56 ~-.0x8p+1020 [ ~-.0x8p+1020 ];
  [%expect {| OK! |}];
  fma_test 0x4.0000000000004p-1024 ~-.0x2.0000000000002p-56 ~-.0x8p+1020 [ ~-.0x8p+1020 ];
  [%expect {| OK! |}];
  fma_test 0x4.0000000000004p-1024 0x2.0000000000002p-56 0x4p+968 [ 0x4p+968 ];
  [%expect {| OK! |}];
  fma_test 0x4.0000000000004p-1024 ~-.0x2.0000000000002p-56 0x4p+968 [ 0x4p+968 ];
  [%expect {| OK! |}];
  fma_test 0x4.0000000000004p-1024 0x2.0000000000002p-56 ~-.0x4p+968 [ ~-.0x4p+968 ];
  [%expect {| OK! |}];
  fma_test 0x4.0000000000004p-1024 ~-.0x2.0000000000002p-56 ~-.0x4p+968 [ ~-.0x4p+968 ];
  [%expect {| OK! |}];
  fma_test 0x7.fffff8p-128 0x3.fffffcp+24 0xf.fffffp+124 [ 0xf.fffffp+124 ];
  [%expect {| OK! |}];
  fma_test 0x7.fffff8p-128 ~-.0x3.fffffcp+24 0xf.fffffp+124 [ 0xf.fffffp+124 ];
  [%expect {| OK! |}];
  fma_test 0x7.fffff8p-128 0x3.fffffcp+24 ~-.0xf.fffffp+124 [ ~-.0xf.fffffp+124 ];
  [%expect {| OK! |}];
  fma_test 0x7.fffff8p-128 ~-.0x3.fffffcp+24 ~-.0xf.fffffp+124 [ ~-.0xf.fffffp+124 ];
  [%expect {| OK! |}];
  fma_test
    0x7.ffffffffffffcp-1024
    0x7.ffffffffffffcp+52
    0xf.ffffffffffff8p+1020
    [ 0xf.ffffffffffff8p+1020 ];
  [%expect {| OK! |}];
  fma_test
    0x7.ffffffffffffcp-1024
    ~-.0x7.ffffffffffffcp+52
    0xf.ffffffffffff8p+1020
    [ 0xf.ffffffffffff8p+1020 ];
  [%expect {| OK! |}];
  fma_test
    0x7.ffffffffffffcp-1024
    0x7.ffffffffffffcp+52
    ~-.0xf.ffffffffffff8p+1020
    [ ~-.0xf.ffffffffffff8p+1020 ];
  [%expect {| OK! |}];
  fma_test
    0x7.ffffffffffffcp-1024
    ~-.0x7.ffffffffffffcp+52
    ~-.0xf.ffffffffffff8p+1020
    [ ~-.0xf.ffffffffffff8p+1020 ];
  [%expect {| OK! |}]
