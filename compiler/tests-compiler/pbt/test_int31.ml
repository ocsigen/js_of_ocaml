open! Js_of_ocaml_compiler.Stdlib
open QCheck2
module Int31 = Js_of_ocaml_compiler.Targetint

let () = Int31.set_num_bits 31

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

let sixty_four = Sys.int_size > 32

let%expect_test _ =
  let i = Gen.(generate1 (no_shrink out_of_range_i32)) in
  let i_trunc = Int32.(shift_right (shift_left i 1) 1) in
  ignore (Int31.of_int32_warning_on_overflow i);
  let output = [%expect.output] in
  let expected =
    Format.sprintf
      "Warning: integer overflow: int32 0x%lx (%ld) truncated to 0x%lx (%ld); the \
       generated code might be incorrect.@."
      i
      i
      i_trunc
      i_trunc
  in
  if sixty_four && not (String.equal output expected)
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
  if sixty_four && not (String.equal output expected)
  then Format.printf "Unexpected output string@.%s@.Expected:@.%s@." output expected;
  [%expect ""]

let%expect_test _ =
  let i = Gen.(generate1 (no_shrink (Nativeint.of_int <$> out_of_range_int))) in
  let i_trunc = Int32.(shift_right (shift_left (Nativeint.to_int32 i) 1) 1) in
  ignore (Int31.of_nativeint_warning_on_overflow i);
  let output = [%expect.output] in
  let expected =
    Format.sprintf
      "Warning: integer overflow: native integer 0x%nx (%nd) truncated to 0x%lx (%ld); \
       the generated code might be incorrect.@."
      i
      i
      i_trunc
      i_trunc
  in
  if sixty_four && not (String.equal output expected)
  then Format.printf "Unexpected output string@.%s@.Expected:@.%s@." output expected;
  [%expect ""]

let modulus = Int32.(shift_left 1l 31)

let canonicalize x = if in_range x then x else Int32.(sub x modulus)

let canon_equal x y = Int32.( = ) (canonicalize x) (canonicalize y)

module E = struct
  let zero = Int31.of_int32_warning_on_overflow 0l

  let one = Int31.of_int32_warning_on_overflow 1l

  let min_int = Int31.min_int ()

  let max_int = Int31.max_int ()

  let numbits = Int31.num_bits ()

  let cases =
    [ "zero", zero; "one", one; "-one", Int31.neg one; "min", min_int; "max", max_int ]

  let to_string_simple x =
    let x32 = Int31.to_int32 x in
    Format.sprintf "%ld" x32

  let to_string x =
    let x32 = Int31.to_int32 x in
    let ma32 = Int31.to_int32 max_int in
    let mi32 = Int31.to_int32 min_int in
    if Int32.(x32 < 0l && sub x32 mi32 < 10l)
    then
      let diff = Int32.(sub x32 mi32) in
      if Int32.(diff = 0l) then "min" else Printf.sprintf "min + %ld" diff
    else if Int32.(x32 > 0l && sub ma32 x32 < 10l)
    then
      let diff = Int32.(sub ma32 x32) in
      if Int32.(diff = 0l) then "max" else Printf.sprintf "max - %ld" diff
    else to_string_simple x

  let%expect_test _ =
    List.iter cases ~f:(fun (name, case) ->
        Printf.printf "%s = %s\n" name (to_string_simple case));
    [%expect
      {|
      zero = 0
      one = 1
      -one = -1
      min = -1073741824
      max = 1073741823
      |}]

  let%expect_test _ =
    List.iter cases ~f:(fun (name, case) ->
        Printf.printf "neg( %s ) = %s\n" name (to_string (Int31.neg case)));
    [%expect
      {|
      neg( zero ) = 0
      neg( one ) = -1
      neg( -one ) = 1
      neg( min ) = min
      neg( max ) = min + 1
      |}]

  let binop fname f =
    List.iter cases ~f:(fun (name1, case1) ->
        List.iter cases ~f:(fun (name2, case2) ->
            match to_string (f case1 case2) with
            | r -> Printf.printf "%s( %s, %s) = %s\n" fname name1 name2 r
            | exception _ -> Printf.printf "%s( %s, %s) = ERR\n" fname name1 name2))

  let%expect_test _ =
    binop "add" Int31.add;
    [%expect
      {|
      add( zero, zero) = 0
      add( zero, one) = 1
      add( zero, -one) = -1
      add( zero, min) = min
      add( zero, max) = max
      add( one, zero) = 1
      add( one, one) = 2
      add( one, -one) = 0
      add( one, min) = min + 1
      add( one, max) = min
      add( -one, zero) = -1
      add( -one, one) = 0
      add( -one, -one) = -2
      add( -one, min) = max
      add( -one, max) = max - 1
      add( min, zero) = min
      add( min, one) = min + 1
      add( min, -one) = max
      add( min, min) = 0
      add( min, max) = -1
      add( max, zero) = max
      add( max, one) = min
      add( max, -one) = max - 1
      add( max, min) = -1
      add( max, max) = -2
      |}]

  let%expect_test _ =
    binop "sub" Int31.sub;
    [%expect
      {|
      sub( zero, zero) = 0
      sub( zero, one) = -1
      sub( zero, -one) = 1
      sub( zero, min) = min
      sub( zero, max) = min + 1
      sub( one, zero) = 1
      sub( one, one) = 0
      sub( one, -one) = 2
      sub( one, min) = min + 1
      sub( one, max) = min + 2
      sub( -one, zero) = -1
      sub( -one, one) = -2
      sub( -one, -one) = 0
      sub( -one, min) = max
      sub( -one, max) = min
      sub( min, zero) = min
      sub( min, one) = max
      sub( min, -one) = min + 1
      sub( min, min) = 0
      sub( min, max) = 1
      sub( max, zero) = max
      sub( max, one) = max - 1
      sub( max, -one) = min
      sub( max, min) = -1
      sub( max, max) = 0
      |}]

  let%expect_test _ =
    binop "mul" Int31.mul;
    [%expect
      {|
      mul( zero, zero) = 0
      mul( zero, one) = 0
      mul( zero, -one) = 0
      mul( zero, min) = 0
      mul( zero, max) = 0
      mul( one, zero) = 0
      mul( one, one) = 1
      mul( one, -one) = -1
      mul( one, min) = min
      mul( one, max) = max
      mul( -one, zero) = 0
      mul( -one, one) = -1
      mul( -one, -one) = 1
      mul( -one, min) = min
      mul( -one, max) = min + 1
      mul( min, zero) = 0
      mul( min, one) = min
      mul( min, -one) = min
      mul( min, min) = 0
      mul( min, max) = min
      mul( max, zero) = 0
      mul( max, one) = max
      mul( max, -one) = min + 1
      mul( max, min) = min
      mul( max, max) = 1
      |}]

  let%expect_test _ =
    binop "div" Int31.div;
    [%expect
      {|
      div( zero, zero) = ERR
      div( zero, one) = 0
      div( zero, -one) = 0
      div( zero, min) = 0
      div( zero, max) = 0
      div( one, zero) = ERR
      div( one, one) = 1
      div( one, -one) = -1
      div( one, min) = 0
      div( one, max) = 0
      div( -one, zero) = ERR
      div( -one, one) = -1
      div( -one, -one) = 1
      div( -one, min) = 0
      div( -one, max) = 0
      div( min, zero) = ERR
      div( min, one) = min
      div( min, -one) = min
      div( min, min) = 1
      div( min, max) = -1
      div( max, zero) = ERR
      div( max, one) = max
      div( max, -one) = min + 1
      div( max, min) = 0
      div( max, max) = 1
      |}]

  let%expect_test _ =
    binop "rem" Int31.rem;
    [%expect
      {|
      rem( zero, zero) = ERR
      rem( zero, one) = 0
      rem( zero, -one) = 0
      rem( zero, min) = 0
      rem( zero, max) = 0
      rem( one, zero) = ERR
      rem( one, one) = 0
      rem( one, -one) = 0
      rem( one, min) = 1
      rem( one, max) = 1
      rem( -one, zero) = ERR
      rem( -one, one) = 0
      rem( -one, -one) = 0
      rem( -one, min) = -1
      rem( -one, max) = -1
      rem( min, zero) = ERR
      rem( min, one) = 0
      rem( min, -one) = 0
      rem( min, min) = 0
      rem( min, max) = -1
      rem( max, zero) = ERR
      rem( max, one) = 0
      rem( max, -one) = 0
      rem( max, min) = max
      rem( max, max) = 0
      |}]

  let%expect_test _ =
    binop "logand" Int31.logand;
    [%expect
      {|
      logand( zero, zero) = 0
      logand( zero, one) = 0
      logand( zero, -one) = 0
      logand( zero, min) = 0
      logand( zero, max) = 0
      logand( one, zero) = 0
      logand( one, one) = 1
      logand( one, -one) = 1
      logand( one, min) = 0
      logand( one, max) = 1
      logand( -one, zero) = 0
      logand( -one, one) = 1
      logand( -one, -one) = -1
      logand( -one, min) = min
      logand( -one, max) = max
      logand( min, zero) = 0
      logand( min, one) = 0
      logand( min, -one) = min
      logand( min, min) = min
      logand( min, max) = 0
      logand( max, zero) = 0
      logand( max, one) = 1
      logand( max, -one) = max
      logand( max, min) = 0
      logand( max, max) = max
      |}]

  let%expect_test _ =
    binop "logor" Int31.logor;
    [%expect
      {|
      logor( zero, zero) = 0
      logor( zero, one) = 1
      logor( zero, -one) = -1
      logor( zero, min) = min
      logor( zero, max) = max
      logor( one, zero) = 1
      logor( one, one) = 1
      logor( one, -one) = -1
      logor( one, min) = min + 1
      logor( one, max) = max
      logor( -one, zero) = -1
      logor( -one, one) = -1
      logor( -one, -one) = -1
      logor( -one, min) = -1
      logor( -one, max) = -1
      logor( min, zero) = min
      logor( min, one) = min + 1
      logor( min, -one) = -1
      logor( min, min) = min
      logor( min, max) = -1
      logor( max, zero) = max
      logor( max, one) = max
      logor( max, -one) = -1
      logor( max, min) = -1
      logor( max, max) = max
      |}]

  let%expect_test _ =
    binop "logxor" Int31.logxor;
    [%expect
      {|
      logxor( zero, zero) = 0
      logxor( zero, one) = 1
      logxor( zero, -one) = -1
      logxor( zero, min) = min
      logxor( zero, max) = max
      logxor( one, zero) = 1
      logxor( one, one) = 0
      logxor( one, -one) = -2
      logxor( one, min) = min + 1
      logxor( one, max) = max - 1
      logxor( -one, zero) = -1
      logxor( -one, one) = -2
      logxor( -one, -one) = 0
      logxor( -one, min) = max
      logxor( -one, max) = min
      logxor( min, zero) = min
      logxor( min, one) = min + 1
      logxor( min, -one) = max
      logxor( min, min) = 0
      logxor( min, max) = -1
      logxor( max, zero) = max
      logxor( max, one) = max - 1
      logxor( max, -one) = min
      logxor( max, min) = -1
      logxor( max, max) = 0
      |}]

  let shiftop fname f =
    List.iter cases ~f:(fun (name1, case1) ->
        List.iter [ 0; 1; numbits ] ~f:(fun shift ->
            match to_string (f case1 shift) with
            | r -> Printf.printf "%s( %s, %d) = %s\n" fname name1 shift r
            | exception _ -> Printf.printf "%s( %s, %d) = ERR\n" fname name1 shift))

  let%expect_test _ =
    shiftop "shift_left" Int31.shift_left;
    [%expect
      {|
      shift_left( zero, 0) = 0
      shift_left( zero, 1) = 0
      shift_left( zero, 31) = 0
      shift_left( one, 0) = 1
      shift_left( one, 1) = 2
      shift_left( one, 31) = 0
      shift_left( -one, 0) = -1
      shift_left( -one, 1) = -2
      shift_left( -one, 31) = 0
      shift_left( min, 0) = min
      shift_left( min, 1) = 0
      shift_left( min, 31) = 0
      shift_left( max, 0) = max
      shift_left( max, 1) = -2
      shift_left( max, 31) = 0
      |}]

  let%expect_test _ =
    shiftop "shift_right" Int31.shift_right;
    [%expect
      {|
      shift_right( zero, 0) = 0
      shift_right( zero, 1) = 0
      shift_right( zero, 31) = 0
      shift_right( one, 0) = 1
      shift_right( one, 1) = 0
      shift_right( one, 31) = 0
      shift_right( -one, 0) = -1
      shift_right( -one, 1) = -1
      shift_right( -one, 31) = -1
      shift_right( min, 0) = min
      shift_right( min, 1) = -536870912
      shift_right( min, 31) = -1
      shift_right( max, 0) = max
      shift_right( max, 1) = 536870911
      shift_right( max, 31) = 0
      |}]

  let%expect_test _ =
    shiftop "shift_right_logical" Int31.shift_right_logical;
    [%expect
      {|
      shift_right_logical( zero, 0) = 0
      shift_right_logical( zero, 1) = 0
      shift_right_logical( zero, 31) = 0
      shift_right_logical( one, 0) = 1
      shift_right_logical( one, 1) = 0
      shift_right_logical( one, 31) = 0
      shift_right_logical( -one, 0) = -1
      shift_right_logical( -one, 1) = max
      shift_right_logical( -one, 31) = 0
      shift_right_logical( min, 0) = min
      shift_right_logical( min, 1) = 536870912
      shift_right_logical( min, 31) = 0
      shift_right_logical( max, 0) = max
      shift_right_logical( max, 1) = 536870911
      shift_right_logical( max, 31) = 0
      |}]
end

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
