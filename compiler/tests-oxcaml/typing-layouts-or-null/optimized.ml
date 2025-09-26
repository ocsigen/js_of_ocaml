(* TEST
 reference = "${test_source_directory}/optimized.reference";
 flambda2;
 {
   native;
 } {
   flags = "-O3";
   native;
 } {
   flags = "-Oclassic";
   native;
 } {
   bytecode;
 }
*)

let print_int_or_null prefix x =
  match x with
  | Null -> Printf.printf "%s: Null\n" prefix
  | This n -> Printf.printf "%s: This %d\n" prefix n

let print_float_or_null prefix x =
  match x with
  | Null -> Printf.printf "%s: Null\n" prefix
  | This n -> Printf.printf "%s: This %.2f\n" prefix n

let print_string_or_null prefix x =
  match x with
  | Null -> Printf.printf "%s: Null\n" prefix
  | This s -> Printf.printf "%s: This %s\n" prefix s

let double x = x * 2
let square x = x * x

(************************************************)
(* Test: Function inlining with or_null types *)

let[@inline] add_or_null a b =
  match (a, b) with
  | (This x, This y) -> This (x + y)
  | _ -> Null

let[@inline] multiply_or_null a b =
  match (a, b) with
  | (This x, This y) -> This (x *. y)
  | _ -> Null

let[@inline] divide_or_null a b =
  match (a, b) with
  | (This x, This y) ->
      if y = 0 then Null else This (x / y)
  | _ -> Null

let test_inlined_functions () =
  print_int_or_null "Inline Test, 5 + 7 inlined"
    ((add_or_null [@inlined]) (This 5) (This 7));
  print_int_or_null "Inline Test, 5 + 7 not inlined"
    ((add_or_null [@inlined never]) (This 5) (This 7));

  print_float_or_null "Inline Test, 6 * 8 inlined"
    ((multiply_or_null [@inlined]) (This 6.0) (This 8.0));
  print_float_or_null "Inline Test, 6 * 8 not inlined"
    ((multiply_or_null [@inlined never]) (This 6.0) (This 8.0));

  print_int_or_null "Inline Test, 20 / 4 inlined"
    ((divide_or_null [@inlined]) (This 20) (This 4));
  print_int_or_null "Inline Test, 20 / 4 not inlined"
    ((divide_or_null [@inlined never]) (This 20) (This 4));

  print_int_or_null "Inline Test, 5 + null inlined"
    ((add_or_null [@inlined]) (This 5) Null);
  print_float_or_null "Inline Test, null * 4 inlined"
    ((multiply_or_null [@inlined]) Null (This 4.0));
  print_int_or_null "Inline Test, null / 4 inlined"
    ((divide_or_null [@inlined]) Null (This 4));
  print_int_or_null "Inline Test, 20 / 0 inlined"
    ((divide_or_null [@inlined]) (This 20) (This 0));
;;

let () = test_inlined_functions ()

(************************************************)
(* Test: Higher-order functions with or_null types *)

let[@inline never] map_or_null f x =
  match x with
  | Null -> Null
  | This v -> This (f v)

let[@inline never] map_or_null_default f default x =
  match x with
  | Null -> default
  | This v -> This (f v)

let[@inline never] and_then f x =
  match x with
  | Null -> Null
  | This v -> f v

let[@inline never] unwrap_or default x =
  match x with
  | Null -> default
  | This v -> v

let[@inline always] compose f g x = f (g x)
let[@inline never] compose_never f g x = f (g x)

let[@inline always] apply_twice f x = f (f x)
let[@inline never] apply_twice_never f x = f (f x)

let test_higher_order () =
  let a = This 5 in
  let b = Null in

  print_int_or_null "HO Test, double 5" (map_or_null double a);
  print_int_or_null "HO Test, square 5" (map_or_null square a);
  print_int_or_null "HO Test, double null" (map_or_null double b);

  let default_val = This 42 in
  print_int_or_null "HO Test, double 5 or default"
    (map_or_null_default double default_val a);
  print_int_or_null "HO Test, double null or default"
    (map_or_null_default double default_val b);

  let safe_div x = if x = 0 then Null else This (100 / x) in
  print_int_or_null "HO Test, 100/5" (and_then safe_div (This 5));
  print_int_or_null "HO Test, 100/0" (and_then safe_div (This 0));
  print_int_or_null "HO Test, 100/null" (and_then safe_div Null);

  Printf.printf "HO Test, unwrap 5 or 10: %d\n" (unwrap_or 10 (This 5));
  Printf.printf "HO Test, unwrap null or 10: %d\n" (unwrap_or 10 Null);

  let x = This 4 in
  print_int_or_null "HO Test, square(double(4))"
    (and_then (fun v -> map_or_null square (This v))
      (map_or_null double x));

  print_int_or_null "HO Test, compose double square 3"
    (map_or_null (compose double square) (This 3));
  print_int_or_null "HO Test, compose_never double square 3"
    (map_or_null (compose_never double square) (This 3));
  print_int_or_null "HO Test, compose double null"
    (map_or_null (compose double square) Null);
  print_int_or_null "HO Test, compose_never double null"
    (map_or_null (compose_never double square) Null);

  let opaque_double = Sys.opaque_identity double in
  print_int_or_null "HO Test, map with opaque double 7"
    (map_or_null opaque_double (This 7));
  print_int_or_null "HO Test, map with opaque double null"
    (map_or_null opaque_double Null);

  print_int_or_null "HO Test, apply_twice double 5"
    (map_or_null (apply_twice double) (This 5));
  print_int_or_null "HO Test, apply_twice_never double 5"
    (map_or_null (apply_twice_never double) (This 5));
  print_int_or_null "HO Test, apply_twice double null"
    (map_or_null (apply_twice double) Null);
  print_int_or_null "HO Test, apply_twice_never double null"
    (map_or_null (apply_twice_never double) Null);
  print_int_or_null "HO Test, apply_twice map_or_null double null"
    (apply_twice (map_or_null double) Null);

  let chain =
    and_then (fun v ->
      map_or_null (compose double square) Null)
      (apply_twice (map_or_null double) (This 1))
  in
  print_int_or_null "HO Test, complex chain with inlining" chain

let () = test_higher_order ()

(************************************************)
(* Test: Closures capturing or_null values *)

let test_closures () =
  let[@inline never] increment_by offset x =
    match (x, offset) with
    | This n, This m -> This (n + m)
    | _, _ -> Null
  in

  let inc_by_five = increment_by (This 5) in
  print_int_or_null "Closure Test, increment 10 by 5" (inc_by_five (This 10));
  print_int_or_null "Closure Test, increment null by 5" (inc_by_five Null);

  let inc_by_null = increment_by Null in
  print_int_or_null "Closure Test, increment 10 with null offset" (inc_by_null (This 10));

  let[@inline never] make_accumulator initial =
    let state = ref initial in
    fun x ->
      match x with
      | This n ->
          (match !state with
          | This m ->
              let result = This (n + m) in
              state := result;
              result
          | Null -> Null)
      | Null -> Null
  in

  let acc = make_accumulator (This 5) in
  print_int_or_null "Closure Test, accumulate 3 from 5" (acc (This 3));
  print_int_or_null "Closure Test, accumulate 8 from 8" (acc (This 8));
  print_int_or_null "Closure Test, accumulate null from 16" (acc Null);
  let acc_null = make_accumulator Null in
  print_int_or_null "Closure Test, accumulate 3 from null" (acc_null (This 3));

  let[@inline never] make_division_chain divisor =
    let[@inline never] divide_by n =
      match n with
      | This x ->
          (match divisor with
          | This d when d <> 0 -> This (x / d)
          | _ -> Null)
      | Null -> Null
    in
    divide_by
  in

  let divide_by_four = make_division_chain (This 4) in
  let divide_by_five = make_division_chain (This 5) in
  let divide_by_zero = make_division_chain (This 0) in
  let divide_by_null = make_division_chain Null in

  print_int_or_null "Closure Test, 100/4" (divide_by_four (This 100));
  print_int_or_null "Closure Test, 25/5" (divide_by_five (This 25));
  print_int_or_null "Closure Test, 5/0" (divide_by_zero (This 5));
  print_int_or_null "Closure Test, null/5" (divide_by_null (This 5));

  let[@inline never] multi_arg_float_function a1 f1 a2 f2 a3 f3 a4 f4 =
    match (a1, a2, a3, a4) with
    | (This x1, This x2, This x3, This x4) ->
        let result = (float_of_int x1) *. f1 +. (float_of_int x2) *. f2 +.
                     (float_of_int x3) *. f3 +. (float_of_int x4) *. f4 in
        This result
    | _ -> Null
  in

  let[@inline never] create_float_closure a1 f1 a2 f2 =
    fun b1 g1 b2 g2 ->
      match (a1, a2, b1, b2) with
      | (This v1, This v2, This w1, This w2) ->
          let result = ((float_of_int v1) *. f1 +. (float_of_int w1) *. g1) *.
                       ((float_of_int v2) *. f2 +. (float_of_int w2) *. g2) in
          This result
      | _ -> Null
  in

  print_float_or_null "Closure Test, float multi-arg all valid"
    (multi_arg_float_function (This 10) 2.5 (This 20) 1.5
                              (This 30) 0.5 (This 40) 0.25);

  print_float_or_null "Closure Test, float multi-arg one null"
    (multi_arg_float_function (This 10) 2.5 (This 20) 1.5
                              Null 0.5 (This 40) 0.25);

  let float_math_fn = create_float_closure (This 5) 2.0 (This 10) 3.0 in

  print_float_or_null "Closure Test, float closure all valid"
    (float_math_fn (This 15) 1.5 (This 20) 0.5);

  print_float_or_null "Closure Test, float closure one null"
    (float_math_fn (This 15) 1.5 Null 0.5);

  let float_partial_fn = multi_arg_float_function (This 25) 1.1 (This 35) 0.9 in
  print_float_or_null "Closure Test, float partial multi-arg all valid"
    (float_partial_fn (This 45) 0.7 (This 55) 0.3);

  print_float_or_null "Closure Test, float partial multi-arg one null"
    (float_partial_fn Null 0.7 (This 55) 0.3);

  let[@inline never] complex_float_function a1 f1 a2 f2 a3 f3 a4 f4 a5 f5 a6 f6 =
    match (a1, a2, a3, a4, a5, a6) with
    | (This x1, This x2, This x3, This x4, This x5, This x6) ->
        let sum1 = (float_of_int x1) *. f1 +. (float_of_int x2) *. f2 in
        let sum2 = (float_of_int x3) *. f3 +. (float_of_int x4) *. f4 in
        let sum3 = (float_of_int x5) *. f5 +. (float_of_int x6) *. f6 in
        let result = sum1 *. sum2 *. sum3 in
        This result
    | _ -> Null
  in

  print_float_or_null "Closure Test, complex float function all valid"
    (complex_float_function
      (This 5) 1.1 (This 10) 2.2 (This 15) 3.3
      (This 20) 4.4 (This 25) 5.5 (This 30) 6.6);

  print_float_or_null "Closure Test, complex float function one null"
    (complex_float_function
      (This 5) 1.1 (This 10) 2.2 (This 15) 3.3
      Null 4.4 (This 25) 5.5 (This 30) 6.6)
;;

let () = test_closures ()

(************************************************)
(* Test: Partial application with or_null types *)

let[@inline never] add_values_float (a : float or_null) (b : float or_null) : float or_null =
  match (a, b) with
  | (This x, This y) -> This (x +. y)
  | _ -> Null

let[@inline never] compute_with_floats
    (a : int or_null) f1 (b : int or_null) f2
    (c : int or_null) f3 : float or_null =
  match (a, b, c) with
  | (This x, This y, This z) ->
      This ((float_of_int x) *. f1 +. (float_of_int y) *. f2 +. (float_of_int z) *. f3)
  | _ -> Null

let[@inline never] make_weighted_adder
    (a : int or_null) w1 (b : int or_null) w2 =
  fun (c : int or_null) w3 (d : int or_null) w4 ->
    match (a, b, c, d) with
    | (This v1, This v2, This v3, This v4) ->
        This ((float_of_int v1) *. w1 +. (float_of_int v2) *. w2 +.
              (float_of_int v3) *. w3 +. (float_of_int v4) *. w4)
    | _ -> Null

let test_partial_application () =
  let add_ten = add_values_float (This 10.0) in
  print_float_or_null "PA Test 1.1, add_ten(5.0)" (add_ten (This 5.0));
  print_float_or_null "PA Test 1.2, add_ten(Null)" (add_ten Null);

  let opaque_add_ten = Sys.opaque_identity add_ten in
  print_float_or_null "PA Test 1.3, opaque_add_ten(7.5)" (opaque_add_ten (This 7.5));

  let add_null = add_values_float Null in
  print_float_or_null "PA Test 2.1, add_null(3.0)" (add_null (This 3.0));

  let compute_part1 = compute_with_floats (This 10) 1.5 (This 20) 0.5 in
  print_float_or_null "PA Test 3.1, compute_part1(This 30)" (compute_part1 (This 30) 0.25);
  print_float_or_null "PA Test 3.2, compute_part1(Null)" (compute_part1 Null 0.25);

  let opaque_compute = Sys.opaque_identity compute_part1 in
  print_float_or_null "PA Test 3.3, opaque_compute(This 15)" (opaque_compute (This 15) 0.75);

  let compute_part2 = compute_with_floats (This 5) 2.0 Null 1.0 in
  print_float_or_null "PA Test 4.1, compute_part2(This 25)" (compute_part2 (This 25) 0.5);

  let weighted_adder = make_weighted_adder (This 5) 1.0 (This 10) 0.5 in
  print_float_or_null "PA Test 5.1, weighted_adder full application"
    (weighted_adder (This 15) 0.25 (This 20) 0.125);

  let opaque_adder = Sys.opaque_identity weighted_adder in
  print_float_or_null "PA Test 5.2, opaque_adder full application"
    (opaque_adder (This 25) 0.2 (This 30) 0.1);

  let adder_stage2 = weighted_adder (This 50) 0.05 in
  print_float_or_null "PA Test 5.3, adder_stage2 application"
    (adder_stage2 (This 100) 0.01);

  let opaque_stage2 = Sys.opaque_identity adder_stage2 in
  print_float_or_null "PA Test 5.4, opaque_stage2 application"
    (opaque_stage2 (This 200) 0.005);

  let add_five = add_values_float (This 5.0) in
  let opaque_add_five = Sys.opaque_identity add_five in

  print_float_or_null "PA Test 6.1, apply_twice add_five to 10.0"
    (apply_twice opaque_add_five (This 10.0));

  let add_seven = add_values_float (This 7.0) in
  let composed = compose opaque_add_five add_seven in

  print_float_or_null "PA Test 7.1, (add_five . add_seven)(3.0)"
    (composed (This 3.0));
;;

let () = test_partial_application ()


(************************************************)
(* Test: Over application with or_null types *)

let[@inline never] f_over n m =
  let[@inline never] go f =
    f (and_then (fun x -> This (x + 1)) n)
  in
  go

let test_over_application () =
  let five = This 5 in
  let null_val = Null in

  let () =
    f_over five null_val
      (fun n s -> print_int_or_null s n) "OA Test, This 6"
  in

  let () =
    f_over null_val five
      (fun n s -> print_int_or_null s n) "OA Test, Null"
  in
  ()

let () = test_over_application ()

(************************************************)
(* Test: Methods with or_null types *)

let make_or_null_math () = object
  method add (a : int or_null) (b : int or_null) : int or_null =
    match (a, b) with
    | (This x, This y) -> This (x + y)
    | _ -> Null

  method multiply (a : int or_null) (b : int or_null) : int or_null =
    match (a, b) with
    | (This x, This y) -> This (x * y)
    | _ -> Null

  method transform (a : int or_null) (f : int -> int) : int or_null =
    match a with
    | This x -> This (f x)
    | Null -> Null
end

let make_recursive_processor n = object(self)
  method process (value : string or_null) (count : int) : string or_null =
    if count = 0 then
      value
    else
      match value with
      | This s -> self#process (This (s ^ n)) (count - 1)
      | Null -> Null
end

let make_over_applier n = object
  method apply (value : int or_null) f =
    match value with
    | This x -> f (x + n) value
    | Null -> f 0 value
end

let test_methods () =
  let math = make_or_null_math () in
  print_int_or_null "Method Test 1.1, add(5,7)" (math#add (This 5) (This 7));
  print_int_or_null "Method Test 1.2, add(null,7)" (math#add Null (This 7));
  print_int_or_null "Method Test 1.3, multiply(3,4)" (math#multiply (This 3) (This 4));
  print_int_or_null "Method Test 1.4, transform(9,square)" (math#transform (This 9) (fun x -> x * x));
  print_int_or_null "Method Test 1.5, transform(null,square)" (math#transform Null (fun x -> x * x));

  let opaque_math = Sys.opaque_identity math in
  print_int_or_null "Method Test 2.1, opaque add(8,3)" (opaque_math#add (This 8) (This 3));
  print_int_or_null "Method Test 2.2, opaque multiply(6,7)" (opaque_math#multiply (This 6) (This 7));

  let processor = make_recursive_processor "_suffix" in
  let opaque_processor = Sys.opaque_identity processor in

  print_string_or_null "Method Test 3.1, recursive process" (opaque_processor#process (This "base") 3);
  print_string_or_null "Method Test 3.2, recursive process null" (opaque_processor#process Null 3);

  let applier = make_over_applier 10 in
  let opaque_applier = Sys.opaque_identity applier in

  let () =
    opaque_applier#apply (This 5)
      (fun n v s -> print_int_or_null s v)
      "Method Test 4.1, over apply 5"
  in

  let () =
    opaque_applier#apply Null
      (fun n v s -> print_int_or_null s v)
      "Method Test 4.2, over apply null"
  in

  let result = math#transform (math#add (This 3) (This 4)) (fun x -> x * 2) in
  print_int_or_null "Method Test 5.1, transform(add(3,4), double)" result;

  let result = math#multiply (math#transform (This 5) (fun x -> x + 1)) (This 3) in
  print_int_or_null "Method Test 5.2, multiply(transform(5,inc), 3)" result;

  let result = math#transform (math#multiply (This 4) (This 5)) (fun x -> x / 2) in
  print_int_or_null "Method Test 5.3, transform(multiply(4,5), half)" result;

  let result = opaque_math#transform (opaque_math#add (This 6) (This 7)) (fun x -> x * 3) in
  print_int_or_null "Method Test 5.4, opaque transform(add(6,7), triple)" result
;;

let () = test_methods ()

(************************************************)
(* Test: letop with or_null values *)

let ( let* ) x f =
  match x with
  | This v -> f v
  | Null -> Null

let ( and* ) x y =
  match (x, y) with
  | (This v, This w) -> This (v, w)
  | _ -> Null

let test_letops () =
  let result1 =
    let* v = This 10 in
    This (v + 5)
  in
  print_int_or_null "Letop Test 1.1, This 10 -> This 15" result1;

  let result2 =
    let* v = Null in
    This (v + 5)
  in
  print_int_or_null "Letop Test 1.2, Null -> Null" result2;

  let double x = x * 2 in
  let add_three x = x + 3 in

  let result3 =
    let* v = This 7 in
    let* w = This (double v) in
    This (add_three w)
  in
  print_int_or_null "Letop Test 2.1, chained operations on 7" result3;

  let result4 =
    let* v = This 7 in
    let* w = Null in
    This (add_three w)
  in
  print_int_or_null "Letop Test 2.2, chain with Null" result4;

  let safe_div a b =
    if b = 0 then Null else This (a / b)
  in

  let result5 =
    let* x = This 20
    and* y = This 4 in
    safe_div x y
  in
  print_int_or_null "Letop Test 3.1, and* with 20/4" result5;

  let result6 =
    let* x = This 20
    and* y = This 0 in
    safe_div x y
  in
  print_int_or_null "Letop Test 3.2, and* with 20/0" result6;

  let result6b =
    let* x = This 20
    and* y = Null in
    safe_div x y
  in
  print_int_or_null "Letop Test 3.3, and* with Null in second arg" result6b;

  let result6c =
    let* x = Null
    and* y = This 5 in
    safe_div x y
  in
  print_int_or_null "Letop Test 3.4, and* with Null in first arg" result6c;

  let combine a b c =
    match (a, b, c) with
    | (va, vb, vc) -> This (va + vb * vc)
  in

  let result7 =
    let* x = This 5
    and* y = This 10 in
    let* z = This 3 in
    combine x y z
  in
  print_int_or_null "Letop Test 4.1, combine 5 10 3" result7;

  let result8 =
    let* a = This 7 in
    let* b = This 3
    and* c = This 4 in
    let* d = This (a + b * c) in
    This (d * 2)
  in
  print_int_or_null "Letop Test 5.1, complex nested ops" result8;

  let safe_sqrt x =
    if x < 0 then Null else This (Float.sqrt (float_of_int x))
  in

  let float_result1 =
    let* x = This 16 in
    safe_sqrt x
  in
  print_float_or_null "Letop Test 6.1, sqrt 16" float_result1;

  let float_result2 =
    let* x = This (-5) in
    safe_sqrt x
  in
  print_float_or_null "Letop Test 6.2, sqrt -5" float_result2
;;

let () = test_letops ()

(************************************************)
(* Test: Local continuations with or_null types *)

let test_local_continuations () =
  let five = This 5 in
  let null_val = Null in

  let[@inline never] test_local_cont value =
    let[@local] handle_value x =
      match x with
      | This n -> This (n * 2)
      | Null -> Null
    in
    handle_value value
  in

  print_int_or_null "Local Cont Test 1.1, double This 5" (test_local_cont five);
  print_int_or_null "Local Cont Test 1.2, double Null" (test_local_cont null_val);

  let[@inline never] test_local_capture multiplier value =
    let[@local] multiply x =
      match multiplier with
      | This n -> This (n * x)
      | Null -> Null
    in
    multiply value
  in

  print_int_or_null "Local Cont Test 2.1, multiply 5 by 3" (test_local_capture (This 3) 5);
  print_int_or_null "Local Cont Test 2.2, multiply 3 by null" (test_local_capture Null 3);

  let[@inline never] test_conditional a b cond =
    let[@local] handle_values x y =
      match (x, y) with
      | (This m, This n) -> This (m + 2 * n)
      | _ -> Null
    in
    if cond then
      handle_values a b
    else
      handle_values b a
  in

  print_int_or_null "Local Cont Test 3.1, conditional true" (test_conditional five (This 10) true);
  print_int_or_null "Local Cont Test 3.2, conditional false" (test_conditional five (This 10) false);
  print_int_or_null "Local Cont Test 3.3, with null true" (test_conditional five null_val true);
;;

let () = test_local_continuations ()

(************************************************)
(* Test: Loopification with or_null types *)

let[@loop] rec sum_numbers n acc =
  match acc with
  | Null -> Null
  | This total ->
      if n <= 0 then acc
      else sum_numbers (n - 1) (This (total + n))

let test_loopification () =
  print_int_or_null "Loop Test 1, sum 1 to 5" (sum_numbers 5 (This 0));
  print_int_or_null "Loop Test 2, sum with null" (sum_numbers 5 Null);
;;

let () = test_loopification ()

(************************************************)
(* Test: Variant payloads with or_null types *)

type mixed_variant =
  | A
  | B
  | C of int
  | D of string
  | E of float * float

let print_mixed_variant_or_null prefix x =
  match x with
  | Null -> Printf.printf "%s: Null\n" prefix
  | This v ->
      match v with
      | A -> Printf.printf "%s: This A\n" prefix
      | B -> Printf.printf "%s: This B\n" prefix
      | C n -> Printf.printf "%s: This C(%d)\n" prefix n
      | D s -> Printf.printf "%s: This D(%s)\n" prefix s
      | E (f1, f2) -> Printf.printf "%s: This E(%.2f,%.2f)\n" prefix f1 f2

let[@inline] transform_variant variant =
  match variant with
  | This A -> This B
  | This B -> This A
  | This (C n) -> This (C (n * 2))
  | This (D s) -> This (D (s ^ s))
  | This (E (f1, f2)) -> This (E (f2, f1))
  | Null -> Null

let[@inline never] transform_variant_nonneg variant =
  match variant with
  | This A -> This B
  | This B -> This A
  | This (C n) when n >= 0 -> This (C (n * 2))
  | This (C _) -> Null
  | This (D s) -> This (D (s ^ s))
  | This (E (f1, f2)) when f1 >= 0.0 && f2 >= 0.0 -> This (E (f2, f1))
  | This (E _) -> Null
  | Null -> Null

let print_int_option_or_null prefix x =
  match x with
  | Null -> Printf.printf "%s: Null\n" prefix
  | This opt ->
      match opt with
      | None -> Printf.printf "%s: This None\n" prefix
      | Some n -> Printf.printf "%s: This (Some %d)\n" prefix n

let[@inline] transform_option_or_null opt =
  match opt with
  | This None -> This (Some 0)
  | This (Some n) -> This (Some (n * 2))
  | Null -> Null

let test_variant_payloads () =
  let var_a = This A in
  let var_b = This B in
  let var_c = This (C 42) in
  let var_d = This (D "hello") in
  let var_e = This (E (3.14, 2.71)) in
  let null_var = Null in

  print_mixed_variant_or_null "Variant Test 1.1, A" var_a;
  print_mixed_variant_or_null "Variant Test 1.2, B" var_b;
  print_mixed_variant_or_null "Variant Test 1.3, C(42)" var_c;
  print_mixed_variant_or_null "Variant Test 1.4, D(hello)" var_d;
  print_mixed_variant_or_null "Variant Test 1.5, E(3.14,2.71)" var_e;
  print_mixed_variant_or_null "Variant Test 1.6, Null variant" null_var;

  print_mixed_variant_or_null "Variant Test 2.1, transform A"
    (transform_variant var_a);
  print_mixed_variant_or_null "Variant Test 2.2, transform C(42)"
    (transform_variant var_c);
  print_mixed_variant_or_null "Variant Test 2.3, transform D(hello)"
    (transform_variant var_d);
  print_mixed_variant_or_null "Variant Test 2.4, transform E(3.14,2.71)"
    (transform_variant var_e);
  print_mixed_variant_or_null "Variant Test 2.5, transform Null"
    (transform_variant null_var);

  print_mixed_variant_or_null "Variant Test 3.1, transform_nonneg B"
    (transform_variant_nonneg var_b);
  print_mixed_variant_or_null "Variant Test 3.2, transform_nonneg C(42)"
    (transform_variant_nonneg var_c);
  print_mixed_variant_or_null "Variant Test 3.3, transform_nonneg C(-1)"
    (transform_variant_nonneg (This (C (-1))));
  print_mixed_variant_or_null "Variant Test 3.4, transform_nonneg D(hello)"
    (transform_variant_nonneg var_d);
  print_mixed_variant_or_null "Variant Test 3.5, transform_nonneg E(3.14,2.71)"
    (transform_variant_nonneg var_e);
  print_mixed_variant_or_null "Variant Test 3.6, transform_nonneg E(-1.0,2.0)"
    (transform_variant_nonneg (This (E (-1.0, 2.0))));

  let some_int = This (Some 42) in
  let none = This None in
  let null_option = Null in

  print_int_option_or_null "Variant Test 4.1, Some 42" some_int;
  print_int_option_or_null "Variant Test 4.2, None" none;
  print_int_option_or_null "Variant Test 4.3, Null option" null_option;

  print_int_option_or_null "Variant Test 5.1, transform Some 42"
    (transform_option_or_null some_int);
  print_int_option_or_null "Variant Test 5.2, transform None"
    (transform_option_or_null none);
  print_int_option_or_null "Variant Test 5.3, transform Null"
    (transform_option_or_null null_option);

  let map_result = map_or_null
    (function
      | A -> C 100
      | B -> D "mapped"
      | C n -> C (n + 1)
      | D s -> D (s ^ "_mapped")
      | E (f1, f2) -> E (f1 *. 2.0, f2 *. 2.0)
    ) var_c in
  print_mixed_variant_or_null "Variant Test 6.1, map C(42)" map_result;

  let and_then_result = and_then
    (function
      | C n when n > 40 -> This (D "transformed")
      | C _ -> This A
      | _ -> Null
    ) (This (C 42)) in
  print_mixed_variant_or_null "Variant Test 6.2, and_then C(42)" and_then_result;

  let opaque_var_a = Sys.opaque_identity var_a in
  let opaque_var_c = Sys.opaque_identity var_c in

  print_mixed_variant_or_null "Variant Test 7.1, opaque A"
    opaque_var_a;
  print_mixed_variant_or_null "Variant Test 7.2, transform opaque A"
    (transform_variant opaque_var_a);
  print_mixed_variant_or_null "Variant Test 7.3, opaque C(42)"
    opaque_var_c;
  print_mixed_variant_or_null "Variant Test 7.4, transform opaque C(42)"
    (transform_variant opaque_var_c);
;;

let () = test_variant_payloads ()
