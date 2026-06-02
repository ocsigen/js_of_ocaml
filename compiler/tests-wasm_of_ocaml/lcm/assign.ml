(* Local references mutated inside a [try] body and read in the handler.
   When compiled without [-g], ocamlc turns them into mutable variables and
   the bytecode parser produces [Assign] instructions for them. The LCM pass
   must not reuse a conversion computed before such an assignment. *)

let k = Sys.opaque_identity 5

let consume_f (x : float) = ignore (Sys.opaque_identity x)

let f1 n =
  let r = ref 1.5 in
  (try
     for i = 1 to n do
       r := !r +. float_of_int i;
       if i = k then raise Exit
     done
   with Exit -> r := !r *. 2.);
  !r

let f2 n =
  let r = ref (float_of_int n) in
  consume_f !r;
  let s = ref 0. in
  (try
     s := !r +. 1.;
     r := 100.;
     if n > 0 then raise Exit;
     r := 7.
   with Exit -> s := !s +. !r);
  !s +. !r

let f3 n =
  let acc = ref 0. in
  let r = ref 0.25 in
  for i = 1 to n do
    (try
       r := !r +. 1.;
       if i mod 2 = 0 then failwith "x";
       r := !r *. 3.
     with Failure _ -> acc := !acc +. !r);
    acc := !acc +. !r
  done;
  !acc

let f4 n =
  let a = ref 3l and b = ref 4L and c = ref 5 in
  (try
     for i = 1 to n do
       a := Int32.add !a (Int32.of_int i);
       b := Int64.mul !b 3L;
       c := !c + i;
       if i = k then raise Not_found
     done
   with Not_found ->
     a := Int32.mul !a 2l;
     b := Int64.add !b !b;
     c := !c * 7);
  Printf.sprintf "%ld %Ld %d" !a !b !c

let f5 n =
  let r = ref 2. in
  let s = ref 3. in
  (try
     (try
        r := !r +. 10.;
        s := !s +. 1.;
        if n > 3 then raise Exit
      with Exit ->
        s := !s +. !r;
        r := !r +. 100.;
        if n > 4 then raise Not_found);
     r := 0.
   with Not_found -> s := !s *. !r);
  !r +. !s

let f6 n =
  let r = ref 1.0 in
  let l = ref [] in
  for i = 1 to n do
    try
      l := !r :: !l;
      r := !r +. float_of_int i;
      if i mod 3 = 0 then raise Exit
    with Exit -> l := (!r *. 10.) :: !l
  done;
  List.fold_left ( +. ) 0. !l

let () =
  assert (Float.equal (f1 10) 33.);
  assert (Float.equal (f1 3) 7.5);
  assert (Float.equal (f2 3) 204.);
  assert (Float.equal (f2 0) 8.);
  assert (Float.equal (f3 7) 421.5);
  assert (Float.equal (f3 1) 3.75);
  assert (String.equal (f4 10) "36 1944 140");
  assert (Float.equal (f5 5) 1904.);
  assert (Float.equal (f5 4) 16.);
  assert (Float.equal (f6 8) 382.);
  assert (Float.equal (f6 2) 3.)
