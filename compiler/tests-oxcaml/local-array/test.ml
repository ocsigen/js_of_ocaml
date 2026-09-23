(* TEST *)

(* Local array allocation.  js_of_ocaml and wasm_of_ocaml have no stack
   allocation, so each [_local] primitive builds an ordinary array; what the
   tests check is that it builds the same array its global counterpart would.
   Nothing in the stdlib calls these, so they are declared here. *)

module Float_u = Stdlib_upstream_compatible.Float_u

external uniform_sub_local : 'a array -> int -> int -> 'a array @ local
  = "caml_uniform_array_sub_local"

external uniform_append_local : 'a array -> 'a array -> 'a array @ local
  = "caml_uniform_array_append_local"

external uniform_concat_local : 'a array list -> 'a array @ local
  = "caml_uniform_array_concat_local"

external uniform_make_local : int -> 'a -> 'a array @ local
  = "caml_uniform_array_make_local"

external float_sub_local : floatarray -> int -> int -> floatarray @ local
  = "caml_floatarray_sub_local"

external float_append_local : floatarray -> floatarray -> floatarray @ local
  = "caml_floatarray_append_local"

external float_concat_local : floatarray list -> floatarray @ local
  = "caml_floatarray_concat_local"

external float_make_local : int -> float -> floatarray @ local
  = "caml_floatarray_make_local"

(* An unboxed argument needs a native name too, though only the bytecode one
   is ever used here. *)
external float_make_unboxed_local : int -> float# -> floatarray @ local
  = "caml_floatarray_make_unboxed_local" "caml_floatarray_make_unboxed_local"

let floatarray_of_list l =
  let a = Array.Floatarray.create (List.length l) in
  List.iteri (fun i x -> Array.Floatarray.set a i x) l;
  a

let test_uniform_sub () =
  let a = [| 1; 2; 3; 4; 5 |] in
  let s = uniform_sub_local a 1 3 in
  assert (Array.length s = 3);
  assert (s.(0) = 2 && s.(1) = 3 && s.(2) = 4);
  (* The result is a copy: writing to it leaves the source alone. *)
  s.(0) <- 99;
  assert (a.(1) = 2);
  let empty = uniform_sub_local a 2 0 in
  assert (Array.length empty = 0)

let test_uniform_append () =
  let r = uniform_append_local [| 1; 2 |] [| 3 |] in
  assert (Array.length r = 3);
  assert (r.(0) = 1 && r.(1) = 2 && r.(2) = 3);
  let r = uniform_append_local [||] [| 7 |] in
  assert (Array.length r = 1 && r.(0) = 7);
  let r = uniform_append_local [| 7 |] [||] in
  assert (Array.length r = 1 && r.(0) = 7)

let test_uniform_concat () =
  let r = uniform_concat_local [ [| 1 |]; [||]; [| 2; 3 |] ] in
  assert (Array.length r = 3);
  assert (r.(0) = 1 && r.(1) = 2 && r.(2) = 3);
  let r = uniform_concat_local ([] : int array list) in
  assert (Array.length r = 0)

let test_uniform_make () =
  let r = uniform_make_local 3 "x" in
  assert (Array.length r = 3);
  assert (r.(0) = "x" && r.(2) = "x");
  let r = uniform_make_local 0 "x" in
  assert (Array.length r = 0)

let get = Array.Floatarray.get

let len = Array.Floatarray.length

let test_float_sub () =
  let a = floatarray_of_list [ 1.; 2.; 3.; 4. ] in
  let s = float_sub_local a 1 2 in
  assert (len s = 2);
  assert (get s 0 = 2.);
  assert (get s 1 = 3.);
  Array.Floatarray.set s 0 99.;
  assert (get a 1 = 2.)

let test_float_append () =
  let a = floatarray_of_list [ 1.; 2. ] in
  let b = floatarray_of_list [ 3. ] in
  let r = float_append_local a b in
  assert (len r = 3);
  assert (get r 0 = 1.);
  assert (get r 1 = 2.);
  assert (get r 2 = 3.)

let test_float_concat () =
  let a = floatarray_of_list [ 1.; 2. ] in
  let b = floatarray_of_list [ 3. ] in
  let r = float_concat_local [ a; b; a ] in
  assert (len r = 5);
  assert (get r 2 = 3.);
  assert (get r 4 = 2.)

let test_float_make () =
  let r = float_make_local 3 1.5 in
  assert (len r = 3);
  assert (get r 0 = 1.5);
  assert (get r 2 = 1.5);
  assert (len (float_make_local 0 1.5) = 0);
  let r = float_make_unboxed_local 2 (Float_u.of_float 2.5) in
  assert (len r = 2);
  assert (get r 0 = 2.5);
  assert (get r 1 = 2.5)

let () =
  test_uniform_sub ();
  test_uniform_append ();
  test_uniform_concat ();
  test_uniform_make ();
  test_float_sub ();
  test_float_append ();
  test_float_concat ();
  test_float_make ();
  print_endline "OK"
