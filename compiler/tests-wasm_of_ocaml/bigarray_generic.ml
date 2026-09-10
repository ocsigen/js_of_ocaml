(* Generic bigarray accesses are specialized when the compiler knows the kind
   and layout of the bigarray and the index array is allocated locally.  This
   checks that the indices are then read at the right offset in the index
   array. *)

open Bigarray

let test name f =
  match f () with
  | true -> Printf.printf "%s: ok\n" name
  | false -> Printf.printf "%s: wrong value\n" name
  | exception e -> Printf.printf "%s: %s\n" name (Printexc.to_string e)

let test1 () =
  let a = Genarray.create int c_layout [| 5 |] in
  for i = 0 to 4 do
    Genarray.set a [| i |] (7 * i)
  done;
  let ok = ref true in
  for i = 0 to 4 do
    if Genarray.get a [| i |] <> 7 * i then ok := false
  done;
  !ok

let test2 () =
  let a = Genarray.create int c_layout [| 4; 5 |] in
  for i = 0 to 3 do
    for j = 0 to 4 do
      Genarray.set a [| i; j |] ((10 * i) + j)
    done
  done;
  let ok = ref true in
  for i = 0 to 3 do
    for j = 0 to 4 do
      if Genarray.get a [| i; j |] <> (10 * i) + j then ok := false
    done
  done;
  !ok

let test3 () =
  let a = Genarray.create int c_layout [| 3; 4; 5 |] in
  for i = 0 to 2 do
    for j = 0 to 3 do
      for k = 0 to 4 do
        Genarray.set a [| i; j; k |] ((100 * i) + (10 * j) + k)
      done
    done
  done;
  let ok = ref true in
  for i = 0 to 2 do
    for j = 0 to 3 do
      for k = 0 to 4 do
        if Genarray.get a [| i; j; k |] <> (100 * i) + (10 * j) + k then ok := false
      done
    done
  done;
  !ok

let test_fortran () =
  let a = Genarray.create float64 fortran_layout [| 4; 5 |] in
  for i = 1 to 4 do
    for j = 1 to 5 do
      Genarray.set a [| i; j |] (float_of_int ((10 * i) + j))
    done
  done;
  let ok = ref true in
  for i = 1 to 4 do
    for j = 1 to 5 do
      if Genarray.get a [| i; j |] <> float_of_int ((10 * i) + j) then ok := false
    done
  done;
  !ok

let () =
  test "1 index" test1;
  test "2 indices" test2;
  test "3 indices" test3;
  test "fortran layout" test_fortran
