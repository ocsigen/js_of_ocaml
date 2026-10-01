(* Generic array accesses are specialized when the type analysis knows
   whether the array is a float array or a block. Empty arrays are
   always represented by the same block, even when they are float
   arrays. *)

let test name f =
  match f () with
  | true -> Printf.printf "%s: ok\n" name
  | false -> Printf.printf "%s: wrong value\n" name
  | exception e -> Printf.printf "%s: %s\n" name (Printexc.to_string e)

let[@inline never] opaque x = x

(* Only called with float arrays *)
let[@inline never] float_sum (a : 'a array) (f : 'a -> float) =
  let s = ref 0. in
  for i = 0 to Array.length a - 1 do
    s := !s +. f a.(i)
  done;
  !s

let[@inline never] float_fill (a : 'a array) x =
  for i = 0 to Array.length a - 1 do
    a.(i) <- x
  done

let[@inline never] float_get (a : 'a array) i = a.(i)

(* Only called with blocks *)
let[@inline never] value_concat (a : 'a array) (f : 'a -> string) =
  let s = ref "" in
  for i = 0 to Array.length a - 1 do
    s := !s ^ f a.(i)
  done;
  !s

let[@inline never] value_fill (a : 'a array) x =
  for i = 0 to Array.length a - 1 do
    a.(i) <- x
  done

let[@inline never] value_get (a : 'a array) i = a.(i)

(* Called with both *)
let[@inline never] length (a : 'a array) = Array.length a

let float_arrays () =
  let n = opaque 4 in
  let a = Array.make n 1.5 in
  let e = if n = 0 then a else [||] in
  let lit = [| 1.; 2.; 3. |] in
  float_fill a 2.5;
  float_fill e 1.;
  float_sum a Fun.id = 10.
  && float_sum e Fun.id = 0.
  && float_sum lit Fun.id = 6.
  && float_get a 3 = 2.5
  && length a = 4
  && length e = 0

let value_arrays () =
  let n = opaque 3 in
  let a = Array.make n "a" in
  let e = if n = 0 then a else [||] in
  let lit = [| "x"; "y" |] in
  value_fill a "b";
  value_fill e "c";
  value_concat a Fun.id = "bbb"
  && value_concat e Fun.id = ""
  && value_concat lit Fun.id = "xy"
  && value_get a 2 = "b"
  && length a = 3
  && length e = 0

let sub_append () =
  let n = opaque 3 in
  let f = Array.make n 1. in
  let f' = Array.append [||] (Array.sub f 1 2) in
  let f'' = Array.append (Array.sub f 0 0) f in
  let v = Array.make n "a" in
  let v' = Array.append v (Array.sub v 0 1) in
  float_sum f' Fun.id = 2.
  && float_sum f'' Fun.id = 3.
  && value_concat v' Fun.id = "aaaa"
  && length f' = 2
  && length v' = 4

let expect_invalid_argument f =
  match f () with
  | _ -> false
  | exception Invalid_argument _ -> true

let bounds () =
  let n = opaque 2 in
  let f = Array.make n 1. in
  let fe = if n = 0 then f else [||] in
  let v = Array.make n "a" in
  let ve = if n = 0 then v else [||] in
  expect_invalid_argument (fun () -> float_get f 2)
  && expect_invalid_argument (fun () -> float_get f (-1))
  && expect_invalid_argument (fun () -> float_get fe 0)
  && expect_invalid_argument (fun () -> value_get v 2)
  && expect_invalid_argument (fun () -> value_get v (-1))
  && expect_invalid_argument (fun () -> value_get ve 0)

let () =
  test "float arrays" float_arrays;
  test "value arrays" value_arrays;
  test "sub and append" sub_append;
  test "bounds" bounds
