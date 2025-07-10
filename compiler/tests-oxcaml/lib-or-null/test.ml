(* TEST *)

open Stdlib_stable

let assert_raise_invalid_argument f v =
  assert (
    try
      ignore (f v);
      false
    with Invalid_argument _ -> true);
  ()

let test_null_this () =
  assert (Or_null.null = Null);
  assert (Or_null.this 2 = This 2);
  ()

let test_value () =
  assert (Or_null.value Null ~default:5 = 5);
  assert (Or_null.value (This 3) ~default:5 = 3);
  ()

let test_get () =
  assert_raise_invalid_argument Or_null.get Null;
  assert (Or_null.get (This 2) = 2);
  ()

let test_bind () =
  assert (Or_null.bind (This 3) (fun x -> This (succ x)) = This 4);
  assert (Or_null.bind (This 3) (fun _ -> Null) = Null);
  assert (Or_null.bind Null (fun x -> This (succ x)) = Null);
  assert (Or_null.bind Null (fun _ -> Null) = Null);
  ()

let test_map () =
  assert (Or_null.map succ (This 3) = This 4);
  assert (Or_null.map succ Null = Null);
  ()

let test_fold () =
  assert (Or_null.fold ~null:3 ~this:succ (This 1) = 2);
  assert (Or_null.fold ~null:3 ~this:succ Null = 3);
  (*
  assert (Or_null.(fold ~null ~this) (This 1) = (This 1));
  assert (Or_null.(fold ~null ~this) Null = Null);
*)
  ()

let test_iter () =
  let count = ref 0 in
  let set_count x = count := x in
  assert (!count = 0);
  Or_null.iter set_count (This 2);
  assert (!count = 2);
  Or_null.iter set_count Null;
  assert (!count = 2);
  ()

let test_is_null_this () =
  assert (Or_null.is_null Null = true);
  assert (Or_null.is_this Null = false);
  assert (Or_null.is_null (This 2) = false);
  assert (Or_null.is_this (This 2) = true);
  ()

let test_equal () =
  let eq v0 v1 = v0 mod 2 = v1 mod 2 in
  let equal = Or_null.equal eq in
  assert (not @@ equal (This 2) (This 3));
  assert (equal (This 2) (This 4));
  assert (not @@ equal (This 2) Null);
  assert (not @@ equal Null (This 3));
  assert (not @@ equal Null (This 4));
  assert (equal Null Null);
  ()

let test_compare () =
  let compare v0 v1 = -compare v0 v1 in
  let compare = Or_null.compare compare in
  assert (compare (This 2) (This 1) = -1);
  assert (compare (This 2) (This 2) = 0);
  assert (compare (This 2) (This 3) = 1);
  assert (compare (This 2) Null = 1);
  assert (compare Null (This 1) = -1);
  assert (compare Null (This 2) = -1);
  assert (compare Null (This 3) = -1);
  assert (compare Null Null = 0);
  ()

let test_to_option_list_seq () =
  assert (Or_null.to_result ~null:6 (This 3) = Ok 3);
  assert (Or_null.to_result ~null:6 Null = Error 6);
  assert (Or_null.to_list (This 3) = [ 3 ]);
  assert (Or_null.to_list Null = []);
  (match (Or_null.to_seq (This 3)) () with
  | Seq.Cons (3, f) -> assert (f () = Seq.Nil)
  | _ -> assert false);
  assert ((Or_null.to_seq Null) () = Seq.Nil);
  ()

let tests () =
  test_null_this ();
  test_value ();
  test_get ();
  test_bind ();
  test_map ();
  test_fold ();
  test_iter ();
  test_is_null_this ();
  test_equal ();
  test_compare ();
  test_to_option_list_seq ();
  ()

let () =
  tests ();
  print_endline "OK"
