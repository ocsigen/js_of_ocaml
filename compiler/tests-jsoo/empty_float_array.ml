let empty = [||]

let check_empty_array (a : float array) =
  let a = Sys.opaque_identity a in
  assert (Array.length a = 0);
  assert (Obj.tag (Obj.repr a) = 0);
  assert (a = empty)

let%expect_test "empty array constant at float array type" = check_empty_array empty

let%expect_test "empty float array literal" = check_empty_array [||]

let%expect_test "recursive empty float array" =
  let rec a : float array =
    let _ = a in
    [||]
  in
  check_empty_array a

let%expect_test "empty Array.concat" =
  check_empty_array (Array.concat []);
  check_empty_array (Array.concat [ empty; empty ])

let%expect_test "empty Float.Array.concat" =
  let empty = Float.Array.create 0 in
  let check a =
    assert (Float.Array.length a = 0);
    assert (a = empty);
    assert (Obj.tag (Obj.repr a) = 0)
  in
  check (Float.Array.concat []);
  check (Float.Array.concat [ empty; empty ])
