(* TEST
 flags = "-extension-universe beta";
*)

type int_or_null : value_or_null mod external_

external mk : int or_null -> int_or_null = "%identity"

external compare_int_or_null : int_or_null -> int_or_null -> int = "%compare"

let compare x y = compare_int_or_null (mk x) (mk y)

let () =
  assert (compare Null Null = 0);
  assert (compare (This 4) (This 4) = 0);
  assert (compare Null (This 4) < 0);
  assert (compare (This 8) Null > 0);
  assert (compare (This 4) (This 5) < 0);
;;
