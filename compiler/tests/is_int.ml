open Common

let log_stop = log_start "Is_int test suite"

let r = ref false

let f x =
  match Obj.is_int x with
  | true ->
      r := true;
      true
  | false ->
      r := false;
      false

let () =
  if not (f (Obj.repr 1)) then log_failure "[is_int 1] should be true";
  if f (Obj.repr (1, 2, 3)) then log_failure "[is_int (1,2,3)] should be false"

let _ = log_stop ()
