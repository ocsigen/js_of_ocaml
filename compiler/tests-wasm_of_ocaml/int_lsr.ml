(* The result of [lsr] is not normalized when the shift amount is 0: a
   negative integer is then returned with its sign bit cleared. *)

let f x n s = x lsr n < String.length s

let g x n = x lsr n < 0

let h x n = x lsr n

let () =
  let x = int_of_string "-1" in
  let zero = int_of_string "0" in
  let one = int_of_string "1" in
  assert (f x zero "abc");
  assert (g x zero);
  assert (h x zero = x);
  assert (not (f x one "abc"));
  assert (not (g x one));
  assert (h x one = max_int);
  (* A constant shift amount *)
  assert (not (x lsr 1 < String.length "abc"));
  assert (x lsr 1 = max_int)
