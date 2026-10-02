(* The result of [land] is not normalized when the normalized operand
   is negative: [x * 223] overflows 31 bits, and masking it with [-1]
   leaves the overflowing bits in place. *)

let f x mask = x * 223 land mask > 0x3FFFFFFF

let g x mask = x * 223 land mask

let () =
  let x = int_of_string "6089576" in
  let mask = int_of_string "-1" in
  assert (not (f x mask));
  assert (g x mask = int_of_string "-789508200")
