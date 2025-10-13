let none = Int64.float_of_bits 0x7ff0_1234_5678_90ABL

let sign_mask = 0x8000_0000_0000_0000L

let some x =
  if Float.is_nan x
  then if Int64.(logand (bits_of_float x) sign_mask = 0L) then nan else -.nan
  else x

let is_none t = Int64.equal (Int64.bits_of_float t) (Int64.bits_of_float none)

let () =
  assert (is_none none);
  let l = [ nan; -.nan; 1.; -7.; infinity; neg_infinity; 0.; none ] in
  List.iter (fun f -> assert (not (is_none (some f)))) l
