(* TEST *)

let try_float_of_string str =
  try
    print_float (float_of_string str);
    print_newline ()
  with exn ->
    print_endline (Printexc.to_string exn)
;;

let () =
  try_float_of_string "0x1A";
  try_float_of_string "0x1Ap3";
  try_float_of_string "0x";
  try_float_of_string "0x.";
  try_float_of_string "0xp0";
  try_float_of_string "0x.p0";

  (* MPR#7690 *)
  try_float_of_string "0x1.0p-2147483648";
  try_float_of_string "0x123456789ABCDEF0p2147483647";
  try_float_of_string "0x1p2147483648";

  (* Allow underscore almost everywhere *)
  try_float_of_string "_0x1.1";
  try_float_of_string "0_x1.1";
  try_float_of_string "0x_1.1";
  try_float_of_string "0x1_.1";
  try_float_of_string "0x1._";
  try_float_of_string "0x1.1_";
  try_float_of_string "0x1_p1";
  try_float_of_string "0x1p_1";
  try_float_of_string "0x1p1_";
  try_float_of_string "0x1p-1_1";
  try_float_of_string "0x1p-1_";
  try_float_of_string "0x1p+1_1";
  try_float_of_string "0x1p+1_";

  try_float_of_string "0x1p1\000suffix"

let () =
  (* Subnormals are rounded only once *)
  let check str bits =
    assert (Int64.bits_of_float (float_of_string str) = bits)
  in
  check "0x1p-1075" 0x0L;
  check "0x3p-1075" 0x2L;
  check "0x1.0000000000001p-1075" 0x1L;
  check "0x20000000000001p-1128" 0x1L;
  check "0x5FFFFFFFFFFFFFp-1128" 0x1L;
  check "0x1.7ffffffffffffcp-1074" 0x1L;
  check "0x1.00000000000000000001p-1075" 0x1L;
  check "-0x20000000000001p-1128" 0x8000_0000_0000_0001L;
  check "0x1.00000000000014p-1023" 0x8_0000_0000_0001L;
  check "0x1.00000000000013ffp-1023" 0x8_0000_0000_0001L;
  check "0x1.ffffffffffffe8p-1023" 0xf_ffff_ffff_ffffL;
  check "0x1.fffffffffffff8p-1023" 0x10_0000_0000_0000L

let () =
  (* check that the compiler can also parse tokens *)
  let _ = 0x1A in
  let _ = 0x1Ap3 in

  let _ = 0x1.0p-2147483648 in
  let _ = 0x123456789ABCDEF0p2147483647 in
  let _ = 0x1p2147483648 in

  let _ = 0x1_._1p1_1 in
  let _ = 0x1_._1p1_ in
  let _ = 0x1_._1p-1_1 in
  let _ = 0x1_._1p-1_ in
  let _ = 0x1_._1p+1_1 in
  let _ = 0x1_._1p+1_ in
  ()
