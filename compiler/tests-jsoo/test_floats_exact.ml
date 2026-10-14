(* Formatting with precisions above 100 (toFixed/toExponential's limit) and
   %f of values >= 1e21 must print the exact decimal expansion, like glibc.
   Native is the oracle, but mingw's printf rounds past ~17 significant
   digits, so this test is disabled on Windows; the js and wasm runtimes are
   OS-independent and still exercised everywhere else. *)

[@@@if ocaml_version >= (4, 14, 0) && os_type <> "Win32"]

let%expect_test "format with large precision" =
  let p (fmt : _ format) x =
    match Printf.sprintf fmt x with
    | s -> print_endline s
    | exception Failure _ -> print_endline "failure"
  in
  p "%.150e" 0.1;
  p "%.110f" 0.1;
  p "%.0f" 1.2345678e22;
  p "%.2f" 1.2345678e22;
  p "%.150e" 2.;
  p "%.110e" 0.;
  p "%.105g" 0.1;
  (* 1e153 has 18 leading nines, the most of any double *)
  p "%.120e" 1e153;
  p "%.110g" 1e153;
  [%expect
    {|
    1.000000000000000055511151231257827021181583404541015625000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000e-01
    0.10000000000000000555111512312578270211815834045410156250000000000000000000000000000000000000000000000000000000
    12345678000000000327680
    12345678000000000327680.00
    2.000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000e+00
    0.00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000e+00
    0.1000000000000000055511151231257827021181583404541015625
    9.999999999999999997334030041231537448555390191184366862858401880243696795224237616729197595645671584436693788240287100204e+152
    9.9999999999999999973340300412315374485553901911843668628584018802436967952242376167291975956456715844366937882e+152
    |}]

let p (fmt : _ format) x = print_endline (Printf.sprintf fmt x)

(* C keeps fixed notation while the exponent is below the precision; the js
   and wasm runtimes switch to exponential notation at 1e21, where toFixed
   does. *)
let%expect_test "%g of values >= 1e21" =
  p "%.25g" 1e21;
  p "%.30g" 1.2345678e22;
  [%expect ({|
    1e+21
    1.234567800000000032768e+22
    |} [@when not native])];
  [%expect {|
    1000000000000000000000
    12345678000000000327680
    |}]

(* C rounds exact ties to even; the js and wasm runtimes round them away from
   zero, like toFixed and toExponential. *)
let%expect_test "exact ties" =
  p "%.17g" 90.0351715087890625;
  p "%.2f" 0.125;
  p "%.0f" 2.5;
  p "%.0f" 3.5;
  p "%.0e" 25.;
  p "%.2e" 0.5625;
  p "%.3g" 0.5625;
  p "%.10g" 0.65673828125;
  [%expect
    ({|
    90.035171508789063
    0.13
    3
    4
    3e+01
    5.63e-01
    0.563
    0.6567382813
    |}
    [@when not native])];
  [%expect
    {|
    90.035171508789062
    0.12
    2
    4
    2e+01
    5.62e-01
    0.562
    0.6567382812
    |}]
