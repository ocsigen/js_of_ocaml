
(* In javascript, float32s are represented as floats.
   In native code and wasm, float32s are custom blocks containing a float32 field. *)

external float_of_float32 : float32 -> float = "%floatoffloat32"

type float64s = { a : float; b : float }

let%expect_test "float64 javascript" [@tags "js-only", "no-wasm"] =
  let f64 = Marshal.to_string { a = 123.; b = 456. } [] in
  Printf.printf "%S" f64;
  [%expect
    {| "\132\149\166\190\000\000\000\n\000\000\000\001\000\000\000\003\000\000\000\003\b\000\000\b\254\000{\001\001\200" |}];
  let f64 : float64s = Marshal.from_string f64 0 in
  Printf.printf "%f %f" f64.a f64.b;
  [%expect
    {| 123.000000 456.000000 |}]

let%expect_test "float64 wasm" [@tags "wasm-only"] =
  let f64 = Marshal.to_string { a = 123.; b = 456. } [] in
  Printf.printf "%S" f64;
  [%expect
    {| "\132\149\166\190\000\000\000\018\000\000\000\001\000\000\000\005\000\000\000\003\014\002\000\000\000\000\000\192^@\000\000\000\000\000\128|@" |}];
  let f64 : float64s = Marshal.from_string f64 0 in
  Printf.printf "%f %f" f64.a f64.b;
  [%expect
    {| 123.000000 456.000000 |}]

type float32s = { a : float32; b : float32 }

let%expect_test "float32 javascript" [@tags "js-only", "no-wasm"] =
  let f32 = Marshal.to_string { a = 123.s; b = 456.s } [] in
  Printf.printf "%S" f32;
  [%expect
    {| "\132\149\166\190\000\000\000\006\000\000\000\001\000\000\000\003\000\000\000\003\160\000{\001\001\200" |}];
  let f32 : float32s = Marshal.from_string f32 0 in
  Printf.printf "%f %f" (float_of_float32 f32.a) (float_of_float32 f32.b);
  [%expect {| 123.000000 456.000000 |}]

let%expect_test "float32 wasm" [@tags "wasm-only"] =
  let f32 = Marshal.to_string { a = 123.s; b = 456.s } [] in
  Printf.printf "%S" f32;
  [%expect
    {| "\132\149\166\190\000\000\000\021\000\000\000\003\000\000\000\t\000\000\000\t\160\025_f32\000B\246\000\000\025_f32\000C\228\000\000" |}];
  let f32 : float32s = Marshal.from_string f32 0 in
  Printf.printf "%f %f" (float_of_float32 f32.a) (float_of_float32 f32.b);
  [%expect {| 123.000000 456.000000 |}]
