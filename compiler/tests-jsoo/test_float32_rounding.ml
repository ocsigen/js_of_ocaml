(* Js_of_ocaml
 * http://www.ocsigen.org/js_of_ocaml/
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

(* Float32 results computed in double precision must still be rounded only
   once: rounding to a double can land exactly halfway between two float32
   values, and rounding to a float32 then picks the even one. *)

external fma : float32 -> float32 -> float32 -> float32
  = "caml_fma_float32_bytecode" "fmaf"
[@@unboxed] [@@noalloc]

external of_int64 : int64 -> float32
  = "caml_float32_of_int64_bytecode" "caml_float32_of_int64"
[@@unboxed] [@@noalloc] [@@builtin]

external of_bits : int32 -> float32
  = "caml_float32_of_bits_bytecode" "caml_float32_of_bits"
[@@unboxed] [@@noalloc] [@@builtin]

external to_bits : float32 -> int32
  = "caml_float32_to_bits_bytecode" "caml_float32_to_bits"
[@@unboxed] [@@noalloc] [@@builtin]

external of_string : string -> float32 = "caml_float32_of_string"

external modf : float32 -> float32 * float32 = "caml_modf_float32"

let%expect_test "of_string" =
  List.iter
    (fun s -> Printf.printf "%S %08lx\n" s (to_bits (of_string s)))
    [ (* the midpoint between 1+2^-23 and 1+2^-22 is
         1.000000178813934326171875 *)
      "1.000000178813934326171874"
    ; "1.000000178813934326171875"
    ; "1.000000178813934326171876"
    ; "-1.000000178813934326171874"
    ; "1.000_000_178_813_934_326_171_874"
    ; " \t+1.000000178813934326171874"
    ; "0x1.000002fffffffffffffffffp0"
    ; "0x1.0000030000000000000001p0"
    ; (* the midpoint between the largest float32 and 2^128 is
         2^128 - 2^103 = 340282356779733661637539395458142568448 *)
      "340282356779733661637539395458142568447"
    ; "340282356779733661637539395458142568448"
    ; "340282356779733661637539395458142568449"
    ; (* the midpoint between 0 and the smallest subnormal is 2^-150 =
         7.00649232162408535461864791644958065640130970938257885878534141944895541342930300743319094181060791015625e-46 *)
      "7.00649232162408535461864791644958065640130970938257885878534141944895541342930300743319094181060791015624e-46"
    ; "7.00649232162408535461864791644958065640130970938257885878534141944895541342930300743319094181060791015625e-46"
    ; "7.00649232162408535461864791644958065640130970938257885878534141944895541342930300743319094181060791015626e-46"
    ; "-7.00649232162408535461864791644958065640130970938257885878534141944895541342930300743319094181060791015624e-46"
    ; "-7.00649232162408535461864791644958065640130970938257885878534141944895541342930300743319094181060791015626e-46"
    ; "-0x1p-150"
      (* Not "-0x1.00000000000000001p-150" (-0x1p-149): OxCaml's native
         hexadecimal float32 parser rounds subnormals twice
         (oxcaml/oxcaml#7934) *)
    ];
  [%expect
    {|
    "1.000000178813934326171874" 3f800001
    "1.000000178813934326171875" 3f800002
    "1.000000178813934326171876" 3f800002
    "-1.000000178813934326171874" bf800001
    "1.000_000_178_813_934_326_171_874" 3f800001
    " \t+1.000000178813934326171874" 3f800001
    "0x1.000002fffffffffffffffffp0" 3f800001
    "0x1.0000030000000000000001p0" 3f800002
    "340282356779733661637539395458142568447" 7f7fffff
    "340282356779733661637539395458142568448" 7f800000
    "340282356779733661637539395458142568449" 7f800000
    "7.00649232162408535461864791644958065640130970938257885878534141944895541342930300743319094181060791015624e-46" 00000000
    "7.00649232162408535461864791644958065640130970938257885878534141944895541342930300743319094181060791015625e-46" 00000000
    "7.00649232162408535461864791644958065640130970938257885878534141944895541342930300743319094181060791015626e-46" 00000001
    "-7.00649232162408535461864791644958065640130970938257885878534141944895541342930300743319094181060791015624e-46" 80000000
    "-7.00649232162408535461864791644958065640130970938257885878534141944895541342930300743319094181060791015626e-46" 80000001
    "-0x1p-150" 80000000
    |}]

let%expect_test "fma" =
  (* a = 1 + 2^-12 *)
  let a = of_bits 0x3f800800l in
  List.iter
    (fun z -> Printf.printf "%08lx\n" (to_bits (fma a a (of_bits z))))
    [ 0x17800000l (* 2^-80 *); 0x97800000l (* -2^-80 *); 0l ];
  (* The double nearest to 1 - 2^-80 is 1, whose significand has its low 32
     bits all zero: rounding to odd goes one ulp down across them *)
  let one = of_bits 0x3f800000l in
  Printf.printf "%08lx\n" (to_bits (fma one one (of_bits 0x97800000l)));
  [%expect {|
    3f801001
    3f801000
    3f801000
    3f800000
    |}]

let%expect_test "modf" =
  (* Not a rounding test: guards against the argument being lost when the
     primitives around it are unboxed *)
  List.iter
    (fun x ->
      let f, i = modf (of_bits x) in
      Printf.printf "%08lx %08lx\n" (to_bits f) (to_bits i))
    [ 0x40700000l (* 3.75 *); 0xc0200000l (* -2.5 *) ];
  [%expect {|
    3f400000 40400000
    bf000000 c0000000
    |}]

let%expect_test "of_int64" =
  List.iter
    (fun n -> Printf.printf "%Ld %08lx\n" n (to_bits (of_int64 n)))
    [ 0x1000001000000001L (* 2^60 + 2^36 + 1 *)
    ; 0x1000001000000000L (* 2^60 + 2^36: a tie *)
    ; 0x0fffffffffffffffL
    ; -0x1000001000000001L
    ; Int64.max_int
    ; Int64.min_int
    ; 0x20000000000001L (* 2^53 + 1 *)
    ; (* the nearest double is 2^62, rounded to odd one ulp down *)
      0x3fffffffffffffffL
    ; -0x3fffffffffffffffL
    ];
  [%expect
    {|
    1152921573326323713 5d800001
    1152921573326323712 5d800000
    1152921504606846975 5d800000
    -1152921573326323713 dd800001
    9223372036854775807 5f000000
    -9223372036854775808 df000000
    9007199254740993 5a000000
    4611686018427387903 5e800000
    -4611686018427387903 de800000
    |}]
