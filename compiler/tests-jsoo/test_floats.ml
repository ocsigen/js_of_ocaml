(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2019 Hugo Heuzard
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

let%expect_test _ =
  (* copied from https://github.com/ocaml/ocaml/pull/1794 *)
  let z =
    let x = -0. and y = 0. in
    if mod_float x 1. >= 0. then x else if false then x else y
  in
  Printf.printf "%g\n" (1. /. z);
  [%expect {|-inf|}]
;;

module Float = struct
  include Float

  external acosh : float -> float = "caml_acosh_float"

  external asinh : float -> float = "caml_asinh_float"

  external atanh : float -> float = "caml_atanh_float"

  external erf : float -> float = "caml_erf_float"

  external erfc : float -> float = "caml_erfc_float"

  external cbrt : float -> float = "caml_cbrt_float"

  external exp2 : float -> float = "caml_exp2_float"

  external log2 : float -> float = "caml_log2_float"
end

let%expect_test "acosh" =
  let p x = Printf.printf "%f\n" (Float.acosh x) in
  p (-1.0);
  [%expect {| nan |}];
  p 0.0;
  [%expect {| nan |}];
  p 0.5;
  [%expect {| nan |}];
  p 1.0;
  [%expect {| 0.000000 |}];
  p 2.0;
  [%expect {| 1.316958 |}]
;;

let%expect_test "asinh" =
  let p x = Printf.printf "%f\n" (Float.asinh x) in
  p 1.0;
  [%expect {| 0.881374 |}];
  p 0.0;
  [%expect {| 0.000000 |}];
  p (-1.0);
  [%expect {| -0.881374 |}];
  p 2.0;
  [%expect {| 1.443635 |}]
;;

let%expect_test "atanh" =
  let p x = Printf.printf "%f\n" (Float.atanh x) in
  p (-2.0);
  [%expect {| nan |}];
  p (-1.0);
  [%expect {| -inf |}];
  p 0.0;
  [%expect {| 0.000000 |}];
  p 0.5;
  [%expect {| 0.549306 |}];
  p 1.0;
  [%expect {| inf |}];
;;

(* TODO: These results don't seem to be correct *)
let%expect_test "erf" =
  let p x = Printf.printf "%f\n" (Float.erf x) in
  p (-2.0);
  [%expect {| -0.762913 |}];
  p (-1.0);
  [%expect {| -0.940296 |}];
  p 0.0;
  [%expect {| 1.000000 |}];
  p 0.5;
  [%expect {| 1.163111 |}];
  p 1.0;
  [%expect {| 0.940296 |}];
  p 10.0;
  [%expect {| 0.941387 |}];
;;

(* TODO: These results don't seem to be correct *)
let%expect_test "erfc" =
  let p x = Printf.printf "%f\n" (Float.erfc x) in
  p (-2.0);
  [%expect {| 1.762913 |}];
  p (-1.0);
  [%expect {| 1.940296 |}];
  p 0.0;
  [%expect {| -0.000000 |}];
  p 0.5;
  [%expect {| -0.163111 |}];
  p 1.0;
  [%expect {| 0.059704 |}];
  p 10.0;
  [%expect {| 0.058613 |}];
;;

let%expect_test "cbrt" =
  let p x = Printf.printf "%f\n" (Float.cbrt x) in
  p Float.nan;
  [%expect {| nan |}];
  p (-1.0);
  [%expect {| -1.000000 |}];
  p (-0.0);
  [%expect {| -0.000000 |}];
  p Float.neg_infinity;
  [%expect {| -inf |}];
  p 0.0;
  [%expect {| 0.000000 |}];
  p 1.0;
  [%expect {| 1.000000 |}];
  p Float.infinity;
  [%expect {| inf |}];
  p 2.0;
  [%expect {| 1.259921 |}]
;;

let%expect_test "exp2" =
  let p x = Printf.printf "%f\n" (Float.exp2 x) in
  p Float.nan;
  [%expect {| nan |}];
  p (-1.0);
  [%expect {| 0.500000 |}];
  p (-0.0);
  [%expect {| 1.000000 |}];
  p Float.neg_infinity;
  [%expect {| 0.000000 |}];
  p 0.0;
  [%expect {| 1.000000 |}];
  p 1.0;
  [%expect {| 2.000000 |}];
  p Float.infinity;
  [%expect {| inf |}];
  p 2.0;
  [%expect {| 4.000000 |}]
;;

let%expect_test "log2" =
  let p x = Printf.printf "%f\n" (Float.log2 x) in
  p 3.0;
  [%expect {| 1.584963 |}];
  p 2.0;
  [%expect {| 1.000000 |}];
  p 1.0;
  [%expect {| 0.000000 |}];
  p 0.0;
  [%expect {| -inf |}];
  p (-2.0);
  [%expect {| nan |}];
  p 1024.0;
  [%expect {| 10.000000 |}];
;;
