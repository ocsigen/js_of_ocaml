(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2019 Ty Overby
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

open Js_of_ocaml_compiler.Stdlib

let%expect_test _ =
  let f = Unix.time () in
  let z, _integral = modf f in
  match Float.classify_float f, Float.classify_float z with
  | FP_normal, FP_zero -> ()
  | _ -> assert false

let%expect_test _ =
  let f = Unix.gettimeofday () in
  match Float.classify_float f with
  | FP_normal -> ()
  | _ -> assert false

(* Attempt to check that the result of gmtime has the right shape *)
let%expect_test _ =
  let open Unix in
  let { tm_sec
      ; tm_min
      ; tm_hour
      ; tm_mday
      ; tm_mon
      ; tm_year
      ; tm_wday
      ; tm_yday
      ; tm_isdst = _
      } =
    gmtime (time ())
  in
  let check_int s =
    assert (
      String.length s > 0
      && String.for_all
           ~f:(function
             | '0' .. '9' | ' ' -> true
             | _ -> false)
           s)
  in
  print_int tm_sec;
  check_int [%expect.output];
  print_int tm_min;
  check_int [%expect.output];
  print_int tm_hour;
  check_int [%expect.output];
  print_int tm_mday;
  check_int [%expect.output];
  print_int tm_mon;
  check_int [%expect.output];
  print_int tm_year;
  check_int [%expect.output];
  print_int tm_wday;
  check_int [%expect.output];
  print_int tm_yday;
  check_int [%expect.output];
  [%expect {|  |}]

let now = 1377134255.469

(* check gmtime *)
let tmg = Unix.gmtime now

let failure_tm name tmg =
  let open Unix in
  Printf.printf
    "%s: s:%d; m:%d; h:%d; D:%d; M:%d; Y:%d; WD:%d; YD:%d; dst:%b"
    name
    tmg.tm_sec
    tmg.tm_min
    tmg.tm_hour
    tmg.tm_mday
    tmg.tm_mon
    tmg.tm_year
    tmg.tm_wday
    tmg.tm_yday
    tmg.tm_isdst

let%expect_test _ =
  (match tmg with
  | { Unix.tm_sec = 35
    ; Unix.tm_min = 17
    ; Unix.tm_hour = 1
    ; Unix.tm_mday = 22
    ; Unix.tm_mon = 7
    ; Unix.tm_year = 113
    ; Unix.tm_wday = 4
    ; Unix.tm_yday = 233
    ; Unix.tm_isdst = false
    } -> ()
  | _ -> failure_tm "Unix.gmtime" tmg);
  [%expect {||}]
