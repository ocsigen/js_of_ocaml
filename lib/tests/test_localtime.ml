(* Js_of_ocaml tests
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2022 Hugo Heuzard
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
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

(* These tests pin down [Unix.localtime] in a chosen timezone by mutating
   [process.env.TZ], which is specific to the Node.js runtime. They are
   therefore excluded from the QuickJS and Wasm backends (see gen-rules). *)

open Js_of_ocaml

let%expect_test "localtime tm_yday under DST" =
  ignore (Js.Unsafe.js_expr {|(process.env.TZ = "America/New_York")|});
  (* 2021-07-01T04:30:00Z is 2021-07-01 00:30 in New York, with DST in
     effect; tm_yday must not be computed from the wall-clock distance
     to January 1 *)
  let tm = Unix.localtime 1625113800. in
  Printf.printf
    "%d-%02d-%02d %02d:%02d yday=%d dst=%b\n"
    (tm.Unix.tm_year + 1900)
    (tm.Unix.tm_mon + 1)
    tm.Unix.tm_mday
    tm.Unix.tm_hour
    tm.Unix.tm_min
    tm.Unix.tm_yday
    tm.Unix.tm_isdst;
  [%expect {| 2021-07-01 00:30 yday=181 dst=true |}]

let%expect_test "localtime tm_isdst for Europe/Dublin (negative DST)" =
  ignore (Js.Unsafe.js_expr {|(process.env.TZ = "Europe/Dublin")|});
  (* Dublin uses "negative DST": winter (GMT) is the daylight deviation
     from its summer standard (IST), so tm_isdst must match the native
     runtime, which reports it inverted from the usual offset heuristic *)
  let show t =
    let tm = Unix.localtime t in
    Printf.printf
      "%d-%02d-%02d %02d:%02d dst=%b\n"
      (tm.Unix.tm_year + 1900)
      (tm.Unix.tm_mon + 1)
      tm.Unix.tm_mday
      tm.Unix.tm_hour
      tm.Unix.tm_min
      tm.Unix.tm_isdst
  in
  show 1610712000. (* 2021-01-15 12:00 GMT, winter *);
  show 1625137200. (* 2021-07-01 12:00 IST, summer *);
  [%expect {|
    2021-01-15 12:00 dst=true
    2021-07-01 12:00 dst=false
    |}]
