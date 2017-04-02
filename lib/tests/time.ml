(* Js_of_ocaml example
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2013 Hugo Heuzard
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

open Common
let log_stop = log_start "Time test suite"


let log_failure_tm name tmg =
  let open Unix in
  log_failure (Printf.sprintf "%s: s:%d; m:%d; h:%d; D:%d; M:%d; Y:%d; WD:%d; YD:%d; dst:%b"
                 name
                 tmg.tm_sec tmg.tm_min tmg.tm_hour
                 tmg.tm_mday tmg.tm_mon tmg.tm_year
                 tmg.tm_wday tmg.tm_yday tmg.tm_isdst)


let now = 1377134255.469(* +. 24. *. 60. *. 60. *. 1. *)

(* check gmtime *)
let tmg = Unix.gmtime now
let () = match tmg with
  | {Unix.tm_sec = 35; Unix.tm_min = 17; Unix.tm_hour = 1; Unix.tm_mday = 22;
      Unix.tm_mon = 7; Unix.tm_year = 113; Unix.tm_wday = 4; Unix.tm_yday = 233;
      Unix.tm_isdst = false} -> log_success ()
  | _ -> log_failure_tm "Unix.gmtime" tmg

(* check localetime *)
(*
let tm = Unix.localtime now
let () = match tm with
  | {Unix.tm_sec = 35; Unix.tm_min = 17; Unix.tm_hour = 18; Unix.tm_mday = 21;
      Unix.tm_mon = 7; Unix.tm_year = 113; Unix.tm_wday = 3; Unix.tm_yday = 232;
      Unix.tm_isdst = true} -> log_success ()
  | _ -> log_failure_tm "Unix.localtime" tm

(* check normalization *)
let norm = Unix.mktime {tm with Unix.tm_mon = 9; Unix.tm_mday = 40}
let _ = match norm with
  | (1384049855.,
     {Unix.tm_sec = 35; Unix.tm_min = 17; Unix.tm_hour = 18; Unix.tm_mday = 9;
    Unix.tm_mon = 10; Unix.tm_year = 113; Unix.tm_wday = 6;
    Unix.tm_yday = 312; Unix.tm_isdst = false}) -> log_success ()
  | (1384049855.,tm) -> log_failure_tm "Unix.mktime" tm
  | (wrong,tm) -> log_failure_tm "Unix.mktime" tm;
    log_failure (Printf.sprintf "Unix.mktime: %.0f <> 1384049855" wrong)
*)
let () = log_stop ()
