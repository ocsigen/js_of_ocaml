(***************************************************************************)
(*  Copyright (C) 2000-2013 LexiFi SAS. All rights reserved.               *)
(*                                                                         *)
(* This program is free software: you can redistribute it and/or modify    *)
(* it under the terms of the GNU General Public License as published       *)
(* by the Free Software Foundation, either version 3 of the License,       *)
(* or (at your option) any later version.                                  *)
(*                                                                         *)
(* This program is distributed in the hope that it will be useful,         *)
(* but WITHOUT ANY WARRANTY; without even the implied warranty of          *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the           *)
(* GNU General Public License for more details.                            *)
(*                                                                         *)
(* You should have received a copy of the GNU General Public License       *)
(* along with this program.  If not, see <http://www.gnu.org/licenses/>.   *)
(***************************************************************************)

type date = int

type gregorian = {
    year : int;
    month : int;
    day : int;
    hour : int;
    minute : int
  }

let hours_in_day = 24
let minutes_in_day = hours_in_day * 60
let fminutes_in_day = float minutes_in_day
let minutes_to_noon = (hours_in_day / 2) * 60

(*
   Communications of the ACM by Henry F. Fliegel and Thomas C. Van Flandern,
   ``A Machine Algorithm for Processing Calendar Dates'',
   CACM, volume 11, number 10, October 1968, p. 657
*)
let date_of_gregorian {year = y; month = m; day = d; hour = hr; minute = mn} =
  (
   (match m with
   | 1 | 2 ->
       ( 1461 * ( y + 4800 - 1 ) ) / 4 +
         ( 367 * ( m + 10 ) ) / 12 -
         ( 3 * ( ( y + 4900 - 1 ) / 100 ) ) / 4
   | _ ->
       ( 1461 * ( y + 4800 ) ) / 4 +
         ( 367 * ( m - 2 ) ) / 12 -
         ( 3 * ( ( y + 4900 ) / 100 ) ) / 4)
     + d - 32075 - 2444238) * minutes_in_day
    + hr * 60 + mn

let gregorian_of_date minutes_since_epoch =
  let jul = minutes_since_epoch / minutes_in_day in
  let l = jul + 68569 + 2444238 in
  let n = ( 4 * l ) / 146097 in
  let l = l - ( 146097 * n + 3 ) / 4 in
  let i = ( 4000 * ( l + 1 ) ) / 1461001 in
  let l = l - ( 1461 * i ) / 4 + 31 in
  let j = ( 80 * l ) / 2447 in
  let d = l - ( 2447 * j ) / 80 in
  let l = j / 11 in
  let m = j + 2 - ( 12 * l ) in
  let y = 100 * ( n - 49 ) + i + l in
  let daytime = minutes_since_epoch mod minutes_in_day in
  if daytime = minutes_to_noon
  then {year = y; month = m; day = d; hour = 12; minute = 0}
  else {year = y; month = m; day = d; hour = daytime / 60; minute = daytime mod 60}

let check_date ~year ~month ~day =
  1 <= day &&
  1 <= month && month <= 12 &&
  1980 <= year && year <= 2299 &&
  begin
    day <= 28 ||
    match month with
    | 2 -> day = 29 && year mod 4 = 0 && (year = 2000 || (year mod 100 <> 0))
          (* we don't check y mod 400 because 2000 is ok and we don't support
             neither 1600 nor 2400. *)
    | 4 | 6 | 9 | 11 -> day <= 30
    | _ -> day <= 31
  end

let of_string s : date =
  let sub ofs len =
    let rec sub acc ofs len =
      if len = 0
      then acc
      else sub (acc * 10 + int_of_char(String.unsafe_get s ofs) - 48) (ofs + 1) (len - 1)
    in
    sub (int_of_char(String.unsafe_get s ofs) - 48) (ofs + 1) (len - 1)
  in
  if String.length s < 10 then invalid_arg "date_of_string";
  let year = sub 0 4 in
  let month = sub 5 2 in
  let day = sub 8 2 in
  (* minimal coherence check of the date, just what is needed more than what the lexer is doing *)
  if check_date ~year ~month ~day then
    if String.length s < 16
    then date_of_gregorian{year; month; day; hour=12; minute=0}
    else date_of_gregorian{year; month; day; hour=sub 11 2; minute=sub 14 2}
  else invalid_arg "date_of_string"

let days_between t1 t2 =
  float (t1 - t2) /.  fminutes_in_day

let act_365 t1 t2 = (days_between t1 t2) /. 365.

let leap y = (y mod 4 = 0) && ((y mod 100 <> 0) || (y mod 400 = 0))
let end_of_month year month =
  match month with
  | 2 when leap year -> 29
  | 2 -> 28
  | 4 | 6 | 9 | 11 -> 30
  | _ -> 31
let add_months date nbmonths =
  let {year = y; month = m; day = d; hour = _; minute = _;} as date = gregorian_of_date date in
  let m = m + nbmonths in
  let y, m = y + (m-1) / 12, ((m-1) mod 12) + 1 in
  let y, m = if m <= 0 then y - 1, m + 12 else y, m in
  date_of_gregorian {date with year = y; month = m; day = min d (end_of_month y m)}
let add_years date nbyears = add_months date (nbyears * 12)

let max_date = of_string "2299-12-31T23:59:59"
let min_date = of_string "1980-01-01T12:00:00"
