(* Js_of_ocaml example
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2010 Jérôme Vouillon
 * Laboratoire PPS - CNRS Université Paris Diderot
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

open Js_of_ocaml
open Common

let log_stop = log_start "Regexp test suite"

let () =
  let re1 = Regexp.regexp "ab?" in
  let re2 = Regexp.regexp "\\." in
  let re3 = Regexp.regexp_string "(.)\\(.)" in
  let s1 = "totobtutua" in
  let s2 = "rr.ee.ab.a.b.bb.a.ee." in
  begin match Regexp.string_match re1 s1 0 with
    | None -> log_failure "Can't match 1 1"
    | Some r ->
        let x = Regexp.matched_string r in
        if x = "a" then
          log_success ()
        else
          log_failure ("Wrong match 1 1: " ^ x)
  end;
  begin match Regexp.string_match re1 s2 0 with
    | None -> log_failure "Can't match 1 2"
    | Some r ->
        let x = Regexp.matched_string r in
        if x = "ab" then
          log_success ()
        else
          log_failure ("Wrong match 1 2: " ^ x)
  end;
  begin
    let l = Regexp.split re2 s2 in
    if l = ["rr";"ee";"ab";"a";"b";"bb";"a";"ee";""] then
      log_success ()
    else
      log_failure "Wrong split 2 2"
  end ;
  begin
    let x = Regexp.global_replace re2 s2 "" in
    if x = "rreeababbbaee" then
      log_success ()
    else
      log_failure ("Wrong replacement 2 2: " ^ x)
  end ;
  begin match Regexp.string_match re3 "(.)\\(.)" 0 with
    | None -> log_failure "Quote 3 3"
    | Some _ -> log_success ()
  end

let () = log_stop ()
