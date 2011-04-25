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

let log_success s =
  Firebug.console##log_2 (Js.string "\tSUCCESS: ", Js.string s)
let log_failure s =
  Firebug.console##log_2 (Js.string "\tFAILURE: ", Js.string s)

let log_start s =
  Firebug.console##log_2 (Js.string "START: ", Js.string s)
let log_stop s =
  Firebug.console##log_2 (Js.string "STOP: ", Js.string s)

let raw_log x =
  Firebug.console##log_2 (Js.string "\t\t", x)
let log s = raw_log (Js.string s)


(* TEST Url *)


let () = log_start "Url test suite"

let url_string_url u = Url.url_of_string (Url.string_of_url u)
let () = match Url.Current.get () with
  | None -> log_failure "can't parse current url"
  | Some u -> match url_string_url u with
    | None -> log_failure "can't parse pretty-printed url"
    | Some v ->
       if u = v
       then (log_success "fixpoint acheived";
             log (Url.string_of_url u)
       )
       else (log_failure "no fixpoint";
             log (Url.string_of_url u);
             log (Url.string_of_url v)
       )
let () =
  let t1 = Url.urlencode "/toto+ blah&tutu" in
  let t2 = Url.urlencode ~with_plus:false "/toto+ blah&tutu" in
  if t1 = "/toto%2B%20blah%26tutu" && t2 = "/toto+%20blah%26tutu"
  then (log_success "escaping works";
        log t2)
  else (log_failure "escaping error";
        log t1; log t2)


let () = log_stop "Url test suite"


(* Tests Regexp *)


let () = log_start "Regexp test suite"

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
        if x = "a"
        then log_success "Match 1 1"
        else log_failure ("Wrong match 1 1: " ^ x)
  end;
  begin match Regexp.string_match re1 s2 0 with
    | None -> log_failure "Can't match 1 2"
    | Some r ->
        let x = Regexp.matched_string r in
        if x = "ab"
        then log_success "Match 1 2"
        else log_failure ("Wrong match 1 2: " ^ x)
  end;
  begin
    if Regexp.split re2 s2 = ["rr";"ee";"ab";"a";"b";"bb";"a";"ee";""]
    then log_success "Split 2 2"
    else log_failure "Wrong split 2 2"
  end ;
  begin
    let x = Regexp.global_replace re2 s2 "" in
    if x = "rreeababbbaee"
    then log_success "Replace 2 2"
    else log_failure ("Wrong replacement 2 2: " ^ x)
  end ;
  begin
    match Regexp.string_match re3 "(.)\\(.)" 0 with
      | None -> log_failure "Quote 3 3"
      | Some x -> log_success "Quote 3 3"
  end


let () = log_stop "Regexp test suite"


(* Tests Colors *)

let () = log_start "Colors test suite"

let () =
  let cols = [
    CSS.RGB(120,  3, 56);
    CSS.RGBA(120,  3, 56,1.);
    CSS.RGB_percent( 10, 3,60);
    CSS.RGBA_percent(100,53,60, 0.45);
    CSS.HSL(120,75,56);
    CSS.HSLA(180, 3,56,0.2);
    CSS.RGB (CSS.rgb_of_color_name CSS.Dodgerblue);
    CSS.RGB (CSS.rgb_of_color_name CSS.Pink);
    CSS.Color_name CSS.Hotpink;
    CSS.Color_name CSS.Cornsilk;
  ]
  in
  List.iter
    (fun c ->
      try
        let c = CSS.string_of_color c in
        let (_: CSS.js_color) = CSS.js_color_of_js_string (Js.string c) in
        log_success c
      with
        | Invalid_argument s -> log_failure s
    )
    cols

let () = log_stop "Colors test suite"


