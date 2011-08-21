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
    CSS.Color.RGB(120, 3, 56);
    CSS.Color.RGBA(120, 3, 56,1.);
    CSS.Color.RGB_percent(10, 3,60);
    CSS.Color.RGBA_percent(100,53,60,0.45);
    CSS.Color.HSL(120,75,56);
    CSS.Color.HSLA(180, 3,56,0.2);
    CSS.Color.RGB (CSS.Color.rgb_of_name CSS.Color.Dodgerblue);
    CSS.Color.RGB (CSS.Color.rgb_of_name CSS.Color.Pink);
    CSS.Color.Name CSS.Color.Hotpink;
    CSS.Color.Name CSS.Color.Cornsilk;
  ]
  in
  List.iter
    (fun c ->
      try
        let js = CSS.Color.js c  in
        let ml = CSS.Color.ml js in
        if c = ml then
          log_success (CSS.Color.string_of_t c)
        else
          log_failure (Printf.sprintf "%s   %s"
            (CSS.Color.string_of_t c)
            (CSS.Color.string_of_t ml)
            )
      with
        | Invalid_argument s -> log_failure s
        | Failure s -> log_failure s
    )
    cols

let () = log_stop "Colors test suite"


(* CSS.Length testing *)

let () = log_start "CSS.Length test suite"

let () =
  let ls =
    [ CSS.Length.Em  0.1
    ; CSS.Length.Ex  0.12
    ; CSS.Length.Px  5.4
    ; CSS.Length.Gd  0.
    ; CSS.Length.Rem 10.0
    ; CSS.Length.Vw  0.10
    ; CSS.Length.Vh  0.1
    ; CSS.Length.Vm  0.5
    ; CSS.Length.Ch  20.6
    ; CSS.Length.Zero
    ; CSS.Length.Mm 100.
    ; CSS.Length.Cm 10.1
    ; CSS.Length.In 0.1
    ; CSS.Length.Pt 10.2
    ; CSS.Length.Pc 103.1
    ]
  in
  List.iter
    (fun c ->
      try
        let js = CSS.Length.js c  in
        let ml = CSS.Length.ml js in
        if c = ml then
          log_success (CSS.Length.string_of_t c)
        else
          log_failure (Printf.sprintf "%s   %s"
            (CSS.Length.string_of_t c)
            (CSS.Length.string_of_t ml)
            )
      with
        | Invalid_argument s -> log_failure s
        | Failure s -> log_failure s
    )
    ls


let () = log_stop "CSS.Length test suite"


