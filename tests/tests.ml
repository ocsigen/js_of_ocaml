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

let success_count = ref 0
let test_count = ref 0

let log_success () = incr success_count; incr test_count
let log_failure s =
  incr test_count;
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
       if u = v then
         log_success ()
       else
         log_failure "no fixpoint"
let () =
  let t1 = Url.urlencode "/toto+ blah&tutu" in
  let t2 = Url.urlencode ~with_plus:false "/toto+ blah&tutu" in
  if t1 = "/toto%2B%20blah%26tutu" && t2 = "/toto+%20blah%26tutu" then
    log_success ()
  else
    log_failure "escaping error"


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
    | Some x -> log_success ()
  end


let () = log_stop "Regexp test suite"


(* Tests Colors *)

let () = log_start "CSS.Colors test suite"

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
          log_success ()
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

let () = log_stop "CSS.Colors test suite"


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
          log_success ()
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


(*CSS.Angle test suite*)

let () = log_start "CSS.Angle test suite"

let () =
  let a =
    [ CSS.Angle.Rad  0.1
    ; CSS.Angle.Turns  0.12
    ; CSS.Angle.Deg  5.4
    ; CSS.Angle.Turns  0.
    ; CSS.Angle.Grad 10.0
    ; CSS.Angle.Grad  0.10
    ]
  in
  List.iter
    (fun c ->
      try
        let js = CSS.Angle.js c  in
        let ml = CSS.Angle.ml js in
        if c = ml then
          log_success ()
        else
          log_failure (Printf.sprintf "%s   %s"
            (CSS.Angle.string_of_t c)
            (CSS.Angle.string_of_t ml)
            )
      with
        | Invalid_argument s -> log_failure s
        | Failure s -> log_failure s
    )
    a

let () = log_stop "CSS.Angle test suite"


(* Json conversion *)

let () = log_start "Json"

let str = String.create 256

let () =
  for i = 0 to 255 do
    str.[i] <- Char.chr i
  done

type t = int list * float option * string deriving (Json)

let test t v =
  if v = Json.unsafe_input (Json.output v)
  then log_success ()
  else log_failure "Not equal";
  if v = Deriving_Json.from_string t (Js.to_string (Json.output v))
  then log_success ()
  else log_failure "Not equal";
  if v = Json.unsafe_input (Js.string (Deriving_Json.to_string t v))
  then log_success ()
  else log_failure "Not equal";
  if v = Deriving_Json.from_string t (Deriving_Json.to_string t v)
  then log_success ()
  else log_failure "Not equal"

let _ = test Json.t<t> ([1;2;3], Some 1., str)

type intseq = Z | S of int * intseq deriving (Json)

let _ = test Json.t<intseq> (S (1, S (2, S (3, Z))))

type 'a seq = ZZ | SS of 'a * 'a seq deriving (Json)

let _ = test Json.t<int seq> (SS (1, SS (2, SS (3, ZZ))))

let () = log_stop "Json"

let () =
  Firebug.console##log(
    Js.string (
      Printf.sprintf "Test results: %d sucesses out of %d tests"
        !success_count !test_count
      )
    )

