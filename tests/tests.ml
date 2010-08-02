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


let _ = log_start "Url test suite"

let url_string_url u = Url.url_of_string (Url.string_of_url u)
let _ =
  match url_string_url (Url.Current.get ()) with
    | None ->
        log_failure "can't parse once";
        log Url.Current.as_string
    | Some u -> match url_string_url u with
        | Some v ->
          if u = v
          then (log_success "fixpoint acheived";
                log (Url.string_of_url u)
               )
          else (log_failure "no fixpoint";
                log (Url.string_of_url u);
                log (Url.string_of_url v)
               )
        | None ->
          log_failure "can't parse twice";
          log (Url.string_of_url u)
let _ =
  let t1 = Url.urlencode "/toto+ blah&tutu" in
  let t2 = Url.urlencode ~with_plus:false "/toto+ blah&tutu" in
  if t1 = "/toto%2B%20blah%26tutu" && t2 = "/toto+%20blah%26tutu"
  then (log_success "escaping works";
        log t2)
  else (log_failure "escaping error";
        log t1; log t2)


let _ = log_stop "Url test suite"

