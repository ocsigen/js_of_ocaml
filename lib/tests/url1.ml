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

let log_stop = log_start "Url test suite"
let url_string_url u = Url.url_of_string (Url.string_of_url u)
let url = "http://ocsigen.org/js_of_ocaml/"
let () = match Url.url_of_string url with
  | None -> log_failure "can't parse current url2"
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

let () = log_stop ()
