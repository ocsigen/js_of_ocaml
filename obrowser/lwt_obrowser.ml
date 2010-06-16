(* Js_of_ocaml library
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2010 Jérôme Vouillon
 * Laboratoire PPS - CNRS Université Paris Diderot
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

let register_event node name (f : 'a -> unit Lwt.t) =
  Js.Node.register_event node name
    (fun a -> f a)

open Jslib

external magic : 'a -> 'b = "%identity"

let sleep = Lwt_js.sleep
let yield = Lwt_js.yield
let run _ = magic ()

let js = Js.string

external escape : Js.js_string Js.t -> Js.js_string Js.t = "escape"

let urlencode_string s = Js.to_string (escape (js s))

let urlencode args =
  String.concat "&"
    (List.map
       (fun (n,v) -> urlencode_string n ^ "=" ^ urlencode_string v)
       args)

(*FIX: this is extremely slow;  *)
(* the following encode the whole string, even regular chars.
   Otherwise it does not work with Ocaml's Marshalled data *)
(*FIX: we probably want to simply escape the 8bit encoding of the string *)
let urlencode_string_ s =
  let hex c =
    Char.chr ((if c < 10 then Char.code '0' else Char.code 'A' - 10) + c)
  in
  let s' = String.make (String.length s * 3) ' ' in
  for i = 0 to String.length s - 1 do
    s'.[i * 3] <- '%' ;
    s'.[i * 3 + 1] <- hex ((Char.code s.[i]) lsr 4) ;
    s'.[i * 3 + 2] <- hex ((Char.code s.[i]) land 0xF)
  done ;
  s'

let urlencode_ args =
 String.concat "&"
   (List.map
      (fun (n,v) -> (urlencode_string_ n) ^ "=" ^ (urlencode_string_ v))
      args
   )

let http_get url args =
  let (res, w) = Lwt.wait () in
  let url = if args = [] then url else url ^ urlencode args in
  let req = XmlHttpRequest.create () in
  req##_open (js "GET", js url, Js._true);
  req##onreadystatechange <- Js.some
    (fun () ->
       if req##readyState = XmlHttpRequest.DONE then
         Lwt.wakeup w (req##status, Js.to_string req##responseText));
  req##send (Js.null);
  res

let http_post url post_args =
  let (res, w) = Lwt.wait () in
  let req = XmlHttpRequest.create () in
  req##_open (js "POST", js url, Js._true);
  req##setRequestHeader
    (js"Content-type",js"application/x-www-form-urlencoded");
  req##onreadystatechange <- Js.some
    (fun () ->
       if req##readyState = XmlHttpRequest.DONE then
         Lwt.wakeup w (req##status, Js.to_string req##responseText));
  req##send (Js.some (js (urlencode_ post_args)));
  res

let http_get_post url get_args post_args =
  let url = url ^ "?" ^ urlencode get_args in
  http_post url post_args
