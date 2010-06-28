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

open Js

type readyState = UNSENT | OPENED | HEADERS_RECEIVED | LOADING | DONE

class type xmlHttpRequest = object
  method onreadystatechange : (unit -> unit) opt prop
  method readyState : readyState readonly_prop
  method _open :
    js_string t -> js_string t -> bool t -> unit meth
  method _open_full :
    js_string t -> js_string t -> bool t ->
    js_string t opt -> js_string t opt -> unit meth
  method setRequestHeader : js_string t -> js_string t -> unit meth
  method send : js_string t opt -> unit meth
  method send_document : Dom.element Dom.document -> unit meth
  method abort : unit meth
  method status : int readonly_prop
  method statusText : js_string t readonly_prop
  method getResponseHeader : js_string t -> js_string t opt meth
  method getAllResponseHeaders : js_string t meth
  method responseText : js_string t readonly_prop
  method responseXML : Dom.element Dom.document t opt readonly_prop
end

let xmlHttpRequest () : xmlHttpRequest t constr =
  Js.Unsafe.variable "XMLHttpRequest"

let activeXObject () : (js_string t -> xmlHttpRequest t) constr =
  Js.Unsafe.variable "ActiveXObject"

let create () =
  try jsnew (xmlHttpRequest ()) () with _ ->
  try jsnew (activeXObject ()) (Js.string "Msxml2.XMLHTTP") with _ ->
  try jsnew (activeXObject ()) (Js.string "Msxml3.XMLHTTP") with _ ->
  try jsnew (activeXObject ()) (Js.string "Microsoft.XMLHTTP") with _ ->
  assert false

let send_request url callback postData =
  let req = create () in
  let meth = Js.string (if postData == null then "GET" else "POST") in
  req##_open (meth, url, Js._true);
  Opt.iter postData
    (fun d -> req##setRequestHeader
                (Js.string "Content-type",
                 Js.string "application/x-www-form-urlencoded"));
  req##onreadystatechange <- Js.some
    (fun () ->
       (* For local files, req##status is 0 on success... *)
       if
         req##readyState = DONE &&
         (req##status = 0 || req##status = 200 || req##status = 304)
       then
         callback req);
  req##send (postData)



let escape_string s = Js.to_bytestring (Js.escape (Js.bytestring s))
let encode args = (*TODO: use buffers instead of strings *)
  String.concat "&"
    (List.map (fun (n,v) -> escape_string n ^ "=" ^ escape_string v) args)

let send_asynchronous_request
      ?(content_type="application/x-www-form-urlencoded")
      ?(post_args=[])
      ?(get_args=[])
      url =
  (* infer method *)
  let method_ = match post_args with | [] -> "GET" | _::_ -> "POST" in
  (* create Lwt task *)
  let (res, w) = Lwt.task () in
  (* create req *)
  let req = create () in
  (* set req properties *)
  req##_open (Js.string method_, Js.string url, Js._true);
  req##setRequestHeader (Js.string "Content-type", Js.string content_type);
  req##onreadystatechange <- Js.some
    (fun () ->
       if req##readyState = DONE then
         Lwt.wakeup w (req##status, Js.to_string req##responseText));
  (* send *)
  (match post_args with
     | [] -> req##send (Js.null)
     | _::_ as l -> req##send (Js.some (Js.string (encode l)))
  );
  (* abort on cancel *)
  Lwt.on_cancel res (fun () -> req##abort ()) ;
  res

