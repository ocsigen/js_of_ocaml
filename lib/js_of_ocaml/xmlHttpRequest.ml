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
open! Import

type readyState =
  | UNSENT
  | OPENED
  | HEADERS_RECEIVED
  | LOADING
  | DONE

type _ response =
  | ArrayBuffer : Typed_array.arrayBuffer t Opt.t response
  | Blob : #File.blob t Opt.t response
  | Document : Dom.element Dom.document t Opt.t response
  | JSON : 'a Opt.t response
  | Text : js_string t response
  | Default : string response

class type xmlHttpRequest = object ('self)
  method onreadystatechange : (unit -> unit) Js.callback Js.writeonly_prop

  method readyState : readyState readonly_prop

  method _open : js_string t -> js_string t -> bool t -> unit meth

  method _open_full :
       js_string t
    -> js_string t
    -> bool t
    -> js_string t opt
    -> js_string t opt
    -> unit meth

  method setRequestHeader : js_string t -> js_string t -> unit meth

  method overrideMimeType : js_string t -> unit meth

  method send : js_string t opt -> unit meth

  method send_blob : #File.blob t -> unit meth

  method send_document : Dom.element Dom.document t -> unit meth

  method send_formData : Form.formData t -> unit meth

  method abort : unit meth

  method status : int readonly_prop

  method statusText : js_string t readonly_prop

  method getResponseHeader : js_string t -> js_string t opt meth

  method getAllResponseHeaders : js_string t meth

  method response : File.file_any readonly_prop

  method responseText : js_string t opt readonly_prop

  method responseXML : Dom.element Dom.document t opt readonly_prop

  method responseType : js_string t prop

  method withCredentials : bool t writeonly_prop

  inherit File.progressEventTarget

  method ontimeout :
    ('self t, 'self File.progressEvent t) Dom.event_listener writeonly_prop

  method upload : xmlHttpRequestUpload t readonly_prop
end

and xmlHttpRequestUpload = object ('self)
  inherit File.progressEventTarget
end

module Event = struct
  type typ = xmlHttpRequest File.progressEvent t Dom.Event.typ

  let readystatechange = Dom.Event.make "readystatechange"

  let loadstart = Dom.Event.make "loadstart"

  let progress = Dom.Event.make "progress"

  let abort = Dom.Event.make "abort"

  let error = Dom.Event.make "error"

  let load = Dom.Event.make "load"

  let timeout = Dom.Event.make "timeout"

  let loadend = Dom.Event.make "loadend"
end

external create : unit -> xmlHttpRequest Js.t = "caml_xmlhttprequest_create"
