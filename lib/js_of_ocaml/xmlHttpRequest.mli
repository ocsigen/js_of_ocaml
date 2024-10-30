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

(** XmlHttpRequest object. *)

open Js

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

  method upload : xmlHttpRequestUpload t optdef readonly_prop
end

and xmlHttpRequestUpload = object ('self)
  inherit File.progressEventTarget
end

val create : unit -> xmlHttpRequest t

(** The next part of this module allow one to use Ocaml with no need for Javascript
    documentation. *)

module Event : sig
  type typ = xmlHttpRequest File.progressEvent t Dom.Event.typ

  val readystatechange : xmlHttpRequest Dom.event t Dom.Event.typ

  val loadstart : typ

  val progress : typ

  val abort : typ

  val error : typ

  val load : typ

  val timeout : typ

  val loadend : typ
end
