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

type readyState = UNSENT | OPENED | HEADERS_RECEIVED | LOADING | DONE

class type xmlHttpRequest = object ('self)
  method onreadystatechange : (unit -> unit) Js.callback Js.writeonly_prop
  method readyState : readyState readonly_prop
  method _open :
    js_string t -> js_string t -> bool t -> unit meth
  method _open_full :
    js_string t -> js_string t -> bool t ->
    js_string t opt -> js_string t opt -> unit meth
  method setRequestHeader : js_string t -> js_string t -> unit meth
  method overrideMimeType : js_string t -> unit meth
  method send : js_string t opt -> unit meth
  method send_document : Dom.element Dom.document -> unit meth
  method send_formData : Form.formData t -> unit meth
  method abort : unit meth
  method status : int readonly_prop
  method statusText : js_string t readonly_prop
  method getResponseHeader : js_string t -> js_string t opt meth
  method getAllResponseHeaders : js_string t meth
  method responseText : js_string t readonly_prop
  method responseXML : Dom.element Dom.document t opt readonly_prop

  inherit File.progressEventTarget
  method ontimeout : ('self t, 'self File.progressEvent t) Dom.event_listener writeonly_prop
  method upload : xmlHttpRequestUpload t optdef readonly_prop
end

and xmlHttpRequestUpload = object ('self)
  inherit File.progressEventTarget
end

val create : unit -> xmlHttpRequest t

(** The next part of this module allow one to use Ocaml with no need for
     Javascript documentation. *)

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

type http_frame =
    {
      url: string;
      code: int;
      headers: string -> string option;
      content: string;
      content_xml: unit -> Dom.element Dom.document t option;
    }
(** The type for XHR results. The code field is the http status code of the
    answer. The headers field is a function associating values to any header
    name. *)

exception Wrong_headers of (int * (string -> string option))
(** The exception raise by perform functions when the check_headers
    parameter returned false. The parameter of the exception is a
    function is like the [headers] function of [http_frame] *)

val perform_raw_url :
    ?headers:(string * string) list
  -> ?content_type:string
  -> ?post_args:((string * Form.form_elt) list)
  -> ?get_args:((string * string) list)  (* [] *)
  -> ?form_arg:Form.form_contents
  -> ?check_headers:(int -> (string -> string option) -> bool)
  -> ?progress:(int -> int -> unit)
  -> ?upload_progress:(int -> int -> unit)
  -> ?override_mime_type:string
  -> string
  -> http_frame Lwt.t
  (** [perform_raw_url ?headers ?content_type ?post_args ?get_args ?form_arg url]
      makes an asynchronous request to the specified [url] with
      specified options. The result is a cancelable thread returning
      an HTTP frame. If [post_args] and [form_arg] are [None], a GET request is
      used. If [post_args] or [form_arg] is [Some _] (even [Some []]) then a POST
      request is made. The [check_headers] argument is run as soon as the answer
      code and headers are available. If it returns false, the request is canceled
      and the functions raise the [Wrong_headers] exception *)

val perform :
    ?headers:(string * string) list
  -> ?content_type:string
  -> ?post_args:((string * Form.form_elt) list)
  -> ?get_args:((string * string) list)  (* [] *)
  -> ?form_arg:Form.form_contents
  -> ?check_headers:(int -> (string -> string option) -> bool)
  -> ?override_mime_type:string
  -> Url.url
  -> http_frame Lwt.t
  (** [perform] is the same as {!perform_raw_url} except that the Url argument has type
      {!Url.url}. *)

val get : string -> http_frame Lwt.t
  (** [get url] makes an asynchronous request to the specified [url] *)
