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

open Js_of_ocaml
open Js
open XmlHttpRequest

type 'response generic_http_frame =
  { url : string
  ; code : int
  ; headers : string -> string option
  ; content : 'response
  ; content_xml : unit -> Dom.element Dom.document t option
  }
(** The type for XHR results. The code field is the http status code of the answer. The
    headers field is a function associating values to any header name. *)

type http_frame = string generic_http_frame

exception Wrong_headers of (int * (string -> string option))
(** The exception raise by perform functions when the check_headers parameter returned
    false. The parameter of the exception is a function is like the [headers] function of
    [http_frame] *)

val perform_raw :
     ?headers:(string * string) list
  -> ?content_type:string
  -> ?get_args:(string * string) list
  -> ?check_headers:
       (   (* [] *)
           int
        -> (string -> string option)
        -> bool)
  -> ?progress:(int -> int -> unit)
  -> ?upload_progress:(int -> int -> unit)
  -> ?contents:
       [< `POST_form of (string * Form.form_elt) list
       | `Form_contents of Form.form_contents
       | `String of string
       | `Blob of #File.blob Js.t
       ]
  -> ?override_mime_type:string
  -> ?override_method:[< `GET | `POST | `HEAD | `PUT | `DELETE | `OPTIONS | `PATCH ]
  -> ?with_credentials:bool
  -> response_type:'a response
  -> string
  -> 'a generic_http_frame Lwt.t
(** [perform_raw] is the same as {!perform_raw_url} except that an additional
    response_type argument can be given to set the XMLHttpRequest responseType, and hence
    return different types of data for GET requests. *)

val perform_raw_url :
     ?headers:(string * string) list
  -> ?content_type:string
  -> ?get_args:(string * string) list
  -> ?check_headers:
       (   (* [] *)
           int
        -> (string -> string option)
        -> bool)
  -> ?progress:(int -> int -> unit)
  -> ?upload_progress:(int -> int -> unit)
  -> ?contents:
       [< `POST_form of (string * Form.form_elt) list
       | `Form_contents of Form.form_contents
       | `String of string
       | `Blob of #File.blob Js.t
       ]
  -> ?override_mime_type:string
  -> ?override_method:[< `GET | `POST | `HEAD | `PUT | `DELETE | `OPTIONS | `PATCH ]
  -> ?with_credentials:bool
  -> string
  -> http_frame Lwt.t
(** [perform_raw_url] makes an asynchronous request to the specified [url] with specified
    options. The result is a cancelable thread returning an HTTP frame. By default, if
    [post_args] and [form_arg] are [None], a GET request is used. If [post_args] or
    [form_arg] is [Some _] (even [Some []]) then a POST request is made. But if
    [override_method] is set, the request method is forced, no matter the [post_args] or
    [form_arg] value. For example, with [override_method] set to [`PUT] and [form_arg] set
    to [Some _] a PUT request including the form data will be made. The [check_headers]
    argument is run as soon as the answer code and headers are available. If it returns
    false, the request is canceled and the functions raise the [Wrong_headers] exception
*)

val perform :
     ?headers:(string * string) list
  -> ?content_type:string
  -> ?get_args:(string * string) list
  -> ?check_headers:
       (   (* [] *)
           int
        -> (string -> string option)
        -> bool)
  -> ?progress:(int -> int -> unit)
  -> ?upload_progress:(int -> int -> unit)
  -> ?contents:
       [< `POST_form of (string * Form.form_elt) list
       | `Form_contents of Form.form_contents
       | `String of string
       | `Blob of #File.blob Js.t
       ]
  -> ?override_mime_type:string
  -> ?override_method:[< `GET | `POST | `HEAD | `PUT | `DELETE | `OPTIONS | `PATCH ]
  -> ?with_credentials:bool
  -> Url.url
  -> http_frame Lwt.t
(** [perform] is the same as {!perform_raw_url} except that the Url argument has type
    {!Url.url}. *)

val get : string -> http_frame Lwt.t
(** [get url] makes an asynchronous request to the specified [url] *)
