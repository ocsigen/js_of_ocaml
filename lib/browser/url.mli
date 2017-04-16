(* Js_of_ocaml library
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2010 Raphaël Proust
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

(** This module provides functions for tampering with Url. It's main goal is to
    allow one to stay in the Ocaml realm without wandering into the
    {!Dom_html.window}##.location object. *)


(** The first functions are mainly from and to string conversion functions for
     the different parts of a url. *)

(** [urldecode s] swaps percent encoding characters for their usual
    representation. *)
val urldecode : string -> string

(** [urlencode ?with_plus s] replace characters for their percent encoding
    representation. Note that the '/' (slash) character is escaped as well. If
    [with_plus] is [true] (default) then ['+']'s are escaped as ["%2B"]. If not,
    ['+']'s are left as is. *)
val urlencode : ?with_plus:bool -> string -> string


(** The type for HTTP(s) url. *)
type http_url = {
  hu_host : string; (** The host part of the url. *)
  hu_port : int; (** The port for the connection if any. *)
  hu_path : string list; (** The path splitted on ['/'] characters. *)
  hu_path_string : string; (** The original entire path. *)
  hu_arguments : (string * string) list; (** Arguments as a field-value
                                             association list.*)
  hu_fragment : string; (** The fragment part (after the ['#'] character). *)
}

(** The type for local file urls. *)
type file_url = {
  fu_path : string list;
  fu_path_string : string;
  fu_arguments : (string * string) list;
  fu_fragment : string;
}

(** The type for urls. *)
type url =
  | Http of http_url  (**Non secure HTTP urls*)
  | Https of http_url (**Secure HTTPS urls*)
  | File of file_url  (**Local files*)

(** The default port for [Http] communications (80). *)
val default_http_port : int

(** The default port for [Https] communications (443). *)
val default_https_port : int

(** [path_of_path_string s] splits [s] on each ["/"] character. *)
val path_of_path_string : string -> string list

(** [encode_arguments a] expects a list of pair of values of the form
    [(name,value)] were [name] is the name of an argument and [value] it's
    associated value. *)
val encode_arguments : (string * string) list -> string

(** [decode_arguments s] parses [s] returning the sliced-diced
    association list. *)
val decode_arguments : string -> (string * string) list


(** The following part allow one to handle Url object in a much higher level
    than what a string provides. *)


(** [url_of_string s] parses [s] and builds a value of type [url] if [s] is not
    a valid url string, it returns [None]. *)
val url_of_string : string -> url option

(** [string_of_url u] returns a valid string representation of [u]. Note that
  * [string_of_url ((fun Some u -> u) (url_of_string s))] is NOT necessarily
  * equal to [s]. However [url_of_string (string_of_url u) = u]. *)
val string_of_url : url -> string

(** This module can be used to handle the Url associated to the current
    document. *)
module Current :

  sig

    (** The host part of the current url. *)
    val host : string

    (** The port of the current url. *)
    val port : int option

    (** The protocol of the current url. *)
    val protocol : string

    (** The path of the current url as one long string. *)
    val path_string : string

    (** The path of the current url as a list of small string. *)
    val path : string list

    (** The arguments of the current url as an association list. *)
    val arguments : (string * string) list

    (** Because the [fragment] of the Url for the current document can change
        dynamically, we use a functional value here. *)
    val get_fragment : unit -> string

    (** [set_fragment s] replaces the current fragment by [s]. *)
    val set_fragment : string -> unit

    (** [get ()] returns a value of type {!url} with fields reflecting the
        state of the current Url. *)
    val get : unit -> url option

    (** [set u] replaces the current Url for [u]. {b WARNING:} Causes the
        document to change. *)
    val set : url -> unit

    (** [as_string] is the original string representation of the current Url. It
        is NOT necessarily equals to [string_of_url (get ())] but
        [url_of_string as_string = get ()] holds. *)
    val as_string : string


  end
