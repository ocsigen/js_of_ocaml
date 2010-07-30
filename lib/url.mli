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

(** This module provides functions for tampering with Url. It's main goal is to
    allow one to stay in the Ocaml realm without wandering into the
    {!Dom_html.window}##location object. *)


(** The first functions are mainly from and to string conversion functions for
     the different parts of a url. *)

val urldecode : string -> string
(** [urldecode s] swaps percent encoding characters for their usual
    representation. *)

val urlencode : string -> string
(** [urlencode s] replace characters for their percent encoding representation
  *)

type protocol = Http | Https | File | Exotic of string
(** The type for protocols. [File] is for local files and [Exotic s] is for
    unknown/unsupported protocols. *)

val protocol_of_string : string -> protocol
(** [protocol_of_string s] parses [s] and tries to figure out what protocol [s]
    represents. If no match is found, it returns [Exotic s]. *)

val string_of_protocol : ?comma:bool -> protocol -> string
(** [string_of_protocol ?comma p] return a string representing the protocol [p].
    If [comma] is true then the [':'] character is appended. [comma] defaults to
    false. *)

val default_http_port : int
(** The default port for [Http] communications (80). *)

val default_https_port : int
(** The default port for [Https] communications (443). *)

val port_of_string : protocol -> string -> int option
(** [port_of_string p s] parses [s] trying to figure out what port it
    represents. If [s] is empty then the default port for [p] is used (if any
    exists). If [s] is not a valid port number, or if [s] is the empty string
    and [p] is either [Exotic _] or [File], then [None] is returned. *)

val string_of_port : int -> string
(** [string_of_port i] gives a string representative of [i]. *)

val string_of_port_option : protocol -> int option -> string option
(** [string_of_port_option p (Some i)] is [Some (string_of_port i)].
    [string_of_port_option p None] is the default port for [p] or [None] if [p]
    has no default port. *)

val path_of_path_string : string -> string list
(** [path_of_path_string s] splits [s] on each ["/"] character. *)

val encode_arguments : (string * string) list -> string
(** [encode_arguments a] expects a list of pair of values of the form
    [(name,value)] were [name] is the name of an argument and [value] it's
    associated value. *)

val decode_arguments : string -> (string * string) list
(** [decode_arguments s] parses [s] returning the sliced-diced
    association list. *)

(** The following part allow one to handle Url object in a much higher level
    than what a string provides. *)


type url = {
  protocol : protocol;
  host : string;
  port : int option;
  path : string list;
  path_string : string;
  arguments : (string * string) list;
  fragment : string;
}
(** The type of urls. The generic forms are:
    protocol://host/list/of/path/components
    protocol://host:port/list/of/path/components
    protocol://host/list/of/path/components?arguments
    protocol://host/list/of/path/components#fragment
    protocol://[host]:port/list/of/path/components

    Note that for [File] protocol there is no [host] component:
    file:///list/of/path/components/
  *)

val url_of_string : string -> url option
(** [url_of_string s] parses [s] and builds a value of type [url] if [s] is not
    a valid url string, it returns [None]. *)

val string_of_url : url -> string
(** [string_of_url u] returns a valid string representation of [u]. Note that
  * [string_of_url ((fun Some u -> u) (url_of_string s))] is NOT necessarily
  * equal to [s]. However [url_of_string (string_of_url u) = u]. *)

module Current :
(** This module can be used to handle the Url associated to the current
    document. *)

  sig

    val protocol : protocol
    val host : string
    val port : int option
    val path_string : string
    (** [path_string] is the concatenation of all the path components of the
        current Url. *)

    val path : string list
    (** [path] is the ["/"] split version of the [path_string] value. *)

    val arguments : (string * string) list

    val fragment : unit -> string
    (** Because the [fragment] of the Url for the current document can change
        dynamically, we use a functional value here. *)

    val set_fragment : string -> unit
    (** [set_fragment s] replaces the current fragment by [s]. *)

    val get : unit -> url
    (** [get ()] returns a value of type {!url} with fields reflecting the
        state of the current Url. *)

    val set : url -> unit
    (** [set u] replaces the current Url for [u]. {b WARNING:} Causes the document
        to change. *)

    val as_string : string
    (** [as_string] is the original string representation of the current Url. It
        is NOT necessarily equals to [string_of_url (get ())] but
        [url_of_string as_string = get ()] holds. *)


  end


