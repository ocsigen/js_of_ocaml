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
open! Import

(* Url tampering. *)

let split c s = Js.str_array (s##split (Js.string (String.make 1 c)))

let split_2 c s =
  let index = s##indexOf (Js.string (String.make 1 c)) in
  if index < 0 then Js.undefined else Js.def (s##slice 0 index, s##slice_end (index + 1))

exception Local_exn

let interrupt () = raise Local_exn

(* url (AKA percent) encoding/decoding *)

let plus_re = Regexp.regexp_string "+"

let escape_plus s = Regexp.global_replace plus_re s "%2B"

let unescape_plus s = Regexp.global_replace plus_re s " "

let plus_re_js_string = new%js Js.regExp_withFlags (Js.string "\\+") (Js.string "g")

let unescape_plus_js_string s =
  plus_re_js_string##.lastIndex := 0;
  s##replace plus_re_js_string (Js.string " ")

let urldecode_js_string_string s =
  Js.to_bytestring (Js.unescape (unescape_plus_js_string s))

let urldecode s = Js.to_bytestring (Js.unescape (Js.bytestring (unescape_plus s)))

(*let urlencode_js_string_string s =
  Js.to_bytestring (Js.escape s)*)

let urlencode ?(with_plus = true) s =
  if with_plus
  then escape_plus (Js.to_bytestring (Js.escape (Js.bytestring s)))
  else Js.to_bytestring (Js.escape (Js.bytestring s))

type http_url =
  { hu_host : string  (** The host part of the url. *)
  ; hu_port : int  (** The port for the connection if any. *)
  ; hu_path : string list  (** The path split on ['/'] characters. *)
  ; hu_path_string : string  (** The original entire path. *)
  ; hu_arguments : (string * string) list
        (** Arguments as a field-value association list.*)
  ; hu_fragment : string  (** The fragment part (after the ['#'] character). *)
  }
(** The type for HTTP url. *)

type file_url =
  { fu_path : string list
  ; fu_path_string : string
  ; fu_arguments : (string * string) list
  ; fu_fragment : string
  }
(** The type for local file urls. *)

type url =
  | Http of http_url
  | Https of http_url
  | File of file_url
      (** The type for urls. [File] is for local files and [Exotic s] is for
          unknown/unsupported protocols. *)

exception Not_an_http_protocol

let is_secure prot_string =
  match Js.to_bytestring prot_string##toLowerCase with
  | "https:" | "https" -> true
  | "http:" | "http" -> false
  | "file:" | "file" | _ -> raise Not_an_http_protocol

(* port number *)
let default_http_port = 80

let default_https_port = 443

(* path *)
let path_of_path_string s =
  let l = String.length s in
  let rec aux i =
    let j = try String.index_from s i '/' with Not_found -> l in
    let word = String.sub s i (j - i) in
    if j >= l then [ word ] else word :: aux (j + 1)
  in
  match aux 0 with
  | [ "" ] -> []
  | [ ""; "" ] -> [ "" ]
  | a -> a

(* Arguments *)
let encode_arguments l =
  String.concat "&" (List.map (fun (n, v) -> urlencode n ^ "=" ^ urlencode v) l)

let decode_arguments_js_string s =
  let arr = split '&' s in
  let len = arr##.length in
  let name_value_split s = split_2 '=' s in
  let rec aux acc idx =
    if idx < 0
    then acc
    else
      try
        aux
          (Js.Optdef.case (Js.array_get arr idx) interrupt (fun s ->
               Js.Optdef.case (name_value_split s) interrupt (fun (x, y) ->
                   let get = urldecode_js_string_string in
                   get x, get y))
          :: acc)
          (pred idx)
      with Local_exn -> aux acc (pred idx)
  in
  aux [] (len - 1)

let decode_arguments s = decode_arguments_js_string (Js.bytestring s)

let url_re =
  new%js Js.regExp
    (Js.bytestring
       "^([Hh][Tt][Tt][Pp][Ss]?)://([0-9a-zA-Z.-]+|\\[[0-9a-zA-Z.-]+\\]|\\[[0-9A-Fa-f:.]+\\])?(:([0-9]+))?(/([^\\?#]*)(\\?([^#]*))?(#(.*))?)?$")

let file_re =
  new%js Js.regExp
    (Js.bytestring "^([Ff][Ii][Ll][Ee])://([^\\?#]*)(\\?([^#]*))?(#(.*))?$")

let url_of_js_string s =
  Js.Opt.case
    (url_re##exec s)
    (fun () ->
      Js.Opt.case
        (file_re##exec s)
        (fun () -> None)
        (fun handle ->
          let res = Js.match_result handle in
          let path_str =
            urldecode_js_string_string (Js.Optdef.get (Js.array_get res 2) interrupt)
          in
          Some
            (File
               { fu_path = path_of_path_string path_str
               ; fu_path_string = path_str
               ; fu_arguments =
                   decode_arguments_js_string
                     (Js.Optdef.get (Js.array_get res 4) (fun () -> Js.bytestring ""))
               ; fu_fragment =
                   Js.to_bytestring
                     (Js.Optdef.get (Js.array_get res 6) (fun () -> Js.bytestring ""))
               })))
    (fun handle ->
      let res = Js.match_result handle in
      let ssl = is_secure (Js.Optdef.get (Js.array_get res 1) interrupt) in
      let port_of_string = function
        | "" -> if ssl then 443 else 80
        | s -> int_of_string s
      in
      let path_str =
        urldecode_js_string_string
          (Js.Optdef.get (Js.array_get res 6) (fun () -> Js.bytestring ""))
      in
      let url =
        { hu_host =
            urldecode_js_string_string (Js.Optdef.get (Js.array_get res 2) interrupt)
        ; hu_port =
            port_of_string
              (Js.to_bytestring
                 (Js.Optdef.get (Js.array_get res 4) (fun () -> Js.bytestring "")))
        ; hu_path = path_of_path_string path_str
        ; hu_path_string = path_str
        ; hu_arguments =
            decode_arguments_js_string
              (Js.Optdef.get (Js.array_get res 8) (fun () -> Js.bytestring ""))
        ; hu_fragment =
            urldecode_js_string_string
              (Js.Optdef.get (Js.array_get res 10) (fun () -> Js.bytestring ""))
        }
      in
      Some (if ssl then Https url else Http url))

let url_of_string s = url_of_js_string (Js.bytestring s)

let string_of_url = function
  | File { fu_path = path; fu_arguments = args; fu_fragment = frag; _ } -> (
      "file://"
      ^ String.concat "/" (List.map (fun x -> urlencode x) path)
      ^ (match args with
        | [] -> ""
        | l -> "?" ^ encode_arguments l)
      ^
      match frag with
      | "" -> ""
      | s -> "#" ^ urlencode s)
  | Http
      { hu_host = host
      ; hu_port = port
      ; hu_path = path
      ; hu_arguments = args
      ; hu_fragment = frag
      ; _
      } -> (
      "http://"
      ^ urlencode host
      ^ (match port with
        | 80 -> ""
        | n -> ":" ^ string_of_int n)
      ^ "/"
      ^ String.concat "/" (List.map (fun x -> urlencode x) path)
      ^ (match args with
        | [] -> ""
        | l -> "?" ^ encode_arguments l)
      ^
      match frag with
      | "" -> ""
      | s -> "#" ^ urlencode s)
  | Https
      { hu_host = host
      ; hu_port = port
      ; hu_path = path
      ; hu_arguments = args
      ; hu_fragment = frag
      ; _
      } -> (
      "https://"
      ^ urlencode host
      ^ (match port with
        | 443 -> ""
        | n -> ":" ^ string_of_int n)
      ^ "/"
      ^ String.concat "/" (List.map (fun x -> urlencode x) path)
      ^ (match args with
        | [] -> ""
        | l -> "?" ^ encode_arguments l)
      ^
      match frag with
      | "" -> ""
      | s -> "#" ^ urlencode s)

module Current = struct
  let l =
    if Js.Optdef.test (Js.Optdef.return Dom_html.window##.location)
    then Dom_html.window##.location
    else
      let empty = Js.string "" in
      object%js
        val mutable href = empty

        val mutable protocol = empty

        val mutable host = empty

        val mutable hostname = empty

        val mutable port = empty

        val mutable pathname = empty

        val mutable search = empty

        val mutable hash = empty

        val origin = Js.undefined

        method reload = ()

        method replace _ = ()

        method assign _ = ()
      end

  let host = urldecode_js_string_string l##.hostname

  let protocol = urldecode_js_string_string l##.protocol

  let port =
    (fun () ->
      try Some (int_of_string (Js.to_bytestring l##.port)) with Failure _ -> None)
      ()

  let path_string = urldecode_js_string_string l##.pathname

  let path = path_of_path_string path_string

  let arguments =
    decode_arguments_js_string
      (if Js.equals (l##.search##charAt 0) (Js.string "?")
       then l##.search##slice_end 1
       else l##.search)

  let get_fragment () =
    let s = Js.to_bytestring l##.hash in
    if String.length s > 0 && Char.equal s.[0] '#'
    then String.sub s 1 (String.length s - 1)
    else s

  let set_fragment s = l##.hash := Js.bytestring s

  let get () = url_of_js_string l##.href

  let set u = l##.href := Js.bytestring (string_of_url u)

  let as_string = urldecode_js_string_string l##.href
end
