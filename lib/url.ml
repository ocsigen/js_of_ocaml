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

(* Url tampering. *)

let l = Dom_html.window##location

let split c s =
  Js.str_array (s##split (Js.string (String.make 1 c)))

exception Local_exn
let interrupt () = raise Local_exn


(* url (AKA percent) encoding/decoding *)

let plus_re = jsnew Js.regExp_withFlags (Js.string "\\+", Js.string "g")
let escape_plus_js s = s##replace (plus_re, Js.string "%2B")

let urldecode_js_string_string s =
  Js.to_bytestring (Js.unescape s)
let urldecode s =
  Js.to_bytestring (Js.unescape (Js.bytestring s))

let urlencode_string_js_string ?(with_plus=true) s =
  let pre = Js.escape (Js.bytestring s) in
  if with_plus
  then escape_plus_js pre
  else pre
let urlencode_js_string_string s =
  Js.to_bytestring (Js.escape s)
let urlencode ?(with_plus=true) s =
  Js.to_bytestring (
    if with_plus
    then escape_plus_js (Js.escape (Js.bytestring s))
    else Js.escape (Js.bytestring s)
  )


(* protocol *)
type protocol =
  | Http
  | Https
  | File
  | Exotic of string

let protocol_of_string s = match String.lowercase s with
  | "http:"  | "http"  -> Http
  | "https:" | "https" -> Https
  | "file:"  | "file"  -> File
  | s -> Exotic s
let string_of_protocol ?(comma=false) p = match p with
  | Http  -> if comma then "http:" else "http"
  | Https -> if comma then "https:" else "https"
  | File  -> if comma then "file:" else "file"
  | Exotic s -> s



(* port number *)
let default_http_port = 80
let default_https_port = 443
let port_of_string prot = function
  | "" -> begin
      match prot with
      | Http  -> Some default_http_port
      | Https -> Some default_https_port
      | File | Exotic _ -> None
    end
  | s ->
     try Some (int_of_string s)
     with Invalid_argument _ -> None
let string_of_port = string_of_int
let string_of_port_option prot = function
  | Some i -> Some (string_of_port i)
  | None -> match prot with
      | Http -> Some (string_of_port default_http_port)
      | Https -> Some (string_of_port default_https_port)
      | File | Exotic _ -> None



(* path *)
let rec path_of_path_string s = (* inspired from: Ocsigen_lib *)
  try
    let length = String.length s in
    if length = 0
    then []
    else
      let pos_slash = String.index s '/' in
      if pos_slash = 0
      then "" :: path_of_path_string (String.sub s 1 (length-1))
      else (   String.sub s 0 pos_slash
            :: (path_of_path_string
                  (String.sub s (pos_slash+1) (length - pos_slash - 1))
               )
           )
  with Not_found -> [s]


(* Arguments *)
let encode_arguments l =
  String.concat "&"
    (List.map
       (fun (n, v) -> urlencode n ^"="^ urlencode v)
       l
    )

let decode_arguments_js_string s =
  let arr = split '&' l##search in
  let len = arr##length in
  let name_value_split s =
    let arr_bis = split '=' s in
    match arr_bis##length with
      | 3 -> Js.def (Js.array_get arr_bis 1, Js.array_get arr_bis 2)
      | _ -> Js.undefined
  in
  let rec aux acc idx =
    if idx < 1
    then acc
    else try aux (Js.Optdef.case
                    (Js.array_get arr idx)
                    interrupt
                    (fun s -> Js.Optdef.case (name_value_split s)
                                interrupt
                                (fun (x, y) ->
                                  let get t =
                                    urldecode_js_string_string
                                      (Js.Optdef.get t interrupt)
                                  in
                                  (get x, get y)
                                )
                    )
                  :: acc)
               (pred idx)
         with Local_exn -> aux acc (pred idx)
  in
    aux [] len

let decode_arguments s =
  decode_arguments_js_string (Js.bytestring s)

type url = {
  protocol    : protocol ;
  host        : string ;
  port        : int option ;
  path        : string list ;
  path_string : string ;
  arguments   : (string * string) list ;
  fragment    : string
}

let url_re =
  jsnew Js.regExp (Js.bytestring "^([Hh][Tt][Tt][Pp][Ss]?)://\
                                   ([0-9a-zA-Z.-]+|\\[[0-9A-Fa-f:.]+\\])?\
                                   (:([0-9]+))?\
                                   /([^\\?#]*)\
                                   (\\?([^#])*)?\
                                   (#(.*))?$"
                  )
let file_re =
  jsnew Js.regExp (Js.bytestring "^([Ff][Ii][Ll][Ee])://\
                                   ([^\\?#]*)\
                                   (\\?([^#])*)?\
                                   (#(.*))?$"
                  )

let url_of_js_string s =
  try
  Js.Opt.case (url_re##exec (s))
    (fun () -> Js.Opt.case (file_re##exec (s))
       interrupt
       (fun handle ->
          let res = Js.match_result handle in
          let path_str =
            urldecode_js_string_string
              (Js.Optdef.get (Js.array_get res 2) interrupt)
          in
          Some {
            protocol    = File ;
            host        = "" ;
            port        = None ;
            path        = path_of_path_string path_str ;
            path_string = path_str ;
            arguments   = decode_arguments_js_string
                            (Js.Optdef.get
                               (Js.array_get res 4)
                               (fun () -> Js.bytestring "")
                            );
            fragment    = Js.to_bytestring
                            (Js.Optdef.get
                              (Js.array_get res 6)
                              (fun () -> Js.bytestring "")
                            );
          }
       )
    )
    (fun handle ->
       let res = Js.match_result handle in
       let prot =
         protocol_of_string
           (Js.to_bytestring
              (Js.Optdef.get (Js.array_get res 1) interrupt)
           )
       in
       let path_str =
         urldecode_js_string_string
           (Js.Optdef.get (Js.array_get res 4) interrupt)
       in
       Some {
         protocol    = prot ;
         host        = urldecode_js_string_string
                         (Js.Optdef.get (Js.array_get res 2) interrupt)
                         ;
         port        = port_of_string prot
                        (Js.to_bytestring
                           (Js.Optdef.get
                              (Js.array_get res 3)
                              (fun () -> Js.bytestring "")
                           )
                        ) ;
         path        = path_of_path_string path_str ;
         path_string = path_str ;
         arguments   = decode_arguments_js_string
                         (Js.Optdef.get
                            (Js.array_get res 7)
                            (fun () -> Js.bytestring "")
                         )
                         ;
         fragment    = urldecode_js_string_string
                         (Js.Optdef.get
                            (Js.array_get res 9)
                            (fun () -> Js.bytestring "")
                         )
    ;
       }
    )
  with Local_exn -> None

let url_of_string s = url_of_js_string (Js.bytestring s)
let string_of_url u =
  (string_of_protocol u.protocol)
  ^ "://"
  ^ (match u.host with
       | "" -> "" (* When protocol is File *)
       | s -> "[" ^ urlencode s ^ "]"
    )
  ^ (match u.port, u.protocol with
     (*| Some 80, Http | Some 443, Https -> ""*)
       | Some _, File -> "" (*TODO: change url type *)
       | None, File -> ""
       | None, _ -> "" (*TODO: change url type *)
       | Some n, _ -> ":" ^ string_of_int n
    )
  ^ String.concat "/" (List.map urlencode u.path)
  ^ (match u.arguments with
       | [] -> ""
       | l -> "?" ^ encode_arguments l
    )
  ^ (match u.fragment with
       | "" -> ""
       | s -> "#" ^ (urlencode s)
    )

module Current =
struct

  let protocol = protocol_of_string (Js.to_bytestring l##protocol)

  let host = urldecode_js_string_string l##hostname

  let port = port_of_string protocol (Js.to_bytestring l##port)

  let path_string = urldecode_js_string_string l##pathname

  let path = path_of_path_string path_string

  let arguments = decode_arguments_js_string l##search

  let fragment () =
    let s = Js.to_bytestring l##hash in
    if String.length s > 0 && s.[0] = '#'
    then String.sub s 1 (String.length s - 1)
    else s
    (*TODO: switch behavior depending on the browser (Firefox bug : https://bugzilla.mozilla.org/show_bug.cgi?id=483304 )*)

  let set_fragment s = l##hash <- urlencode_string_js_string s

  let get () = {
    protocol    = protocol ;
    host        = host ;
    port        = port ;
    path        = path ;
    path_string = path_string ;
    arguments   = arguments ;
    fragment    = fragment () ;
  }

  let set u = l##href <- (Js.bytestring (string_of_url u))

  let as_string = urldecode_js_string_string l##href

end

