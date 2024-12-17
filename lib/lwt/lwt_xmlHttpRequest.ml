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

open Js_of_ocaml
open Js
open XmlHttpRequest
open! Import

let encode_url l =
  String.concat
    "&"
    (List.map
       (function
         | name, `String s -> Url.urlencode name ^ "=" ^ Url.urlencode (to_string s)
         | name, `File s -> Url.urlencode name ^ "=" ^ Url.urlencode (to_string s##.name))
       l)

(* Higher level interface: *)

type 'response generic_http_frame =
  { url : string
  ; code : int
  ; headers : string -> string option
  ; content : 'response
  ; content_xml : unit -> Dom.element Dom.document t option
  }
(** type of the http headers *)

type http_frame = string generic_http_frame

exception Wrong_headers of (int * (string -> string option))

let default_response url code headers req =
  { url
  ; code
  ; content = Js.Opt.case req##.responseText (fun () -> "") (fun x -> Js.to_string x)
  ; content_xml =
      (fun () ->
        match Js.Opt.to_option req##.responseXML with
        | None -> None
        | Some doc -> if Js.some doc##.documentElement == Js.null then None else Some doc)
  ; headers
  }

let text_response url code headers req =
  { url
  ; code
  ; content = Js.Opt.case req##.responseText (fun () -> Js.string "") (fun x -> x)
  ; content_xml = (fun () -> assert false)
  ; headers
  }

let document_response url code headers req =
  { url
  ; code
  ; content = File.CoerceTo.document req##.response
  ; content_xml = (fun () -> assert false)
  ; headers
  }

let json_response url code headers req =
  { url
  ; code
  ; content = File.CoerceTo.json req##.response
  ; content_xml = (fun () -> assert false)
  ; headers
  }

let blob_response url code headers req =
  { url
  ; code
  ; content = File.CoerceTo.blob req##.response
  ; content_xml = (fun () -> assert false)
  ; headers
  }

let arraybuffer_response url code headers req =
  { url
  ; code
  ; content = File.CoerceTo.arrayBuffer req##.response
  ; content_xml = (fun () -> assert false)
  ; headers
  }

let has_get_args url =
  try
    ignore (String.index url '?');
    true
  with Not_found -> false

let perform_raw
    ?(headers = [])
    ?content_type
    ?(get_args = [])
    ?(check_headers = fun _ _ -> true)
    ?progress
    ?upload_progress
    ?contents
    ?override_mime_type
    ?override_method
    ?with_credentials
    (type resptype)
    ~(response_type : resptype response)
    url =
  let contents_normalization = function
    | `POST_form args ->
        let only_strings =
          List.for_all
            (fun x ->
              match x with
              | _, `String _ -> true
              | _ -> false)
            args
        in
        let form_contents =
          if only_strings then `Fields (ref []) else Form.empty_form_contents ()
        in
        List.iter (fun (name, value) -> Form.append form_contents (name, value)) args;
        `Form_contents form_contents
    | (`String _ | `Form_contents _) as x -> x
    | `Blob b -> `Blob (b : #File.blob Js.t :> File.blob Js.t)
  in
  let contents =
    match contents with
    | None -> None
    | Some c -> Some (contents_normalization c)
  in
  let method_to_string m =
    match m with
    | `GET -> "GET"
    | `POST -> "POST"
    | `HEAD -> "HEAD"
    | `PUT -> "PUT"
    | `DELETE -> "DELETE"
    | `OPTIONS -> "OPTIONS"
    | `PATCH -> "PATCH"
  in
  let method_, content_type =
    let override_method m =
      match override_method with
      | None -> m
      | Some v -> method_to_string v
    in
    let override_content_type c =
      match content_type with
      | None -> Some c
      | Some _ -> content_type
    in
    match contents with
    | None -> override_method "GET", content_type
    | Some (`Form_contents form) -> (
        match form with
        | `Fields _strings ->
            ( override_method "POST"
            , override_content_type "application/x-www-form-urlencoded" )
        | `FormData _ -> override_method "POST", content_type)
    | Some (`String _ | `Blob _) -> override_method "POST", content_type
  in
  let url =
    if Poly.(get_args = [])
    then url
    else url ^ (if has_get_args url then "&" else "?") ^ Url.encode_arguments get_args
  in
  let (res : resptype generic_http_frame Lwt.t), w = Lwt.task () in
  let req = create () in
  req##_open (Js.string method_) (Js.string url) Js._true;
  (match override_mime_type with
  | None -> ()
  | Some mime_type -> req##overrideMimeType (Js.string mime_type));
  (match response_type with
  | ArrayBuffer -> req##.responseType := Js.string "arraybuffer"
  | Blob -> req##.responseType := Js.string "blob"
  | Document -> req##.responseType := Js.string "document"
  | JSON -> req##.responseType := Js.string "json"
  | Text -> req##.responseType := Js.string "text"
  | Default -> req##.responseType := Js.string "");
  (match with_credentials with
  | Some c -> req##.withCredentials := Js.bool c
  | None -> ());
  (match content_type with
  | Some content_type ->
      req##setRequestHeader (Js.string "Content-type") (Js.string content_type)
  | _ -> ());
  List.iter (fun (n, v) -> req##setRequestHeader (Js.string n) (Js.string v)) headers;
  let headers s =
    Opt.case
      (req##getResponseHeader (Js.bytestring s))
      (fun () -> None)
      (fun v -> Some (Js.to_string v))
  in
  let do_check_headers =
    let st = ref `Not_yet in
    fun () ->
      if Poly.(!st = `Not_yet)
      then
        if check_headers req##.status headers
        then st := `Passed
        else (
          Lwt.wakeup_exn w (Wrong_headers (req##.status, headers));
          st := `Failed;
          req##abort);
      Poly.(!st <> `Failed)
  in
  req##.onreadystatechange :=
    Js.wrap_callback (fun _ ->
        match req##.readyState with
        | HEADERS_RECEIVED -> ignore (do_check_headers ())
        | DONE ->
            (* If we didn't catch a previous event, we check the header. *)
            if do_check_headers ()
            then
              let response : resptype generic_http_frame =
                match response_type with
                | ArrayBuffer -> arraybuffer_response url req##.status headers req
                | Blob -> blob_response url req##.status headers req
                | Document -> document_response url req##.status headers req
                | JSON -> json_response url req##.status headers req
                | Text -> text_response url req##.status headers req
                | Default -> default_response url req##.status headers req
              in
              Lwt.wakeup w response
        | _ -> ());
  (match progress with
  | Some progress ->
      req##.onprogress :=
        Dom.handler (fun e ->
            progress e##.loaded e##.total;
            Js._true)
  | None -> ());
  (match upload_progress with
  | Some upload_progress ->
      req##.upload##.onprogress
      := Dom.handler (fun e ->
             upload_progress e##.loaded e##.total;
             Js._true)
  | None -> ());
  (match contents with
  | None -> req##send Js.null
  | Some (`Form_contents (`Fields l)) -> req##send (Js.some (string (encode_url !l)))
  | Some (`Form_contents (`FormData f)) -> req##send_formData f
  | Some (`String s) -> req##send (Js.some (Js.string s))
  | Some (`Blob b) -> req##send_blob b);
  Lwt.on_cancel res (fun () -> req##abort);
  res

let perform_raw_url
    ?(headers = [])
    ?content_type
    ?(get_args = [])
    ?check_headers
    ?progress
    ?upload_progress
    ?contents
    ?override_mime_type
    ?override_method
    ?with_credentials
    url =
  perform_raw
    ~headers
    ?content_type
    ~get_args
    ?contents
    ?check_headers
    ?progress
    ?upload_progress
    ?override_mime_type
    ?override_method
    ?with_credentials
    ~response_type:Default
    url

let perform
    ?(headers = [])
    ?content_type
    ?(get_args = [])
    ?check_headers
    ?progress
    ?upload_progress
    ?contents
    ?override_mime_type
    ?override_method
    ?with_credentials
    url =
  perform_raw
    ~headers
    ?content_type
    ~get_args
    ?contents
    ?check_headers
    ?progress
    ?upload_progress
    ?override_mime_type
    ?override_method
    ?with_credentials
    ~response_type:Default
    (Url.string_of_url url)

let get s = perform_raw_url s
