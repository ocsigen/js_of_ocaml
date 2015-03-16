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

open Lwt
open Js

type readyState = UNSENT | OPENED | HEADERS_RECEIVED | LOADING | DONE

type _ response =
    ArrayBuffer : Typed_array.arrayBuffer t Opt.t response
  | Blob : #File.blob t Opt.t response
  | Document : Dom.element Dom.document t Opt.t response
  | JSON : 'a Opt.t response
  | Text : js_string t response
  | Default: string response

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
  method response : File.file_any readonly_prop
  method responseText : js_string t readonly_prop
  method responseXML : Dom.element Dom.document t opt readonly_prop
  method responseType : js_string t prop

  inherit File.progressEventTarget
  method ontimeout : ('self t, 'self File.progressEvent t) Dom.event_listener writeonly_prop
  method upload : xmlHttpRequestUpload t optdef readonly_prop
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

class type xmlHttpRequest_binary = object
  inherit xmlHttpRequest
  method sendAsBinary : js_string t opt -> unit meth
  method sendAsBinary_presence : unit optdef readonly_prop
end

let create () : xmlHttpRequest Js.t =
  try jsnew (Js.Unsafe.global##_XMLHttpRequest)() with _ ->
  try jsnew (Js.Unsafe.global##activeXObject)(Js.string "Msxml2.XMLHTTP") with _ ->
  try jsnew (Js.Unsafe.global##activeXObject)(Js.string "Msxml3.XMLHTTP") with _ ->
  try jsnew (Js.Unsafe.global##activeXObject)(Js.string "Microsoft.XMLHTTP") with _ ->
  assert false

let encode = Url.encode_arguments
let encode_url args =
  let args = List.map (fun (n,v) -> to_string n,to_string v) args in
    string (Url.encode_arguments args)

let generateBoundary () =
  let nine_digits () =
    string_of_int (truncate (Js.math##random() *. 1000000000.))
  in
  "js_of_ocaml-------------------" ^ nine_digits () ^ nine_digits ()

(* TODO: test with elements = [] *)
let encode_multipart boundary elements =
  let b = jsnew array_empty () in
  (Lwt_list.iter_s
     (fun v ->
       ignore(b##push(Js.string ("--"^boundary^"\r\n")));
       match v with
	 | name, `String value ->
	   ignore(b##push_3(Js.string ("Content-Disposition: form-data; name=\"" ^ name ^ "\"\r\n\r\n"),
			    value,
			    Js.string "\r\n"));
	   return ()
	 | name, `File value ->
	   File.readAsBinaryString (value :> File.blob Js.t)
	   >>= (fun file ->
	     ignore(b##push_4(Js.string ("Content-Disposition: form-data; name=\"" ^ name ^ "\"; filename=\""),
			      File.filename value,
			      Js.string "\"\r\n",
			      Js.string "Content-Type: application/octet-stream\r\n"));
	     ignore(b##push_3(Js.string "\r\n",
			      file,
			      Js.string "\r\n"));
	     return ())
     )
     elements)
  >|= ( fun () -> ignore(b##push(Js.string ("--"^boundary^"--\r\n"))); b )

let encode_url l =
  String.concat "&"
    (List.map
       (function
	 | name,`String s -> ((Url.urlencode name) ^ "=" ^ (Url.urlencode (to_string s)))
	 | name,`File s -> ((Url.urlencode name) ^ "=" ^ (Url.urlencode (to_string (s##name))))
) l)

(* Higher level interface: *)

(** type of the http headers *)
type 'response http_frame =
    {
      url: string;
      code: int;
      headers: string -> string option;
      content: 'response;
      content_xml: unit -> Dom.element Dom.document t option;
    }

exception Wrong_headers of (int * (string -> string option))

let extract_get_param url =
  let open Url in
  match url_of_string url with
    | Some (Http url) ->
      Url.string_of_url (Http { url with hu_arguments = [] }),
      url.hu_arguments
    | Some (Https url) ->
      Url.string_of_url (Https { url with hu_arguments = [] }),
      url.hu_arguments
    | _ -> url, []

let default_response url code headers req =
  { url = url;
    code = code;
    content = Js.to_string req##responseText;
    content_xml =
      (fun () ->
	match Js.Opt.to_option (req##responseXML) with
	  | None -> None
	  | Some doc ->
	    if (Js.some doc##documentElement) == Js.null
	    then None
	    else Some doc);
    headers = headers
  }

let text_response url code headers req =
  { url = url;
    code = code;
    content = req##responseText;
    content_xml = (fun () -> assert false);
    headers = headers
  }

let document_response url code headers req =
  { url = url;
    code = code;
    content = File.CoerceTo.document (req##response);
    content_xml = (fun () -> assert false);
    headers = headers
  }

let json_response url code headers req =
  { url = url;
    code = code;
    content = File.CoerceTo.json (req##response);
    content_xml = (fun () -> assert false);
    headers = headers
  }

let blob_response url code headers req =
  { url = url;
    code = code;
    content = File.CoerceTo.blob (req##response);
    content_xml = (fun () -> assert false);
    headers = headers
  }

let arraybuffer_response url code headers req =
  { url = url;
    code = code;
    content = File.CoerceTo.arrayBuffer (req##response);
    content_xml = (fun () -> assert false);
    headers = headers
  }

let perform_raw
    ?(headers = [])
    ?content_type
    ?(post_args:(string * Form.form_elt) list option)
    ?(get_args=[])
    ?(form_arg:Form.form_contents option)
    ?(check_headers=(fun _ _ -> true))
    ?progress
    ?upload_progress
    ?override_mime_type
    (type resptype) ~(response_type:resptype response)
    url =

  let form_arg =
    match form_arg with
      | None ->
	(match post_args with
	  | None -> None
	  | Some post_args ->
            let only_strings =
              List.for_all
                (fun x -> match x with (_, `String _) ->  true | _ -> false)
                post_args
            in
	    let contents =
              if only_strings then `Fields (ref []) else
              Form.empty_form_contents ()
            in
	    List.iter (fun (name, value) ->
              Form.append contents (name, value))
              post_args;
	    Some contents)
      | Some form_arg ->
	(match post_args with
	  | None -> ()
	  | Some post_args ->
	    List.iter (fun (name, value) ->
              Form.append form_arg (name, value))
              post_args);
	Some form_arg
  in

  let method_, content_type =
    match form_arg, content_type with
      | None, ct -> "GET", ct
      | Some form_args, None ->
	(match form_args with
	  | `Fields strings ->
               "POST", Some "application/x-www-form-urlencoded"
	  | `FormData f -> "POST", None)
      | Some _, ct -> "POST", ct
  in
  let url, url_get = extract_get_param url in
  let url = match url_get@get_args with
    | [] -> url
    | _::_ as l -> url ^ "?" ^ encode l
  in

  let ((res : resptype http_frame Lwt.t), w) = Lwt.task () in
  let req = create () in

  begin match override_mime_type with
    None           -> ()
  | Some mime_type -> req ## overrideMimeType (Js.string mime_type)
  end;

  begin match response_type with
  | ArrayBuffer -> req ## responseType <- (Js.string "arraybuffer")
  | Blob        -> req ## responseType <- (Js.string "blob")
  | Document    -> req ## responseType <- (Js.string "document")
  | JSON        -> req ## responseType <- (Js.string "json")
  | Text        -> req ## responseType <- (Js.string "text")
  | Default     -> req ## responseType <- (Js.string "")
  end;

  req##_open (Js.string method_, Js.string url, Js._true);
  (match content_type with
    | Some content_type ->
      req##setRequestHeader (Js.string "Content-type", Js.string content_type)
    | _ -> ());
  List.iter (fun (n, v) -> req##setRequestHeader (Js.string n, Js.string v))
    headers;
  let headers s =
    Opt.case
      (req##getResponseHeader (Js.bytestring s))
      (fun () -> None)
      (fun v -> Some (Js.to_string v))
  in
  let do_check_headers =
    let checked = ref false in
    fun () ->
      if not (!checked) && not (check_headers (req##status) headers)
      then begin
        Lwt.wakeup_exn w (Wrong_headers ((req##status),headers));
        req##abort ();
      end;
      checked := true
  in
  req##onreadystatechange <- Js.wrap_callback
    (fun _ ->
       (match req##readyState with
          (* IE doesn't have the same semantics for HEADERS_RECEIVED.
             so we wait til LOADING to check headers. See:
             http://msdn.microsoft.com/en-us/library/ms534361(v=vs.85).aspx *)
        | HEADERS_RECEIVED when not Dom_html.onIE -> do_check_headers ()
	| LOADING when Dom_html.onIE -> do_check_headers ()
	| DONE ->
          (* If we didn't catch a previous event, we check the header. *)
          do_check_headers ();
	  let response : resptype http_frame =
	    match response_type with
	      ArrayBuffer -> arraybuffer_response url (req##status) headers req
	    | Blob -> blob_response url (req##status) headers req
	    | Document -> document_response url (req##status) headers req
	    | JSON -> json_response url (req##status) headers req
	    | Text -> text_response url (req##status) headers req
	    | Default -> default_response url (req##status) headers req in
	  Lwt.wakeup w response
	| _ -> ()));

  begin match progress with
  | Some progress ->
    req##onprogress <- Dom.handler
      (fun e ->
        progress e##loaded e##total;
        Js._true)
  | None -> ()
  end;
  Optdef.iter (req##upload) (fun upload ->
    match upload_progress with
    | Some upload_progress ->
      upload##onprogress <- Dom.handler
        (fun e ->
          upload_progress e##loaded e##total;
          Js._true)
    | None -> ()
  );

  (match form_arg with
     | None -> req##send (Js.null)
     | Some (`Fields l) ->
         ignore (req##send(Js.some (string (encode_url !l)));return ())
     | Some (`FormData f) -> req##send_formData(f));

  Lwt.on_cancel res (fun () -> req##abort ()) ;
  res

let perform_raw_url
    ?(headers = [])
    ?content_type
    ?post_args
    ?(get_args=[])
    ?form_arg
    ?check_headers
    ?progress
    ?upload_progress
    ?override_mime_type
    url =
  perform_raw ~headers ?content_type ?post_args ~get_args ?form_arg
    ?check_headers ?progress ?upload_progress ?override_mime_type
    ~response_type:Default url

let perform
    ?(headers = [])
    ?content_type
    ?post_args
    ?(get_args=[])
    ?form_arg
    ?check_headers
    ?progress
    ?upload_progress
    ?override_mime_type
    url =
  perform_raw ~headers ?content_type ?post_args ~get_args ?form_arg
    ?check_headers ?progress ?upload_progress ?override_mime_type
    ~response_type:Default (Url.string_of_url url)

let get s = perform_raw_url s
