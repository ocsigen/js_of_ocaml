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

class type xmlHttpRequest = object ('self)
  method onreadystatechange : ('self Js.t, Dom_html.event Js.t) Dom_html.event_listener Js.writeonly_prop
  method readyState : readyState readonly_prop
  method _open :
    js_string t -> js_string t -> bool t -> unit meth
  method _open_full :
    js_string t -> js_string t -> bool t ->
    js_string t opt -> js_string t opt -> unit meth
  method setRequestHeader : js_string t -> js_string t -> unit meth
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
end

class type xmlHttpRequest_binary = object
  inherit xmlHttpRequest
  method sendAsBinary : js_string t opt -> unit meth
  method sendAsBinary_test : unit optdef readonly_prop
end

let xmlHttpRequest () : xmlHttpRequest t constr =
  Js.Unsafe.variable "XMLHttpRequest"

let activeXObject () : (js_string t -> xmlHttpRequest t) constr =
  Js.Unsafe.variable "ActiveXObject"

let create () =
  try jsnew (xmlHttpRequest ()) () with _ ->
  try jsnew (activeXObject ()) (Js.string "Msxml2.XMLHTTP") with _ ->
  try jsnew (activeXObject ()) (Js.string "Msxml3.XMLHTTP") with _ ->
  try jsnew (activeXObject ()) (Js.string "Microsoft.XMLHTTP") with _ ->
  assert false

let encode = Url.encode_arguments
let encode_url args =
  let args = List.map (fun (n,v) -> to_string n,to_string v) args in
    string (Url.encode_arguments args)

let generateBoundary () =
  Random.self_init ();
  "js_of_ocaml-------------------" ^ (string_of_int (Random.bits ())) ^ (string_of_int (Random.bits ()))

(* TODO: do it entirely with js_string t: more efficient *)
(* TODO: test with elements = [] *)
let encode_multipart boundary elements =
  let b = Buffer.create 0 in
  (Lwt_list.iter_s
     (fun v ->
       Buffer.add_string b ("--"^boundary^"\r\n");
       match v with
	 | name,`String value ->
	   Buffer.add_string b "Content-Disposition: form-data; name=\"";
	   Buffer.add_string b name;
	   Buffer.add_string b "\"\r\n\r\n";
	   Buffer.add_string b (to_string value);
	   Buffer.add_string b "\r\n";
	   return ()
	 | name,`File value ->
	   File.readAsBinaryString (value :> File.blob Js.t)
	   >>= (fun file ->
	     Buffer.add_string b "Content-Disposition: form-data; name=\"";
	     Buffer.add_string b name;
	     Buffer.add_string b "\"; filename=\"";
	     Buffer.add_string b (to_string (value##name));
	     Buffer.add_string b "\"\r\n";
	     Buffer.add_string b "Content-Type: application/octet-stream\r\n";
	     Buffer.add_string b "\r\n";
	     Buffer.add_string b (to_string file);
	     Buffer.add_string b "\r\n";
	     return ()
	   ))
     elements)
  >|=
      (fun () -> Buffer.add_string b ("--"^boundary^"--\r\n");
	(Buffer.contents b))

let encode_url l =
  String.concat "&"
    (List.map
       (function
	 | name,`String s -> ((Url.urlencode name) ^ "=" ^ (Url.urlencode (to_string s)))
	 | name,`File s -> ((Url.urlencode name) ^ "=" ^ (Url.urlencode (to_string (s##name))))
) l)

let partition_string_file l = List.partition (function
  | _,`String _ -> true
  | _,`File _ -> false ) l

(* Higher level interface: *)

(** type of the http headers *)
type http_frame =
    {
      code: int;
      headers: string -> string option;
      content: string;
    }

let send_field_string
    ?(headers = [])
    ?content_type
    ?(post_args:Form.form_contents option)
    ?(get_args=[])
    url =

  let method_, content_type, post_encode =
    match post_args, content_type with
      | None, ct -> "GET", ct, `Urlencode
      | Some post_args, None ->
	(match post_args with
	  | `Fields l ->
	    let strings,files = partition_string_file !l in
	    (match files with
	      | [] -> "POST", (Some "application/x-www-form-urlencoded"), `Urlencode
	      | _ ->
		let boundary = generateBoundary () in
		"POST", (Some ("multipart/form-data; boundary="^boundary)), `Form_data (boundary))
	  | `FormData f -> "POST", None, `Urlencode)
      | Some _, ct -> "POST", ct, `Urlencode
  in
  let url = match get_args with
    | [] -> url
    | _::_ as l -> url ^ "?" ^ encode l
  in

  let (res, w) = Lwt.task () in
  let req = create () in

  req##_open (Js.string method_, Js.string url, Js._true);
  (match content_type with
    | Some content_type ->
      req##setRequestHeader (Js.string "Content-type", Js.string content_type)
    | _ -> ());
  List.iter (fun (n, v) -> req##setRequestHeader (Js.string n, Js.string v))
    headers;
  req##onreadystatechange <- Dom_html.handler
    (fun _ ->
      if req##readyState = DONE then
        Lwt.wakeup w
          {code = req##status;
           content = Js.to_string req##responseText;
           headers =
              (fun s ->
                Opt.case
                  (req##getResponseHeader (Js.bytestring s))
                  (fun () -> None)
                  (fun v -> Some (Js.to_string v))
              )
              }
      else ();
      Js._false);

  (match post_args with
     | None -> req##send (Js.null)
     | Some (`Fields l) ->
       ignore (
	 match post_encode with
	   | `Urlencode -> req##send(Js.some (string (encode_url !l)));return ()
	   | `Form_data boundary ->
	     (encode_multipart boundary !l >|=
		 (fun data ->
		   let data = Js.some (string data) in
		   (* Firefox specific interface:
		      Chrome can use FormData: don't need this *)
		   let req = (Js.Unsafe.coerce req:xmlHttpRequest_binary t) in
		   if Optdef.test req##sendAsBinary_test
		   then req##sendAsBinary(data)
		   else req##send(data))))
     | Some (`FormData f) -> req##send_formData(f));

  Lwt.on_cancel res (fun () -> req##abort ()) ;
  res

let send_string
    ?(headers = [])
    ?content_type
    ?post_args
    ?(get_args=[])
    url =
  let post_args =
    match post_args with
      | None -> None
      | Some l -> Some (`Fields (ref (List.map (fun (n,v) -> n,`String (string v)) l)))
  in
  send_field_string ~headers ?content_type ?post_args ~get_args url

let send
    ?(headers = [])
    ?content_type
    ?post_args
    ?(get_args=[])
    url =
  send_string ~headers ?content_type ?post_args ~get_args
    (Url.string_of_url url)

let send_get_form_string
    ?(headers = [])
    ?content_type
    ?post_args
    ?(get_args=[])
    form
    url =
  let get_args = (Form.get_form_contents form)@get_args in
  send_string ~headers ?content_type ?post_args ~get_args url

let send_get_form
    ?(headers = [])
    ?content_type
    ?post_args
    ?(get_args=[])
    form
    url =
  send_get_form_string ~headers ?content_type ?post_args ~get_args form
    (Url.string_of_url url)

let send_post_form_string
    ?(headers = [])
    ?content_type
    ?(post_args=[])
    ?(get_args=[])
    form
    url =
  let contents = Form.post_form_contents form in
  List.iter (fun (name,value) -> Form.append contents (name,`String (string value))) post_args;
  send_field_string ~headers ?content_type ~post_args:contents ~get_args url
      

let send_post_form
    ?(headers = [])
    ?content_type
    ?(post_args=[])
    ?(get_args=[])
    form
    url =
  send_post_form_string ~headers ?content_type ~post_args ~get_args form
    (Url.string_of_url url)

