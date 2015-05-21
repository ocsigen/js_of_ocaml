(* Js_of_ocaml library
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2014 Hugo Heuzard
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

let rec random_identifier size =
  let b = Buffer.create size in
  for _i = 0 to (size - 1) do
    Buffer.add_char b (Char.chr (97 + Random.int 26))
  done;
  let s = Buffer.contents b in
  if Js.Optdef.test (Js.Unsafe.get (Dom_html.window) (Js.string s))
  then (* name already in use, choose another one*)
    random_identifier size
  else
    s

let raw_call name uri error_cb user_cb =
  let script = Dom_html.(createScript document) in
  let finalize () =
    Js.Unsafe.delete (Dom_html.window) (Js.string name);
    Js.Opt.iter (script##parentNode) (fun parent -> Dom.removeChild parent script) in
  let executed = ref false in
  Js.Unsafe.set
    (Dom_html.window)
    (Js.string name)
    (fun x -> executed:=true; finalize (); user_cb x);
  script##src <- Js.string uri;
  script##_type <- Js.string ("text/javascript");
  script##async <- Js._true;
  (Js.Unsafe.coerce script)##onerror <- (fun x -> finalize (); error_cb x);
  (Js.Unsafe.coerce script)##onload <- (fun x ->
      Lwt.async (fun () ->
          Lwt.bind (Lwt_js.sleep 1.) (fun () ->
              if !executed
              then Lwt.return_unit
              else (
                Firebug.console##warn(Js.string "Jsonp: script loaded but callback not executed");
                finalize (); error_cb x; Lwt.return_unit))
        )
    );
  let init () = ignore (Dom.appendChild (Dom_html.document##body) script) in
  init, finalize

let call_ make_uri error_cb user_cb =
  let name = random_identifier 10 in
  let uri = make_uri name in
  raw_call name uri error_cb user_cb

let call_custom_url ?timeout make_uri =
  let t,w = Lwt.task () in
  let init, finalize = call_ make_uri (fun _ -> Lwt.cancel t) (Lwt.wakeup w) in
  Lwt.on_cancel t finalize;
  let new_t = match timeout with
    | None -> t
    | Some delay ->
      let wait = Lwt.bind  (Lwt_js.sleep delay) (fun () -> Lwt.cancel t; t) in
      Lwt.choose [wait; t]
  in
  init ();
  new_t

let add_param name value l =
  let l = List.filter (fun (x,_) -> x <> name) l in
  (name,value) :: l

let call ?timeout ?(param="callback") url =
  let make_uri cbname =
    match Url.url_of_string url with
    | None -> failwith "Jsonp.call: Cannot parse url"
    | Some url ->
      let new_url =
        match url with
        | Url.Http http -> Url.Http {http with
                                     Url.hu_arguments = add_param param cbname http.Url.hu_arguments}
        | Url.Https http -> Url.Https {http with
                                       Url.hu_arguments = add_param param cbname http.Url.hu_arguments}
        | Url.File file -> Url.File {file with
                                     Url.fu_arguments = add_param param cbname file.Url.fu_arguments}
      in Url.string_of_url new_url
  in
  call_custom_url ?timeout make_uri
