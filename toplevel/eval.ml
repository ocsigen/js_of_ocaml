(* Js_of_ocaml toplevel
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2011 Jérôme Vouillon
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

let append_string output cl s =
  let d = Dom_html.window##document in
  let span = Dom_html.createDiv d in
  span##classList##add(Js.string cl);
  Dom.appendChild span (d##createTextNode (Js.string s));
  Dom.appendChild output span

let configure o chan attr default =
  try
    let v = o##getAttribute(Js.string attr) in
    match Js.Opt.to_option v with
    | None -> raise Not_found
    | Some id ->
      let dom = Dom_html.getElementById (Js.to_string id) in
      Sys_js.set_channel_flusher chan (append_string dom attr)
  with Not_found -> Sys_js.set_channel_flusher chan default


let _ = Lwt.bind (Lwt_js_events.domContentLoaded ()) (fun () ->
    let toploop_ = open_out "/dev/null" in
    let toploop_ppf = Format.formatter_of_out_channel toploop_ in
    Lwt.async_exception_hook:= (fun exc -> Format.eprintf "exc during Lwt.async: %s@." (Printexc.to_string exc));
    JsooTop.initialize ();
    let scripts = Dom_html.window##document##getElementsByTagName(Js.string "script") in
    let default_stdout = Format.printf  "%s@." in
    let default_stderr = Format.eprintf "%s@." in
    let default_toploop x = () in
    for i = 0 to scripts##length - 1 do
      let item_opt = scripts##item(i) in
      let elt_opt = Js.Opt.bind item_opt Dom_html.CoerceTo.element in
      match Dom_html.opt_tagged elt_opt with
      | Some (Dom_html.Script script) ->
        if script##_type = Js.string "text/ocaml"
        then begin
          let txt = Js.to_string script##text in
          configure script stdout "stdout" default_stdout;
          configure script stderr "stderr" default_stderr;
          configure script toploop_ "toploop" default_toploop;
          let _ret = JsooTop.use toploop_ppf txt in
          ()
        end
        else ()
      | _ -> ()
    done;
    Lwt.return_unit)
