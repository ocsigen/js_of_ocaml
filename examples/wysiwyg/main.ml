(* Js_of_ocaml example
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2010 Dmitry Kosarev
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
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

module Html = Dom_html

let (>>=) = Lwt.bind

let onload _ =
  let d = Html.document in
  let body : Html.element Js.t =
    Js.Opt.get (d##getElementById(Js.string "wiki_demo"))
      (fun () -> assert false) in

  let iframe = Html.createIframe d in
  iframe##style##border <- Js.string "2px green solid";
  iframe##src <- Js.string "#";
  Dom.appendChild body iframe;

  Js.Opt.iter (iframe##contentDocument) (fun iDoc ->
    iDoc##open_ ();
    iDoc##write (Js.string "<html><body><b>Camelus </b><i>bactrianus</i></body></html>");
    iDoc##close ();
    iDoc##designMode <- Js.string "On";

    let iWin  = iframe##contentWindow in
    let createButton title action  = 
      let but = Html.createInput ?_type:(Some (Js.string "submit")) d in
      but##value <- Js.string title;
      but##onclick <- Html.handler 
	(fun _ -> 
	  iWin##focus ();
	  iDoc##execCommand_ (Js.string action); 
	  Js._true);
      Dom.appendChild body but
    in
    Dom.appendChild body (Html.createBr d);
    createButton "B" "bold";
    createButton "I" "italic";
    createButton "U" "underline";
    Dom.appendChild body (Html.createBr d);

    let preview = Html.createTextarea d in
    preview##readOnly <- Js._true;
    preview##cols <- 34;
    preview##rows <- 10;
    preview##style##border <- Js.string "1px black solid";
    preview##style##padding <- Js.string "5px";
    Dom.appendChild body preview;

    let rec dyn_preview old_text n =
      let text = Js.to_string iDoc##body##innerHTML in
      let n =
	if text <> old_text then begin
	  begin try
		  preview##value <- Js.string text
	    with _ -> () end;
	  20
	end else
	  max 0 (n - 1)
      in
      Lwt_js.sleep (if n = 0 then 0.5 else 0.1) >>= fun () ->
      dyn_preview text n
    in
    ignore (dyn_preview "" 0)
  );
  Js._false

let _ = Html.window##onload <- Html.handler onload
