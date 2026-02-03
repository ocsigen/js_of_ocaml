(* Js_of_ocaml example
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2010 Jérôme Vouillon
 * Laboratoire PPS - CNRS Université Paris Diderot
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
open Js_of_ocaml
open Js_of_ocaml_lwt
module Html = Dom_html

let ( >>= ) = Lwt.bind

let replace_child p n =
  Js.Opt.iter p##.firstChild (fun c -> Dom.removeChild p c);
  Dom.appendChild p n

let () =
  let d = Html.document in
  let body =
    Js.Opt.get (d##getElementById (Js.string "wiki_demo")) (fun () -> assert false)
  in
  let edit_pane = Html.createDiv d in
  edit_pane##.className := Js.string "pane";
  let edit_label = Html.createDiv d in
  edit_label##.className := Js.string "pane-label";
  Dom.appendChild edit_label (d##createTextNode (Js.string "Editor"));
  Dom.appendChild edit_pane edit_label;
  let textbox = Html.createTextarea d in
  textbox##.value := Js.string Test.test1;
  Dom.appendChild edit_pane textbox;
  Dom.appendChild body edit_pane;
  let preview_pane = Html.createDiv d in
  preview_pane##.className := Js.string "pane";
  let preview_label = Html.createDiv d in
  preview_label##.className := Js.string "pane-label";
  Dom.appendChild preview_label (d##createTextNode (Js.string "Preview"));
  Dom.appendChild preview_pane preview_label;
  let preview = Html.createDiv d in
  preview##.className := Js.string "preview";
  Dom.appendChild preview_pane preview;
  Dom.appendChild body preview_pane;
  let rec dyn_preview old_text n =
    let text = Js.to_string textbox##.value in
    let n =
      if text <> old_text
      then (
        (try
           let rendered = Wiki_syntax.xml_of_wiki text in
           replace_child preview rendered
         with _ -> ());
        20)
      else max 0 (n - 1)
    in
    Lwt_js.sleep (if n = 0 then 0.5 else 0.1) >>= fun () -> dyn_preview text n
  in
  ignore (dyn_preview "" 0)
