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
open Js_of_ocaml
open Js_of_ocaml_lwt
module Html = Dom_html

let ( >>= ) = Lwt.bind

let ( |> ) x f = f x

exception Break of bool

let is_visible_text s =
  let len = String.length s in
  let rec loop i =
    if i >= len
    then ()
    else if s.[i] = '\\' && i < len - 1 && s.[i + 1] = '\\'
    then loop (i + 2)
    else
      match s.[i] with
      | '\n' -> loop (i + 1)
      | _ -> raise (Break true)
  in
  try
    loop 0;
    false
  with Break b -> b

open Dom

let rec html2wiki body =
  let ans = Buffer.create 10 in
  let add_str ?(surr = "") s =
    if is_visible_text s then Buffer.add_string ans (surr ^ s ^ surr) else ()
  in
  let childNodes = body##.childNodes in
  for i = 0 to childNodes##.length - 1 do
    Js.Opt.iter
      (childNodes##item i)
      (fun node ->
        match Js.to_string node##.nodeName with
        | "B" ->
            let inner = html2wiki node in
            add_str inner ~surr:"**"
        | "I" ->
            let inner = html2wiki node in
            add_str inner ~surr:"//"
        | "#text" -> (
            match Js.Opt.to_option node##.nodeValue with
            | Some x -> Buffer.add_string ans (Js.to_string x)
            | None -> ())
        | "P" ->
            let inner = html2wiki node in
            add_str (inner ^ "\n\n")
        | "BR" -> Buffer.add_string ans "\\\\"
        | "HR" -> Buffer.add_string ans "----"
        | "DIV" ->
            let inner = html2wiki node in
            Buffer.add_string ans inner
        | "A" ->
            let x : element Js.t Js.opt = Js.some (Js.Unsafe.coerce node) in
            let el = Js.Opt.get x (fun _ -> assert false) in
            Js.Opt.case
              (el##getAttribute (Js.string "wysitype"))
              (fun () -> Buffer.add_string ans "^error_in_anchor^")
              (fun s ->
                let url =
                  Js.Opt.get (el##getAttribute (Js.string "href")) (fun _ -> assert false)
                  |> Js.to_string
                in
                match Js.to_string s with
                | "global" ->
                    let desc = html2wiki node in
                    Buffer.add_string
                      ans
                      (String.concat "" [ "[["; url; "|"; desc; "]]" ])
                | "wiki" -> String.concat "" [ "[["; url; "]]" ] |> Buffer.add_string ans
                | _ -> Buffer.add_string ans "^error2_in_anchor^")
        | ("H1" | "H2" | "H3") as hh ->
            let n = int_of_char hh.[1] - int_of_char '0' + 1 in
            let prefix = String.make n '=' in
            let inner = html2wiki node in
            Buffer.add_string ans (prefix ^ inner ^ "\n\n")
        | _ as name -> Buffer.add_string ans ("^" ^ name ^ "^"))
  done;
  Buffer.contents ans

let () =
  let d = Html.document in
  let body =
    Js.Opt.get (d##getElementById (Js.string "wiki_demo")) (fun () -> assert false)
  in
  let iframe = Html.createIframe d in
  iframe##.src := Js.string "#";
  iframe##.id := Js.string "wysiFrame";
  Dom.appendChild body iframe;
  Js.Opt.iter iframe##.contentDocument (fun iDoc ->
      iDoc##open_;
      iDoc##write
        (Js.string
           "<html><body><h1>Bactrian Camel</h1><p>The <b>Bactrian camel</b> (<i>Camelus \
            bactrianus</i>) is a large even-toed ungulate native to the steppes of \
            Central Asia. It has two humps on its back, in contrast to the single-humped \
            dromedary.</p><h2>Characteristics</h2><p>Bactrian camels are exceptionally \
            well adapted to the harsh conditions of the <b>Gobi Desert</b> and \
            surrounding regions. They can withstand temperatures ranging from \
            <i>-40\194\176C in winter</i> to <i>+40\194\176C in summer</i>.</p><p>Key \
            features include:</p><p>- Thick, woolly coat that sheds in summer<br>- \
            Broad, tough feet for walking on sand and snow<br>- Ability to go without \
            water for months</p><h2>Conservation</h2><p>Wild Bactrian camels are \
            <b>critically endangered</b>, with fewer than 1,000 remaining in remote \
            areas of China and Mongolia.</p></body></html>");
      iDoc##close;
      iDoc##.designMode := Js.string "On";
      let iWin = iframe##.contentWindow in
      let toolbar = Html.createDiv d in
      toolbar##.className := Js.string "toolbar";
      Dom.appendChild body toolbar;
      let createButton
          ?(show = Js._false)
          ?(value = None)
          ?(html = None)
          parent
          title
          action =
        let but = Html.createButton ~_type:(Js.string "button") d in
        (match html with
        | Some h -> but##.innerHTML := Js.string h
        | None -> Dom.appendChild but (d##createTextNode (Js.string title)));
        let wrap s =
          match s with
          | None -> Js.null
          | Some s -> Js.some (Js.string s)
        in
        but##.onclick :=
          Html.handler (fun _ ->
              iWin##focus;
              iDoc##execCommand (Js.string action) show (wrap value);
              Js._true);
        Dom.appendChild parent but;
        but
      in
      let format_group = Html.createDiv d in
      format_group##.className := Js.string "toolbar-group";
      Dom.appendChild toolbar format_group;
      ignore (createButton format_group "B" "bold");
      ignore (createButton format_group "I" "italic");
      let remove_fmt_svg =
        "<svg width='14' height='14' viewBox='0 0 16 16' fill='none' \
         stroke='currentColor' stroke-linecap='round'><line x1='4' y1='3' x2='12' y2='3' \
         stroke-width='2.5'/><line x1='8' y1='3' x2='7' y2='13' \
         stroke-width='2.5'/><line x1='3' y1='14' x2='14' y2='3' stroke-width='1.5' \
         stroke='#e06060'/></svg>"
      in
      ignore (createButton format_group "" "removeformat" ~html:(Some remove_fmt_svg));
      let block_group = Html.createDiv d in
      block_group##.className := Js.string "toolbar-group";
      Dom.appendChild toolbar block_group;
      ignore (createButton block_group "P" "formatblock" ~value:(Some "p"));
      ignore (createButton block_group "H1" "formatblock" ~value:(Some "h1"));
      ignore (createButton block_group "H2" "formatblock" ~value:(Some "h2"));
      ignore (createButton block_group "H3" "formatblock" ~value:(Some "h3"));
      let insert_group = Html.createDiv d in
      insert_group##.className := Js.string "toolbar-group";
      Dom.appendChild toolbar insert_group;
      ignore (createButton insert_group "HR" "inserthorizontalrule");
      let link_group = Html.createDiv d in
      link_group##.className := Js.string "toolbar-group";
      Dom.appendChild toolbar link_group;
      let prompt query default =
        Js.Opt.get
          (iWin##prompt (Js.string query) (Js.string default))
          (fun () -> Js.string default)
        |> Js.to_string
      in
      (createButton link_group "Link" "inserthtml")##.onclick
      := Html.handler (fun _ ->
          let link = prompt "Enter a URL" "https://example.com" in
          let desc = prompt "Enter description" link in
          let link =
            String.concat
              ""
              [ "<a href=\""; link; "\" wysitype=\"global\">"; desc; "</a>" ]
          in
          iDoc##execCommand (Js.string "inserthtml") Js._false (Js.some (Js.string link));
          Js._true);
      (createButton link_group "Wiki Link" "inserthtml")##.onclick
      := Html.handler (fun _ ->
          let link = prompt "Enter a wiki page" "PageName" in
          let link =
            [ "<a href=\""; link; "\" wysitype=\"wiki\">"; link; "</a>" ]
            |> String.concat ""
          in
          iDoc##execCommand (Js.string "inserthtml") Js._false (Js.some (Js.string link));
          Js._true);
      let panels = Html.createDiv d in
      panels##.className := Js.string "output-panels";
      Dom.appendChild body panels;
      let html_panel = Html.createDiv d in
      html_panel##.className := Js.string "output-panel";
      Dom.appendChild panels html_panel;
      let html_label = Html.createDiv d in
      html_label##.className := Js.string "panel-label";
      Dom.appendChild html_label (d##createTextNode (Js.string "HTML Output"));
      Dom.appendChild html_panel html_label;
      let preview = Html.createTextarea d in
      preview##.readOnly := Js._true;
      Dom.appendChild html_panel preview;
      let wiki_panel = Html.createDiv d in
      wiki_panel##.className := Js.string "output-panel";
      Dom.appendChild panels wiki_panel;
      let wiki_label = Html.createDiv d in
      wiki_label##.className := Js.string "panel-label";
      Dom.appendChild wiki_label (d##createTextNode (Js.string "Wiki Markup"));
      Dom.appendChild wiki_panel wiki_label;
      let wikiFrame = Html.createTextarea d in
      wikiFrame##.id := Js.string "wikiFrame";
      wikiFrame##.readOnly := Js._true;
      Dom.appendChild wiki_panel wikiFrame;
      let rec dyn_preview old_text n =
        let text = Js.to_string iDoc##.body##.innerHTML in
        let n =
          if text <> old_text
          then (
            (try
               preview##.value := Js.string text;
               wikiFrame##.value := Js.string (html2wiki (iDoc##.body :> Dom.node Js.t))
             with _ -> ());
            20)
          else max 0 (n - 1)
        in
        Lwt_js.sleep (if n = 0 then 0.5 else 0.1) >>= fun () -> dyn_preview text n
      in
      ignore (dyn_preview "" 0))
