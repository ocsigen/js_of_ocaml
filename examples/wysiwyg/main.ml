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
  iframe##.style##.border := Js.string "2px green solid";
  iframe##.src := Js.string "#";
  iframe##.id := Js.string "wysiFrame";
  Dom.appendChild body iframe;
  Js.Opt.iter iframe##.contentDocument (fun iDoc ->
      iDoc##open_;
      iDoc##write
        (Js.string "<html><body><p><b>Camelus</b><i>bactrianus</i></p></body></html>");
      iDoc##close;
      iDoc##.designMode := Js.string "On";
      let iWin = iframe##.contentWindow in
      Dom.appendChild body (Html.createBr d);
      (* see http://www.quirksmode.org/dom/execCommand.html
       * http://www.mozilla.org/editor/midas-spec.html
       *)
      let createButton ?(show = Js._false) ?(value = None) title action =
        let but = Html.createInput ?_type:(Some (Js.string "submit")) d in
        but##.value := Js.string title;
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
        Dom.appendChild body but;
        but
      in
      ignore (createButton "hr" "inserthorizontalrule");
      ignore (createButton "remove format" "removeformat");
      ignore (createButton "B" "bold");
      ignore (createButton "I" "italic");
      Dom.appendChild body (Html.createBr d);
      ignore (createButton "p" "formatblock" ~value:(Some "p"));
      ignore (createButton "h1" "formatblock" ~value:(Some "h1"));
      ignore (createButton "h2" "formatblock" ~value:(Some "h2"));
      ignore (createButton "h3" "formatblock" ~value:(Some "h3"));
      let prompt query default =
        Js.Opt.get
          (iWin##prompt (Js.string query) (Js.string default))
          (fun () -> Js.string default)
        |> Js.to_string
      in
      (createButton "link" "inserthtml")##.onclick
      := Html.handler (fun _ ->
             let link = prompt "Enter a link" "http://google.ru" in
             let desc = prompt "Enter description" "desc" in
             let link =
               String.concat
                 ""
                 [ "<a href=\""; link; "\" wysitype=\"global\">"; desc; "</a>" ]
             in
             iWin##alert (Js.string link);
             iDoc##execCommand
               (Js.string "inserthtml")
               Js._false
               (Js.some (Js.string link));
             Js._true);
      (createButton "link2wiki" "inserthtml")##.onclick
      := Html.handler (fun _ ->
             let link = prompt "Enter a wikipage" "lololo" in
             let link =
               [ "<a href=\""; link; "\" wysitype=\"wiki\">"; link; "</a>" ]
               |> String.concat ""
             in
             iWin##alert (Js.string link);
             iDoc##execCommand
               (Js.string "inserthtml")
               Js._false
               (Js.some (Js.string link));
             Js._true);
      Dom.appendChild body (Html.createBr d);
      let preview = Html.createTextarea d in
      preview##.readOnly := Js._true;
      preview##.cols := 34;
      preview##.rows := 10;
      preview##.style##.border := Js.string "1px black solid";
      preview##.style##.padding := Js.string "5px";
      Dom.appendChild body preview;
      let wikiFrame = Html.createTextarea d in
      wikiFrame##.id := Js.string "wikiFrame";
      wikiFrame##.readOnly := Js._true;
      wikiFrame##.cols := 34;
      wikiFrame##.rows := 10;
      preview##.style##.border := Js.string "2px blue solid";
      preview##.style##.padding := Js.string "5px";
      Dom.appendChild body wikiFrame;
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
