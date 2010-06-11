module Html = Dom_html

let (>>=) = Lwt.bind

let replace_child p n =
  Js.Opt.iter (p##firstChild) (fun c -> Dom.removeChild p c);
  Dom.appendChild p n

let onload () =
  let d = Html.document in
  let body = d##body in
  let textbox = Html.createTextAreaElement d in
  textbox##rows <- 20; textbox##cols <- 80;
  let preview = Html.createDivElement d in
  preview##style##border <- Js.string "1px black dashed";
  preview##style##padding <- Js.string "5px";
  Dom.appendChild body textbox;
  Dom.appendChild body (Html.createBrElement d);
  Dom.appendChild body preview;
  let rec dyn_preview old_text n =
    let text = Js.to_string (textbox##value) in
    let n =
      if text <> old_text then begin
        let rendered = Wiki_syntax.xml_of_wiki text in
        replace_child preview rendered;
        20
      end else
        max 0 (n - 1)
    in
    Lwt_js.sleep (if n = 0 then 0.5 else 0.1) >>= fun () ->
    dyn_preview text n
  in
  ignore (dyn_preview "" 0)

let _ =
Html.window##onload <- onload
