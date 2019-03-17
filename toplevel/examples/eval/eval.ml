open Js_of_ocaml_toplevel

let () = JsooTop.initialize ()

let execute code =
  let code = Js_of_ocaml.Js.to_string code in
  let buffer = Buffer.create 100 in
  let formatter = Format.formatter_of_buffer buffer in
  JsooTop.execute true formatter code;
  Js_of_ocaml.Js.string (Buffer.contents buffer)

let () =
  Js_of_ocaml.Js.export
    "evaluator"
    (object%js
       val execute = execute
    end)

let append_string output cl s =
  let open Js_of_ocaml in
  let d = Dom_html.window##.document in
  let span = Dom_html.createDiv d in
  span##.classList##add (Js.string cl);
  Dom.appendChild span (d##createTextNode (Js.string s));
  Dom.appendChild output span

let configure o chan attr default =
  let open Js_of_ocaml in
  try
    let v = o##getAttribute (Js.string attr) in
    match Js.Opt.to_option v with
    | None -> raise Not_found
    | Some id ->
        let dom = Dom_html.getElementById (Js.to_string id) in
        Sys_js.set_channel_flusher chan (append_string dom attr)
  with Not_found -> Sys_js.set_channel_flusher chan default

let () =
  let open Js_of_ocaml in
  let toploop_ = open_out "/dev/null" in
  let toploop_ppf = Format.formatter_of_out_channel toploop_ in
  JsooTop.initialize ();
  let scripts = Dom_html.window##.document##getElementsByTagName (Js.string "script") in
  let default_stdout = Format.printf "%s@." in
  let default_stderr = Format.eprintf "%s@." in
  let default_toploop x = Format.eprintf "%s@." x in
  for i = 0 to scripts##.length - 1 do
    let item_opt = scripts##item i in
    let elt_opt = Js.Opt.bind item_opt Dom_html.CoerceTo.element in
    match Dom_html.opt_tagged elt_opt with
    | Some (Dom_html.Script script) ->
        if script##._type = Js.string "text/ocaml"
        then (
          let txt = Js.to_string script##.text in
          configure script stdout "stdout" default_stdout;
          configure script stderr "stderr" default_stderr;
          configure script toploop_ "toploop" default_toploop;
          let _ret = JsooTop.use toploop_ppf txt in
          ())
        else ()
    | _ -> ()
  done
