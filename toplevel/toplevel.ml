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

open Lwt
open Compiler
module Html = Dom_html

module H = struct
  type 'a t = {
    data : 'a array;
    mutable start : int;
    mutable size : int;
  }

  let m x m =
    let r = x mod m in
    if r < 0 then r + m else r

  let make len (init : 'a) : 'a t = {
    data = Array.make len init;
    start = 0;
    size = 0;
  }

  let clear t = t.start <- 0; t.size <- 0

  let add x t =
    let len' = Array.length t.data in
    t.data.(m (t.start + t.size) len') <- x;
    if t.size < len'
    then t.size <- t.size + 1
    else t.start <- m (t.start + 1) len'
  let get t i =
    if i >= t.size || i < 0 then raise Not_found;
    t.data.(m (t.start + i) (Array.length t.data))

  let to_array t =
    if t.size = 0 then [||]
    else
      let arr = Array.make t.size t.data.(0) in
      for i = 0 to t.size - 1 do
        arr.(i) <- get t i;
      done;
      arr

  let size t = t.size


  let get_storage () =
    match Js.Optdef.to_option Html.window##localStorage with
        None -> raise Not_found
      | Some t -> t


  let load_or_create n : Js.js_string Js.t t =
    try
      let s = get_storage () in
      match Js.Opt.to_option (s##getItem(Js.string "history")) with
        | None -> raise Not_found
        | Some s ->
          let a = Json.unsafe_input s in
          if Array.length a.data = n
          then a
          else
            let a' = make n (Js.string "") in
            for i = 0 to a.size - 1 do
              add (get a i) a';
            done;
            a'
    with Not_found -> make n (Js.string "")

  let save t =
    try
      let s = get_storage () in
      let str = Json.output t in
      s##setItem(Js.string "history", str)
    with Not_found -> ()

end

#let_default metaocaml = false
#if metaocaml
let compiler_name = "MetaOCaml"
#else
let compiler_name = "OCaml"
#endif


(* hack on *)
module String' = struct
  let init len f =
    let str = Bytes.create len in
    for i=0 to len-1 do
      Bytes.set str i (f i)
    done;
    str
  let init = (* init from String (ocaml >= 4.02) or init above *)
    let open String in init
end
(* hack off *)

(* load file using a synchronous XMLHttpRequest *)
let load_resource_aux url =
  try
    let xml = XmlHttpRequest.create () in
    xml##_open(Js.string "GET", Js.string url, Js._false);
    xml##send(Js.null);
    if xml##status = 200 then
      let resp = xml##responseText in
      let len = resp##length in
      let str = String'.init len (fun i -> Char.chr (int_of_float resp##charCodeAt(i) land 0xff)) in
      Some(str)
    else
      None
  with _ ->
    None

let load_resource scheme (_,suffix) =
  let url = Printf.sprintf "%s://%s" scheme suffix in
  load_resource_aux url

let exec' s =
  let res : bool = JsooTop.use Format.std_formatter s in
  if not res then Format.eprintf "error while evaluating %s@." s;
  ()

let initialize () =
  Sys_js.register_autoload "/dev/" (fun s -> Some "");
  Sys_js.register_autoload "/" (fun (_,s) -> load_resource_aux ("filesys/" ^ s));
  Sys_js.register_autoload "/http/" (fun s -> load_resource "http" s);
  Sys_js.register_autoload "/https/" (fun s -> load_resource "https" s);
  Sys_js.register_autoload "/ftp/" (fun s -> load_resource "ftp" s);
  JsooTop.initialize ();
  Sys.interactive := false;
  (* MetaOcaml *)
#if metaocaml
  Topdirs.dir_install_printer Format.std_formatter
    (Longident.(Ldot(Lident "Print_code", "print_code")));
  Topdirs.dir_install_printer Format.std_formatter
    (Longident.(Ldot(Lident "Print_code", "print_closed_code")));
#endif
  exec'("let _print_error fmt e = Format.pp_print_string fmt (Js.string_of_error e)");
  Topdirs.dir_install_printer Format.std_formatter
    (Longident.(Lident "_print_error"));
  Hashtbl.add Toploop.directive_table "display" (Toploop.Directive_ident (fun lid ->
      let s =
        match lid with
        | Longident.Lident s -> s
        | Longident.Ldot (_,s) -> s
        | Longident.Lapply _ ->
          raise Exit
      in
      let v : < .. > Js.t= Obj.magic (Toploop.getvalue s) in
      if Js.instanceof v (Js.Unsafe.global ## _HTMLElement)
      then
        Dom.appendChild (Dom_html.getElementById "output") v
      else
        let s = Json.output v in
        print_endline (Js.to_string s)
    ));
  exec' ("module Lwt_main = struct
             let run t = match Lwt.state t with
               | Lwt.Return x -> x
               | Lwt.Fail e -> raise e
               | Lwt.Sleep -> failwith \"Lwt_main.run: thread didn't return\"
            end");
  exec' ("let jsoo_logo =
            let open Tyxml_js.Html5 in
            a ~a:[a_href \"http://ocsigen.org/js_of_ocaml\"] [
              img
                ~src:\"http://ocsigen.org/resources/logos/text_js_of_ocaml_with_shadow.png\"
                ~alt:\"Ocsigen\"  ()
            ]");
  exec' ("#display jsoo_logo");
  let header1 =
      Printf.sprintf "        %s version %%s" compiler_name in
  let header2 = Printf.sprintf
      "     Compiled with Js_of_ocaml version %s" Sys_js.js_of_ocaml_version in
  exec' (Printf.sprintf "Format.printf \"%s@.\" Sys.ocaml_version;;" header1);
  exec' (Printf.sprintf "Format.printf \"%s@.\";;" header2);
  (if JsooTop.get_camlp4_syntaxes () <> []
  then
    let header3 = Printf.sprintf
        "     'JsooTop.get_camlp4_syntaxes ()' to get loaded syntax extensions" in
    exec' (Printf.sprintf "Format.printf \"%s@.@.\";;" header3));
  exec' ("#enable \"pretty\";;");
  exec' ("#enable \"shortvar\";;");
  Sys.interactive := true;
  ()

let trim s =
  let ws c = c = ' ' || c = '\t' || c = '\n' in
  let len = String.length s in
  let start = ref 0 in
  let stop = ref (len - 1) in
  while !start < len && (ws s.[!start])
  do incr start done;
  while !stop > !start && (ws s.[!stop])
  do decr stop done;
  String.sub s !start (!stop - !start + 1)

let by_id s = Dom_html.getElementById s

let by_id_coerce s f  = Js.Opt.get (f (Dom_html.getElementById s)) (fun () -> raise Not_found)

let do_by_id s f =
  try f (Dom_html.getElementById s) with Not_found -> ()

let examples =
  let r = Regexp.regexp "^\\(\\*+(.*)\\*+\\)$" in
  let l = ref [] and name = ref "" in
  let content = ref [] in
  try
    begin
      let ic = open_in "/static/examples.ml" in
      try
        while true do
          let line = input_line ic in
          match Regexp.string_match r line 0 with
          | Some res ->
            if !l <> []
            then begin
              content:=(!name,List.rev !l)::!content;
              l:=[]
            end;
            (name := match Regexp.matched_group res 1 with Some s -> s | None -> assert false)
          | None -> l:= line::!l
        done;
        assert false
      with End_of_file ->
        if !l <> []
        then begin
          content:=(!name,List.rev !l)::!content;
          l:=[]
        end;
        List.rev_map (fun (name,content) -> name,trim (String.concat "\n" content)) !content
    end
  with  _ -> []

#let_default ocpindent = false
#if ocpindent
let _ = Approx_lexer.enable_extension "lwt"
let indent_caml s in_lines =
  let output = {
    IndentPrinter.debug = false;
    config = IndentConfig.default;
    in_lines;
    indent_empty = true;
    adaptive = true;
    kind = IndentPrinter.Print (fun s acc -> acc ^ s)
  }
  in
  let stream = Nstream.of_string s in
  IndentPrinter.proceed output stream IndentBlock.empty ""
#else
let indent_caml s in_lines = s
  (* ocp-indent not available yet in 4.02 *)
#endif

let indent_textarea textbox =
  let rec loop s acc (i,pos') =
    try
      let pos = String.index_from s pos' '\n' in
      loop s ((i,(pos',pos))::acc) (succ i,succ pos)
    with _ -> List.rev ((i,(pos',String.length s)) :: acc) in
  let rec find (l : (int * (int * int)) list ) c =
    match l with
    | [] -> assert false
    | (i,(lo,up))::_ when up >= c -> c,i,lo,up
    | (_,(lo,up))::rem -> find rem c in
  let v = textbox##value in
  let pos =
    let c1 = (Obj.magic textbox)##selectionStart
    and c2 = (Obj.magic textbox)##selectionEnd in
    if Js.Opt.test (Obj.magic c1) && Js.Opt.test (Obj.magic c2)
    then begin
      let l = loop (Js.to_string v) [] (0,0) in
      Some (find l c1,find l c2)
    end
    else None in
  let f = match pos with
    | None -> (fun _ -> true)
    | Some ((c1,line1,lo1,up1),(c2,line2,lo2,up2)) -> (fun l -> l>=(line1+1) && l<=(line2+1)) in
  let v = indent_caml (Js.to_string v) f in
  textbox##value<-Js.string v;
  begin match pos with
    | Some ((c1,line1,lo1,up1),(c2,line2,lo2,up2)) ->
      let l = loop v [] (0,0) in
      let (lo1'',up1'') = List.assoc line1 l in
      let (lo2'',up2'') = List.assoc line2 l in
      let n1 = max (c1 + up1'' - up1) lo1'' in
      let n2 = max (c2 + up2'' - up2) lo2'' in
      let () = (Obj.magic textbox)##setSelectionRange(n1,n2) in
      textbox##focus();
      ()
    | None -> () end

let rec filter_map f = function
  | [] -> []
  | x::xs ->
    match f x with
    | None -> filter_map f xs
    | Some x -> x:: filter_map f xs


(* we need to compute the hash form href to avoid different encoding behavior
     across browser. see Url.get_fragment *)
let parse_hash () =
  let hash_regexp = jsnew Js.regExp (Js.string "#(.*)") in
  let frag = Dom_html.window##location##href##_match(hash_regexp) in
  Js.Opt.case
    frag
    (fun () -> [])
    (fun res ->
       let res = Js.match_result res in
       let frag = Js.to_string (Js.Unsafe.get res 1) in
       Url.decode_arguments frag)

let run _ =
  let container = by_id "toplevel-container" in
  let output = by_id "output" in
  let textbox : 'a Js.t = by_id_coerce "userinput" Html.CoerceTo.textarea in
  let example_container = by_id "toplevel-examples" in
  let can = by_id_coerce "test-canvas" Html.CoerceTo.canvas in
  Graphics_js.open_canvas can;
  let sharp_chan = open_out "/dev/null0" in
  let sharp_ppf = Format.formatter_of_out_channel sharp_chan in

  let caml_chan = open_out "/dev/null1" in
  let caml_ppf = Format.formatter_of_out_channel caml_chan in


  let resize () =
    Lwt.pause () >>= fun () ->
    textbox##style##height <- Js.string "auto";
    textbox##style##height <- Js.string (Printf.sprintf "%dpx" (max 18 textbox##scrollHeight));
    container##scrollTop <- container##scrollHeight;
    Lwt.return () in


  let hist = H.load_or_create 100 in
  let hist_idx = ref (H.size hist) in
  let cur = ref (Js.string "") in
  let execute () =
    let content' = Js.to_string textbox##value in
    let content = trim content' in
    let len = String.length content in
    let content =
      if content = ""
      || (len > 2
          && content.[len - 1] = ';'
          && content.[len - 2] = ';')
      then content'
      else content' ^ ";;" in
    H.add (Js.string content') hist;
    H.save hist;
    cur:=Js.string "";
    hist_idx:=H.size hist;
    JsooTop.execute true ~pp_code:sharp_ppf caml_ppf content;
    textbox##value <- Js.string "";
    resize () >>= fun () ->
    container##scrollTop <- container##scrollHeight;
    textbox##focus();
    Lwt.return_unit in

  List.iter (fun (name,content) ->
      let a = Tyxml_js.Html5.(a ~a:[
          a_class ["list-group-item"];
          a_onclick (fun _ ->
              textbox##value <- Js.string content;
              Lwt.async(fun () ->
                  resize () >>= fun () ->
                  container##scrollTop <- container##scrollHeight;
                  textbox##focus();
                  Lwt.return_unit);
              true
            )] [pcdata name]) in
      Dom.appendChild example_container (Tyxml_js.To_dom.of_a a)
    ) examples;

  begin (* setup handlers *)
    do_by_id "btn-execute"
      (fun e -> e##onclick <- Html.handler (fun _ -> Lwt.async execute; Js._false));
    do_by_id "btn-clear"
      (fun e -> e##onclick <- Html.handler (fun _ -> output##innerHTML <- Js.string ""; Js._false));
    do_by_id "btn-reset"
      (fun e -> e##onclick <- Html.handler (fun _ -> output##innerHTML <- Js.string "";
                                             initialize (); Js._false));
    do_by_id "btn-share"
      (fun e ->
         e##style##display <- Js.string "block";
         e##onclick <- Html.handler (fun _ ->

           (* get all ocaml code *)
           let childs = Dom.list_of_nodeList output##childNodes in
           let code = filter_map (fun t ->
               let open Js.Opt in
               to_option (bind (Dom_html.CoerceTo.element t) (fun t ->
                   if Js.to_bool (t##classList##contains(Js.string "sharp"))
                   then bind (t##textContent) (fun t -> return (Js.to_string t))
                   else empty))) childs in
           let code_encoded = Base64.encode (String.concat "" code) in

           let url,is_file = match Url.Current.get () with
             | Some (Url.Http url) -> Url.Http ({url with Url.hu_fragment = "" }),false
             | Some (Url.Https url) -> Url.Https ({url with Url.hu_fragment= "" }), false
             | Some (Url.File url) -> Url.File ({url with Url.fu_fragment= "" }), true
             | _ -> assert false in

           let frag =
             let frags = parse_hash () in
             let frags = List.remove_assoc "code" frags @ ["code",code_encoded] in
             Url.encode_arguments frags
           in

           let uri = Url.string_of_url url ^ "#" ^ frag in
           let append_url str =
             let dom = Tyxml_js.Html5.(
                 p [ pcdata "Share this url : "; a ~a:[a_href str] [ pcdata str ]]) in
             Dom.appendChild output (Tyxml_js.To_dom.of_element dom);
           in
           Lwt.async (fun () ->
               Lwt.catch (fun () ->
                   if is_file
                   then failwith "Cannot shorten url with file scheme"
                   else
                   let jsonp_call = Jsonp.call
                       (Printf.sprintf "http://is.gd/create.php?format=json&url=%s" (Url.urlencode uri)) in
                   Lwt.bind jsonp_call (fun o ->
                       let str = Js.to_string o##shorturl in
                       append_url str;
                       Lwt.return_unit
                     ))
                 (fun exn ->
                    Format.eprintf "Could not generate short url. reason: %s@." (Printexc.to_string exn);
                    append_url uri;
                    Lwt.return_unit));
           Js._false));

    textbox##onkeydown <- Html.handler (fun e ->
        match e##keyCode with
        | 13 when not (Js.to_bool e##ctrlKey  ||
                       Js.to_bool e##shiftKey ||
                       Js.to_bool e##altKey   ||
                       Js.to_bool e##metaKey) ->
          Lwt.async execute;
          Js._false
        | 13 ->
          Lwt.async resize;Js._true
        | 09 ->
          indent_textarea textbox;
          Js._false
        | 38 -> begin
            let txt = Js.to_string textbox##value in
            try
              let pos = (Obj.magic textbox)##selectionStart - 1  in
              (if pos < 0  then raise Not_found);
              let _ = String.rindex_from txt pos '\n' in
              Js._true
            with Not_found ->
              try
                if !hist_idx = H.size hist
                then cur := textbox##value;
                let idx = !hist_idx - 1 in
                let s=H.get hist idx in
                hist_idx:=idx;
                textbox##value <- s;
                let s' = Js.to_string s in
                let p' = try max 0 (String.index s' '\n' - 1) with _ -> String.length s' in
                let () = (Obj.magic textbox)##setSelectionRange(p',p') in
                Js._false
              with _ -> Js._false
          end
        | 40 -> begin
            let txt = Js.to_string textbox##value in
            try
              let pos = (Obj.magic textbox)##selectionStart in
              (if String.length txt = pos  then raise Not_found);
              let _ = String.index_from txt pos '\n' in
              Js._true
            with Not_found ->
              try
                let idx = !hist_idx + 1 in
                if idx = H.size hist
                then begin
                  incr hist_idx;
                  textbox##value <- !cur;
                end else begin
                  let s=H.get hist idx in
                  hist_idx:=idx;
                  let s' = Js.to_string s in
                  let slen = String.length s' in
                  let () = (Obj.magic textbox)##setSelectionRange(slen,slen) in
                  textbox##value <- s
                end;
                Js._false
              with _ -> Js._false
          end
        | 76 when (Js.to_bool e##ctrlKey  ||
                   Js.to_bool e##altKey   ||
                   Js.to_bool e##metaKey) ->
          output##innerHTML <- Js.string "";
          Js._true
        | 75 when (Js.to_bool e##ctrlKey  ||
                   Js.to_bool e##altKey   ||
                   Js.to_bool e##metaKey) ->
          initialize ();
          Js._false

        | _ -> Js._true
      );
    textbox##onkeyup <- Html.handler (fun e ->
        Lwt.async resize;Js._true);
    textbox##onchange <- Html.handler (fun _ -> Lwt.async resize; Js._true)
  end;

  let append_string cl s =
    let span = Tyxml_js.Html5.(span ~a:[a_class [cl]] [pcdata s]) in
    Dom.appendChild output (Tyxml_js.To_dom.of_element span) in

#let_default higlo = false
#if higlo
let append_ocaml cl_base s =
  let tks = Higlo.parse ~lang:"ocaml" s in
  let span' cl s = Tyxml_js.Html5.(span ~a:[a_class [cl]] [pcdata s]) in
  let make_span = function
    | Higlo.Bcomment s -> span' "comment" s
    | Higlo.Constant s -> span' "constant" s
    | Higlo.Directive s -> span' "directive" s
    | Higlo.Escape s -> span' "escape" s
    | Higlo.Id s -> span' "id" s
    | Higlo.Keyword (level,s) -> span' (Printf.sprintf "kw%d" level) s
    | Higlo.Lcomment s -> span' "comment" s
    | Higlo.Numeric s -> span' "numeric" s
    | Higlo.String s -> span' "string" s
    | Higlo.Symbol (level,s) -> span' (Printf.sprintf "sym%d" level) s
    | Higlo.Text s -> span' "text" s in
  let container = Tyxml_js.Html5.(div ~a:[a_class [cl_base]] (List.map make_span tks)) in
  Dom.appendChild output (Tyxml_js.To_dom.of_element container)
in
#else
let append_ocaml = append_string in
#endif
  Sys_js.set_channel_flusher caml_chan (append_ocaml "caml");
  Sys_js.set_channel_flusher sharp_chan (append_ocaml "sharp");
  Sys_js.set_channel_flusher stdout (append_string "stdout");
  Sys_js.set_channel_flusher stderr (append_string "stderr");

  Lwt.async_exception_hook:=(fun exc -> Format.eprintf "exc during Lwt.async: %s@." (Printexc.to_string exc));

  Lwt.async (fun () ->
      resize () >>= fun () ->
      textbox##focus ();
      Lwt.return_unit
    );
  let ph = by_id "last-js" in
  initialize ();
  let runcode : (string -> 'a) = Js.Unsafe.global##toplevelEval in
  Js.Unsafe.global##toplevelEval <- (fun bc ->
      ph##innerHTML <- Js.string bc;
      runcode bc
    );
  (* Run initial code if any *)
  try
    let code = List.assoc "code" (parse_hash ()) in
    textbox##value <- Js.string (Base64.decode code);
    Lwt.async execute
  with
  | Not_found -> ()
  | exc ->
    Firebug.console##log_3(Js.string "exception", Js.string (Printexc.to_string exc), exc)

let _ = Html.window##onload <- Html.handler (fun _ -> run (); Js._false)
