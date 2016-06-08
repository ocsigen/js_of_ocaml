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

#ifdef metaocaml
let compiler_name = "MetaOCaml"
#else
let compiler_name = "OCaml"
#endif

let by_id s = Dom_html.getElementById s
let by_id_coerce s f  = Js.Opt.get (f (Dom_html.getElementById s)) (fun () -> raise Not_found)
let do_by_id s f = try f (Dom_html.getElementById s) with Not_found -> ()

(* load file using a synchronous XMLHttpRequest *)
let load_resource_aux filename url =
  XmlHttpRequest.perform_raw ~response_type:XmlHttpRequest.ArrayBuffer url
  >|= fun frame ->
  if frame.XmlHttpRequest.code = 200
  then
    Js.Opt.case frame.XmlHttpRequest.content
      (fun () -> Printf.eprintf "Could not load %s\n" filename)
      (fun b  ->
         Sys_js.update_file ~name:filename ~content:(Typed_array.String.of_arrayBuffer b))
  else ()

let load_resource scheme (prefix,suffix) =
  let url = scheme ^ suffix in
  let filename = Filename.concat prefix suffix in
  Lwt.async (fun () -> load_resource_aux filename url);
  Some ""

let setup_pseudo_fs () =
  Sys_js.register_autoload "/dev/"   (fun s -> None);
  Sys_js.register_autoload "/http/"  (fun s -> load_resource "http://" s);
  Sys_js.register_autoload "/https/" (fun s -> load_resource "https://" s);
  Sys_js.register_autoload "/ftp/"   (fun s -> load_resource "ftp://" s);
  Sys_js.register_autoload "/"       (fun s -> load_resource "filesys/" s)

let exec' s =
  let res : bool = JsooTop.use Format.std_formatter s in
  if not res then Format.eprintf "error while evaluating %s@." s

let setup_toplevel () =
  JsooTop.initialize ();
  Sys.interactive := false;
  exec' ("module Lwt_main = struct
             let run t = match Lwt.state t with
               | Lwt.Return x -> x
               | Lwt.Fail e -> raise e
               | Lwt.Sleep -> failwith \"Lwt_main.run: thread didn't return\"
            end");
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
  exec' ("#disable \"shortvar\";;");
#ifdef ppx
  Ast_mapper.register "js_of_ocaml" Ppx_js.js_mapper;
#endif
  Hashtbl.add Toploop.directive_table "load_js" (Toploop.Directive_string (fun name ->
    Js.Unsafe.global##load_script_(name)));
  Sys.interactive := true;
  ()

let resize ~container ~textbox ()  =
  Lwt.pause () >>= fun () ->
  textbox##style##height <- Js.string "auto";
  textbox##style##height <- Js.string (Printf.sprintf "%dpx" (max 18 textbox##scrollHeight));
  container##scrollTop <- container##scrollHeight;
  Lwt.return ()

let setup_printers () =
  (* MetaOcaml *)
#ifdef metaocaml
  Topdirs.dir_install_printer Format.std_formatter
    (Longident.(Ldot(Lident "Print_code", "print_code")));
  Topdirs.dir_install_printer Format.std_formatter
    (Longident.(Ldot(Lident "Print_code", "print_closed_code")));
#endif
  exec'("let _print_error fmt e = Format.pp_print_string fmt (Js.string_of_error e)");
  Topdirs.dir_install_printer Format.std_formatter (Longident.(Lident "_print_error"));
  exec'("let _print_unit fmt (_ : 'a) : 'a = Format.pp_print_string fmt \"()\"");
  Topdirs.dir_install_printer Format.std_formatter (Longident.(Lident "_print_unit"))

let setup_examples ~container ~textbox =
  let r = Regexp.regexp "^\\(\\*+(.*)\\*+\\)$" in
  let all = ref [] in
  begin try
    let ic = open_in "/static/examples.ml" in
    while true do
      let line = input_line ic in
      match Regexp.string_match r line 0 with
      | Some res ->
	 let name = match Regexp.matched_group res 1 with Some s -> s | None -> assert false in
         all := `Title name :: !all
      | None -> all := `Content line :: !all
    done;
    assert false
  with  _ -> () end;
  let example_container = by_id "toplevel-examples" in
  let _ = List.fold_left (fun acc tok ->
      match tok with
      | `Content line -> line ^ "\n" ^ acc
      | `Title   name ->
      let a = Tyxml_js.Html.(a ~a:[
        a_class ["list-group-item"];
        a_onclick (fun _ ->
          textbox##value <- (Js.string acc)##trim();
            Lwt.async(fun () ->
              resize ~container ~textbox ()  >>= fun () ->
              textbox##focus();
              Lwt.return_unit);
            true
	 )] [pcdata name]) in
      Dom.appendChild example_container (Tyxml_js.To_dom.of_a a);
      ""
    ) "" !all in
  ()

(* we need to compute the hash form href to avoid different encoding behavior
     across browser. see Url.get_fragment *)
let parse_hash () =
  let frag = Url.Current.get_fragment () in
  Url.decode_arguments frag

let rec iter_on_sharp ~f x =
  Js.Opt.iter (Dom_html.CoerceTo.element x)
	      (fun e -> if Js.to_bool (e##classList##contains(Js.string "sharp")) then f e);
  match Js.Opt.to_option x##nextSibling with
  | None -> ()
  | Some n -> iter_on_sharp ~f n

let setup_share_button ~output =
  do_by_id "btn-share" (fun e ->
    e##style##display <- Js.string "block";
    e##onclick <- Dom_html.handler (fun _ ->
      (* get all ocaml code *)
      let code = ref [] in
      Js.Opt.iter
	(output##firstChild)
	(iter_on_sharp ~f:(fun e ->
          code := Js.Opt.case (e##textContent)
                    (fun () -> "")
                    (Js.to_string) :: !code));
      let code_encoded = B64.encode (String.concat "" (List.rev !code)) in
      let url,is_file = match Url.Current.get () with
        | Some (Url.Http url) -> Url.Http ({url with Url.hu_fragment = "" }),false
        | Some (Url.Https url) -> Url.Https ({url with Url.hu_fragment= "" }), false
        | Some (Url.File url) -> Url.File ({url with Url.fu_fragment= "" }), true
        | _ -> assert false in
      let frag =
        let frags = parse_hash () in
        let frags = List.remove_assoc "code" frags @ ["code",code_encoded] in
        Url.encode_arguments frags in
      let uri = Url.string_of_url url ^ "#" ^ frag in
      let append_url str =
        let dom = Tyxml_js.Html.(
            p [ pcdata "Share this url : "; a ~a:[a_href str] [ pcdata str ]]) in
        Dom.appendChild output (Tyxml_js.To_dom.of_element dom)
      in
      Lwt.async (fun () ->
      Lwt.catch (fun () ->
        if is_file
        then failwith "Cannot shorten url with file scheme"
        else
	  let uri = Printf.sprintf "http://is.gd/create.php?format=json&url=%s" (Url.urlencode uri) in
          Lwt.bind (Jsonp.call uri) (fun o ->
	    let str = Js.to_string o##shorturl in
            append_url str;
            Lwt.return_unit)
      )
      (fun exn ->
       Format.eprintf "Could not generate short url. reason: %s@." (Printexc.to_string exn);
       append_url uri;
       Lwt.return_unit));
      Js._false))

let setup_js_preview () =
  let ph = by_id "last-js" in
  let runcode : (string -> 'a) = Js.Unsafe.global##toplevelEval in
  Js.Unsafe.global##toplevelEval <- (fun bc ->
      ph##innerHTML <- Js.string bc;
      runcode bc
    )

let current_position = ref 0
let highlight_location loc =
  let x = ref 0 in
  let output = by_id "output" in
  let first = Js.Opt.get (output##childNodes##item(!current_position)) (fun _ -> assert false) in
  iter_on_sharp first
    ~f:(fun e ->
     incr x;
     let _file1,line1,col1 = Location.get_pos_info (loc.Location.loc_start) in
     let _file2,line2,col2 = Location.get_pos_info (loc.Location.loc_end) in
     if !x >= line1 && !x <= line2
     then
       let from_ = if !x = line1 then `Pos col1 else `Pos 0 in
       let to_   = if !x = line2 then `Pos col2 else `Last in
       Colorize.highlight from_ to_ e)


let append colorize output cl s =
  Dom.appendChild output (Tyxml_js.To_dom.of_element (colorize ~a_class:cl s))

module History = struct
  let data = ref [|""|]
  let idx = ref 0
  let get_storage () =
    match Js.Optdef.to_option Dom_html.window##localStorage with
    | None -> raise Not_found
    | Some t -> t

  let setup () =
    try
      let s = get_storage () in
      match Js.Opt.to_option (s##getItem(Js.string "history")) with
      | None -> raise Not_found
      | Some s -> let a = Json.unsafe_input s in
		  data:=a; idx:=Array.length a - 1
    with _ -> ()

  let push text =
    let l = Array.length !data in
    let n = Array.make (l + 1) "" in
    Array.set  !data (l - 1) text;
    Array.blit !data 0 n 0 l;
    data := n; idx := l;
    try
      let s = get_storage () in
      let str = Json.output !data in
      s##setItem(Js.string "history", str)
    with Not_found -> ()

  let current text = !data.(!idx) <- text
  let previous textbox =
    if !idx > 0
    then begin decr idx; textbox##value <- Js.string (!data.(!idx)) end
  let next textbox =
    if !idx < Array.length !data - 1
    then begin incr idx; textbox##value <- Js.string (!data.(!idx)) end
end

let run _ =
  let container = by_id "toplevel-container" in
  let output = by_id "output" in
  let textbox : 'a Js.t = by_id_coerce "userinput" Dom_html.CoerceTo.textarea in

  let sharp_chan = open_out "/dev/null0" in
  let sharp_ppf = Format.formatter_of_out_channel sharp_chan in

  let caml_chan = open_out "/dev/null1" in
  let caml_ppf = Format.formatter_of_out_channel caml_chan in

  let execute () =
    let content = Js.to_string (textbox##value##trim()) in
    let content' =
      let len = String.length content in
      if try content <> "" && content.[len-1] <> ';' && content.[len-2] <> ';' with _ -> true
      then content ^ ";;"
      else content in
    current_position := output##childNodes##length;
    textbox##value <- Js.string "";
    History.push content;
    JsooTop.execute true ~pp_code:sharp_ppf ~highlight_location caml_ppf content';
    resize ~container ~textbox () >>= fun () ->
    container##scrollTop <- container##scrollHeight;
    textbox##focus();
    Lwt.return_unit in

  let history_down e =
    let txt = Js.to_string textbox##value in
    let pos = textbox##selectionStart in
    try
      (if String.length txt = pos  then raise Not_found);
      let _ = String.index_from txt pos '\n' in
      Js._true
    with Not_found ->
      History.current txt;
      History.next textbox;
      Js._false
  in
  let history_up   e =
    let txt = Js.to_string textbox##value in
    let pos = textbox##selectionStart - 1  in
    try
      (if pos < 0  then raise Not_found);
      let _ = String.rindex_from txt pos '\n' in
      Js._true
    with Not_found ->
      History.current txt;
      History.previous textbox;
      Js._false
  in

  let meta e =
    let b = Js.to_bool in
    b e##ctrlKey || b e##shiftKey || b e##altKey || b e##metaKey in

  begin (* setup handlers *)
    textbox##onkeyup <-   Dom_html.handler (fun _ -> Lwt.async (resize ~container ~textbox); Js._true);
    textbox##onchange <-  Dom_html.handler (fun _ -> Lwt.async (resize ~container ~textbox); Js._true);
    textbox##onkeydown <- Dom_html.handler (fun e ->
        match e##keyCode with
        | 13 when not (meta e) -> Lwt.async execute; Js._false
        | 13 -> Lwt.async (resize ~container ~textbox); Js._true
        | 09 -> Indent.textarea textbox; Js._false
        | 76 when meta e -> output##innerHTML <- Js.string ""; Js._true
        | 75 when meta e -> setup_toplevel (); Js._false
        | 38 -> history_up e
        | 40 -> history_down e
        | _ -> Js._true
      );
  end;

  Lwt.async_exception_hook:=(fun exc ->
    Format.eprintf "exc during Lwt.async: %s@." (Printexc.to_string exc);
    match exc with
    | Js.Error e -> Firebug.console##log(e##stack)
    | _ -> ());

  Lwt.async (fun () ->
    resize ~container ~textbox () >>= fun () ->
    textbox##focus ();
    Lwt.return_unit);

#ifdef graphics
  Graphics_js.open_canvas (by_id_coerce "test-canvas" Dom_html.CoerceTo.canvas);
#endif

  Sys_js.set_channel_flusher caml_chan  (append Colorize.ocaml output "caml");
  Sys_js.set_channel_flusher sharp_chan (append Colorize.ocaml output "sharp");
  Sys_js.set_channel_flusher stdout     (append Colorize.text  output "stdout");
  Sys_js.set_channel_flusher stderr     (append Colorize.text  output "stderr");

  let readline () =
    Js.Opt.case
      (Dom_html.window##prompt
         (Js.string "The toplevel expects inputs:", Js.string ""))
      (fun () -> "")
      (fun s -> Js.to_string s ^ "\n") in
  Sys_js.set_channel_filler stdin readline;

  setup_share_button ~output;
  setup_examples ~container ~textbox;
  setup_pseudo_fs ();
  setup_toplevel ();
  setup_js_preview ();
  setup_printers ();
  History.setup ();

  textbox##value <- Js.string "";
  (* Run initial code if any *)
  try
    let code = List.assoc "code" (parse_hash ()) in
    textbox##value <- Js.string (B64.decode code);
    Lwt.async execute
  with
  | Not_found -> ()
  | exc -> Firebug.console##log_3(Js.string "exception", Js.string (Printexc.to_string exc), exc)

let _ = Dom_html.window##onload <- Dom_html.handler (fun _ -> run (); Js._false)
