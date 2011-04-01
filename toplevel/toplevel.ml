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

(*
TODO
- different style for toplevel input, output and errors
  in particular, the prompt should not be part of the text
  (CSS generated content)
- syntax highlighting?
*)

let split_primitives p =
  let len = String.length p in
  let rec split beg cur =
    if cur >= len then []
    else if p.[cur] = '\000' then
      String.sub p beg (cur - beg) :: split (cur + 1) (cur + 1)
    else
      split beg (cur + 1) in
  Array.of_list(split 0 0)

(****)

external global_data : unit -> Obj.t array = "caml_get_global_data"

let g = global_data ()

let _ =
(*
  Util.set_debug "parser";
  Util.set_debug "deadcode";
  Util.set_debug "main";
*)
  let toc = Obj.magic (Array.unsafe_get g (-2)) in
  let prims = split_primitives (List.assoc "PRIM" toc) in

  let compile s =
    let output_program = Driver.from_string prims s in
    let b = Buffer.create 100 in
    output_program (Format.formatter_of_buffer b);
    Buffer.contents b
  in
  Array.unsafe_set g (-3) (Obj.repr compile); (*XXX HACK!*)

module Html = Dom_html

let s =
  "let x = 10+10;;\n\
   let y = x * 3;;\n\
   String.make x 'a';;\n\
   sin 1.;;\n\
   let rec fact n = if n = 0 then 1. else float n *. fact (n - 1);;\n\
   fact 20;;\n"

let doc = Dom_html.document
let button_type = Js.string "button"
let button txt action =
  let b = Dom_html.createInput ~_type:button_type doc in
  b##value <- Js.string txt;
  b##onclick <- Dom_html.handler (fun _ -> action (); Js._true);
  b

let start ppf =
  Format.fprintf ppf "        Objective Caml version %s@.@." Sys.ocaml_version;
  Toploop.initialize_toplevel_env ();
  Toploop.input_name := ""

let at_bol = ref true
let consume_nl = ref false

let rec refill_lexbuf buffer len =
  try
    let c = input_char Pervasives.stdin in
    if !consume_nl && c = '\n' then begin
      consume_nl := false;
      refill_lexbuf buffer len
    end else begin
      consume_nl := false;
      buffer.[0] <- c;
      if !at_bol then output_string stdout "# ";
      at_bol := (c = '\n');
      output_char stdout c;
      1
    end
  with End_of_file ->
    0

let ensure_at_bol () =
  if not !at_bol then begin
    output_string stdout "\n";
    consume_nl := true; at_bol := true
  end

let loop ppf =
  let lb = Lexing.from_function refill_lexbuf in
  while true do
    try
      let phr = !Toploop.parse_toplevel_phrase lb in
      ensure_at_bol ();
      ignore(Toploop.execute_phrase true ppf phr)
    with
    | End_of_file ->
        exit 0
    | x ->
        ensure_at_bol ();
        Errors.report_error ppf x
  done

let run _ =
  let top =
    Js.Opt.get (doc##getElementById(Js.string "toplevel"))
      (fun () -> assert false) in
  let output = Html.createDiv doc in
  output##id <- Js.string "output";
  output##style##whiteSpace <- Js.string "pre";
  Dom.appendChild top output;

  let textbox = Html.createTextarea doc in
  textbox##rows <- 10; textbox##cols <- 80;
  textbox##value <- Js.string s;
  Dom.appendChild top textbox;
  Dom.appendChild top (Html.createBr doc);

  textbox##focus(); textbox##select();
  let b =
    button "Send"
      (fun () ->
        Array.unsafe_set g (-5) (Obj.repr (textbox##value)); (*XXX HACK!*)
        ignore
           (((*Lwt.bind (Lwt_js.yield ()) (fun () ->*)
            begin try loop Format.std_formatter with Not_found -> () end;
            textbox##focus(); textbox##select();
            doc##documentElement##scrollTop <- doc##body##scrollHeight;
            Lwt.return ())))
  in
  Dom.appendChild top b;
  start Format.std_formatter;

  Js._false

let _ = Html.window##onload <- Html.handler run
