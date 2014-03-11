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

open Lwt
open Compiler
module Html = Dom_html

module Top : sig
  val setup : (string -> unit) -> unit
  val exec : Format.formatter -> string -> unit
  val initialize : Format.formatter -> unit
end = struct
  let split_primitives p =
    let len = String.length p in
    let rec split beg cur =
      if cur >= len then []
      else if p.[cur] = '\000' then
        String.sub p beg (cur - beg) :: split (cur + 1) (cur + 1)
      else
        split beg (cur + 1) in
    Array.of_list(split 0 0)

  class type global_data = object
    method toc : (string * string) list Js.readonly_prop
    method compile : (string -> string) Js.writeonly_prop
  end

  external global_data : unit -> global_data Js.t = "caml_get_global_data"

  let g = global_data ()

  let setup output =
    Option.Optim.disable "shortvar";
    Option.Optim.enable "pretty";
    let toc = g##toc in
    let prims = split_primitives (List.assoc "PRIM" toc) in

    let compile s =
      let output_program = Driver.from_string prims s in
      let b = Buffer.create 100 in
      output_program (Pretty_print.to_buffer b);
      let res = Buffer.contents b in
      output res;
      res
    in
    g##compile <- compile (*XXX HACK!*)


  let at_bol = ref true
  let consume_nl = ref false

  let refill_lexbuf s p ppf buffer len =
    if !consume_nl then begin
      let l = String.length s in
      if (!p < l && s.[!p] = '\n') then
        incr p
      else if (!p + 1 < l && s.[!p] = '\r' && s.[!p + 1] = '\n') then
        p := !p + 2;
      consume_nl := false
    end;
    if !p = String.length s then
      0
    else begin
      let c = s.[!p] in
      incr p;
      buffer.[0] <- c;
      if !at_bol then Format.fprintf ppf "# ";
      at_bol := (c = '\n');
      if c = '\n' then
        Format.fprintf ppf "@."
      else
        Format.fprintf ppf "%c" c;
      1
    end

  let ensure_at_bol ppf =
    if not !at_bol then begin
      Format.fprintf ppf "@.";
      consume_nl := true; at_bol := true
    end

  let exec' ppf s =
    let lb = Lexing.from_string s in
    try
      List.iter
        (fun phr ->
           if not (Toploop.execute_phrase false ppf phr) then raise Exit)
        (!Toploop.parse_use_file lb)
    with
    | Exit -> ()
    | x    -> Errors.report_error ppf x

  let exec ppf s =
    let lb = Lexing.from_function (refill_lexbuf s (ref 0) ppf) in
    try
      while true do
        try
          let phr = !Toploop.parse_toplevel_phrase lb in
          ensure_at_bol ppf;
          ignore(Toploop.execute_phrase true ppf phr)
        with
          End_of_file ->
          raise End_of_file
        | x ->
          ensure_at_bol ppf;
          Errors.report_error ppf x
      done
    with End_of_file ->
      flush_all ()


  let initialize ppf =
    Toploop.initialize_toplevel_env ();
    Toploop.input_name := "";
    let header = "        Objective Caml version %s@.@." in
    exec' ppf (Printf.sprintf "Format.printf \"%s\" Sys.ocaml_version;;" header)
end


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

let button_type = Js.string "button"
let button id txt =
  let b = Html.createInput ~_type:button_type Html.document in
  b##value <- Js.string txt;
  b##id <- Js.string id;
  b

let by_id s =
  Js.Opt.get (Html.document##getElementById(Js.string s)) (fun () -> failwith (Printf.sprintf "cannot find dom id %S\n%!" s))

exception Next

let examples =
  try
    begin
      let ic = open_in "examples.ml" in
      let ex = ref [] in
      try
        while true do
          let name = input_line ic in
          let content = ref [] in
          try
            while true do
              let l = input_line ic in
              if l = ""
              then raise Next
              else content := l :: !content
            done
          with
          | Next -> ex:=(name,String.concat "\n" (List.rev !content))::!ex
          | End_of_file -> ex:=(name,String.concat "\n" (List.rev !content))::!ex; raise End_of_file
        done;
        List.rev !ex
      with End_of_file -> List.rev !ex
    end
  with  _ -> []
let run _ =
  let container = by_id "toplevel-container" in
  let output = Html.createPre Html.document in
  let sharp = Html.createDiv Html.document in
  let userinput = Html.createDiv Html.document in
  let textbox = Html.createTextarea Html.document in
  let button_exec = button "sendit" "Execute" in

  let side = by_id "toplevel-examples" in
  let ul = Html.createUl Html.document in

  Dom.appendChild container output;
  Dom.appendChild container sharp;
  Dom.appendChild container userinput;
  Dom.appendChild userinput textbox;
  Dom.appendChild userinput button_exec;
  Dom.appendChild side ul;

  output##id <- Js.string "output";
  sharp##id <- Js.string "sharp";
  sharp##innerHTML <- Js.string "#";

  let resize () =
    Lwt.pause () >>= fun () ->
    textbox##style##height <- Js.string "auto";
    textbox##style##height <- Js.string (Printf.sprintf "%dpx" (max 18 textbox##scrollHeight));
    container##scrollTop <- container##scrollHeight;
    Lwt.return () in

  let execute ppf =
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
    Top.exec ppf content;
    textbox##value <- Js.string "";
    resize () >>= fun () ->
    container##scrollTop <- container##scrollHeight;
    textbox##focus();
    Lwt.return_unit in

  List.iter (fun (name,content) ->
      let li = Html.createLi Html.document in
      li##innerHTML <- Js.string name;
      li##onclick <- Html.handler (fun _ ->
          textbox##value <- Js.string content;
          Lwt.async(fun () ->
              resize () >>= fun () ->
              container##scrollTop <- container##scrollHeight;
              textbox##focus();
              Lwt.return_unit);
          Js._true
        );
      Dom.appendChild ul li
    ) examples;

  begin (* setup handlers *)
    button_exec##onclick <- Html.handler (fun _ -> Lwt.async (fun () -> execute Format.std_formatter); Js._false);

    textbox##onkeydown <- Html.handler (fun e ->
        match e##keyCode with
        | 13 ->
          if not (Js.to_bool e##ctrlKey)
          && not (Js.to_bool e##metaKey)
          && not (Js.to_bool e##shiftKey)
          && not (Js.to_bool e##altKey)
          then
            begin
              Lwt.async (fun () -> execute Format.std_formatter);
              Js._false
            end
          else Js._true
        | 09 ->
          textbox##value <- textbox##value##concat (Js.string "   ");
          Js._false
        | 38 ->
          let str = Js.to_string textbox##value in
          if str = ""
          then Js._false
          else Js._true
        | 40 ->
          let str = Js.to_string textbox##value in
          if str = ""
          then Js._false
          else Js._true

        | _ -> Js._true
      );
    textbox##onkeyup <- Html.handler (fun e ->
        Lwt.async resize;Js._true);
    textbox##onchange <- Html.handler (fun _ -> Lwt.async resize; Js._true)
  end;

  let append_string s =
    let span = Html.createSpan Html.document in
    Dom.appendChild span (Html.document##createTextNode(Js.string s ));
    Dom.appendChild output span in

  Sys_js.set_channel_flusher stdout append_string;
  Sys_js.set_channel_flusher stderr append_string;

  Lwt.async (fun () ->
      resize () >>= fun () ->
      textbox##focus ();
      Lwt.return_unit
    );

  Top.setup (fun _ -> ());
  Top.initialize Format.std_formatter

let _ = Html.window##onload <- Html.handler (fun _ -> run (); Js._false)
