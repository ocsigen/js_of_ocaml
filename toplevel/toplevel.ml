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

exception Bad_magic_number

let exec_magic_number = "Caml1999X008"

let seek_section toc ic name =
  let rec seek_sec curr_ofs = function
    [] -> raise Not_found
  | (n, len) :: rem ->
      if n = name
      then begin seek_in ic (curr_ofs - len); len end
      else seek_sec (curr_ofs - len) rem in
  seek_sec (in_channel_length ic - 16 - 8 * List.length toc) toc

let read_toc ic =
  let pos_trailer = in_channel_length ic - 16 in
  seek_in ic pos_trailer;
  let num_sections = input_binary_int ic in
  let header = String.create(String.length exec_magic_number) in
  really_input ic header 0 (String.length exec_magic_number);
  if header <> exec_magic_number then raise Bad_magic_number;
  seek_in ic (pos_trailer - 8 * num_sections);
  let section_table = ref [] in
  for i = 1 to num_sections do
    let name = String.create 4 in
    really_input ic name 0 4;
    let len = input_binary_int ic in
    section_table := (name, len) :: !section_table
  done;
  !section_table

let split_primitives p =
  let len = String.length p in
  let rec split beg cur =
    if cur >= len then []
    else if p.[cur] = '\000' then
      String.sub p beg (cur - beg) :: split (cur + 1) (cur + 1)
    else
      split beg (cur + 1) in
  Array.of_list(split 0 0)

let read_primitive_table toc ic =
  let len = seek_section toc ic "PRIM" in
  let p = String.create len in
  really_input ic p 0 len;
  split_primitives p

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

(*XXX Integer not needed... *)
  let compile s =
    let p = Parse.from_string prims s in
    let output_program = Driver.f ~standalone:false p in
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

let run _ =
  let body =
    Js.Opt.get (doc##getElementById(Js.string "output"))
      (fun () -> assert false) in

  let textbox = Html.createTextarea doc in
  textbox##rows <- 20; textbox##cols <- 80;
  textbox##value <- Js.string s;
  Dom.appendChild body textbox;
  Dom.appendChild body (Html.createBr doc);
  let disable = ref (fun () -> ()) in
  let b =
    button "Run"
      (fun () ->
         !disable();
   Array.unsafe_set g (-5) (Obj.repr (textbox##value)); (*XXX HACK!*)
        ignore
           (((*Lwt.bind (Lwt_js.yield ()) (fun () ->*)
            begin try Topmain.main() with Not_found -> () end;
            Lwt.return ())))
  in
  disable := (fun () -> b##disabled <- Js._true);
  Dom.appendChild body b;
  Dom.appendChild body (Html.createBr doc);

  Js._false

let _ = Html.window##onload <- Html.handler run
