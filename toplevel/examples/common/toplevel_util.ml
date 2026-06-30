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

(* DOM helpers shared by the toplevel examples. *)

open Js_of_ocaml
open Js_of_ocaml_tyxml

let by_id s = Dom_html.getElementById s

let by_id_coerce s f =
  Js.Opt.get (f (Dom_html.getElementById s)) (fun () -> raise Not_found)

let do_by_id s f = try f (Dom_html.getElementById s) with Not_found -> ()

(* Apply [f] to [x] and every following sibling that carries the [sharp] class
   (the echoed input lines). *)
let rec iter_on_sharp ~f x =
  Js.Opt.iter (Dom_html.CoerceTo.element x) (fun e ->
      if Js.to_bool (e##.classList##contains (Js.string "sharp")) then f e);
  match Js.Opt.to_option x##.nextSibling with
  | None -> ()
  | Some n -> iter_on_sharp ~f n

(* Append [s] to [output], styled by [colorize] (syntax-coloured OCaml or plain
   text) with class [cl]. *)
let append colorize output cl s =
  Dom.appendChild output (Tyxml_js.To_dom.of_element (colorize ~a_class:cl s))

(* Grow the input textarea to fit its content and scroll the container to the
   bottom. Synchronous, so it takes effect right after the value is set. *)
let fit ~container ~textbox =
  textbox##.style##.height := Js.string "auto";
  textbox##.style##.height
  := Js.string (Printf.sprintf "%dpx" (max 18 textbox##.scrollHeight));
  container##.scrollTop := Js.float (float container##.scrollHeight)

(* Same as [fit], deferred one tick so the textarea has reflowed (e.g. after a
   keystroke). *)
let resize ~container ~textbox () =
  Lwt.bind (Lwt.pause ()) (fun () ->
      fit ~container ~textbox;
      Lwt.return ())

(* Wire the standard toplevel input behaviour onto [textbox]: auto-resize,
   history browsing (Up/Down), Tab to indent, Enter to [execute] (Shift/Ctrl
   inserts a newline), Ctrl+L to clear [output] and Ctrl+K to [reset]. The two
   things that differ between the toplevels are passed in: [execute] runs the
   current input, [reset] re-initializes the toplevel. *)
let setup_input_handlers ~container ~textbox ~output ~execute ~reset =
  let history_down _e =
    let txt = Js.to_string textbox##.value in
    let pos = textbox##.selectionStart in
    try
      if String.length txt = pos then raise Not_found;
      let _ = String.index_from txt pos '\n' in
      Js._true
    with Not_found ->
      History.current txt;
      History.next textbox;
      Js._false
  in
  let history_up _e =
    let txt = Js.to_string textbox##.value in
    let pos = textbox##.selectionStart - 1 in
    try
      if pos < 0 then raise Not_found;
      let _ = String.rindex_from txt pos '\n' in
      Js._true
    with Not_found ->
      History.current txt;
      History.previous textbox;
      Js._false
  in
  let meta e =
    let b = Js.to_bool in
    b e##.ctrlKey || b e##.altKey || b e##.metaKey
  in
  let shift e = Js.to_bool e##.shiftKey in
  textbox##.onkeyup :=
    Dom_html.handler (fun _ ->
        Lwt.async (resize ~container ~textbox);
        Js._true);
  textbox##.onchange :=
    Dom_html.handler (fun _ ->
        Lwt.async (resize ~container ~textbox);
        Js._true);
  textbox##.onkeydown :=
    Dom_html.handler (fun e ->
        match e##.keyCode with
        | 13 when not (meta e || shift e) ->
            Lwt.async execute;
            Js._false
        | 13 ->
            Lwt.async (resize ~container ~textbox);
            Js._true
        | 09 ->
            Indent.textarea textbox;
            Js._false
        | 76 when meta e ->
            output##.innerHTML := Js.string "";
            Js._false
        | 75 when meta e ->
            reset ();
            Js._false
        | 38 -> history_up e
        | 40 -> history_down e
        | _ -> Js._true)

(* Underline the part of [loc] in the echoed source, whose lines are the [sharp]
   elements reachable from [first]. The caller picks [first]: the first echoed
   node of the phrase. *)
let highlight_location ~first (loc : Js_of_ocaml_toplevel_protocol.Wrapped_intf.loc) =
  let module W = Js_of_ocaml_toplevel_protocol.Wrapped_intf in
  let x = ref 0 in
  iter_on_sharp first ~f:(fun e ->
      incr x;
      let line1, col1 = loc.W.loc_start in
      let line2, col2 = loc.W.loc_end in
      if !x >= line1 && !x <= line2
      then
        let from_ = if !x = line1 then `Pos col1 else `Pos 0 in
        let to_ = if !x = line2 then `Pos col2 else `Last in
        Colorize.highlight from_ to_ e)

(* Fill the [container_id] sidebar from [content] (the examples source): each
   [(* title *)] comment starts a clickable snippet of the lines below it.
   [on_pick] receives the snippet's source when the entry is clicked. *)
let setup_examples ~container_id ~on_pick content =
  let r = Regexp.regexp "^\\(\\*+(.*)\\*+\\)$" in
  let all = ref [] in
  List.iter
    (fun line ->
      match Regexp.string_match r line 0 with
      | Some res ->
          let name =
            match Regexp.matched_group res 1 with
            | Some s -> s
            | None -> ""
          in
          all := `Title name :: !all
      | None -> all := `Content line :: !all)
    (String.split_on_char '\n' content);
  let example_container = by_id container_id in
  ignore
    (List.fold_left
       (fun acc tok ->
         match tok with
         | `Content line -> line ^ "\n" ^ acc
         | `Title name ->
             let a =
               Tyxml_js.Html.(
                 a
                   ~a:
                     [ a_class [ "list-group-item" ]
                     ; a_onclick (fun _ ->
                           on_pick acc;
                           true)
                     ]
                   [ txt name ])
             in
             Dom.appendChild example_container (Tyxml_js.To_dom.of_a a);
             "")
       ""
       !all
      : string)
