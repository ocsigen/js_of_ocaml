(* Js_of_ocaml library
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2014 Hugo Heuzard
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

open Js_of_ocaml
open Js_of_ocaml_lwt
open! Import
include Graphics

class type context_ =
  object
    method canvas : Dom_html.canvasElement Js.t Js.readonly_prop
  end

type context = context_ Js.t

let _ = Callback.register_exception "Graphics.Graphic_failure" (Graphic_failure "")

let ( >>= ) = Lwt.bind

external get_context : unit -> context = "caml_gr_state_get"

external set_context : context -> unit = "caml_gr_state_set"

external create_context : Dom_html.canvasElement Js.t -> int -> int -> context
  = "caml_gr_state_create"

external document_of_context : context -> Dom_html.document Js.t = "caml_gr_doc_of_state"

let open_canvas x =
  let ctx = create_context x x##.width x##.height in
  set_context ctx

let compute_real_pos (elt : #Dom_html.element Js.t) ev =
  let r = elt##getBoundingClientRect in
  let x =
    (float_of_int ev##.clientX -. Js.to_float r##.left)
    /. (Js.to_float r##.right -. Js.to_float r##.left)
    *. float_of_int elt##.width
  in
  let y =
    (float_of_int ev##.clientY -. Js.to_float r##.top)
    /. (Js.to_float r##.bottom -. Js.to_float r##.top)
    *. float_of_int elt##.height
  in
  truncate x, elt##.height - truncate y

let mouse_pos () =
  let ctx = get_context () in
  let elt = ctx##.canvas in
  Lwt_js_events.mousemove elt >>= fun ev -> Lwt.return (compute_real_pos elt ev)

let button_down () =
  let ctx = get_context () in
  let elt = ctx##.canvas in
  Lwt_js_events.mousedown elt >>= fun _ev -> Lwt.return true

let read_key () =
  (* let ctx = get_context() in *)
  (* let elt = ctx##canvas in *)
  let doc = document_of_context (get_context ()) in
  Lwt_js_events.keypress doc >>= fun ev -> Lwt.return (Char.chr ev##.keyCode)

let loop elist f : unit =
  let ctx = get_context () in
  let elt = ctx##.canvas in
  let doc = document_of_context (get_context ()) in
  let button = ref false in
  let null = char_of_int 0 in
  let mouse_x, mouse_y = ref 0, ref 0 in
  let get_pos_mouse () = !mouse_x, !mouse_y in
  if List.mem Button_down elist
  then
    elt##.onmousedown :=
      Dom_html.handler (fun _ev ->
          let mouse_x, mouse_y = get_pos_mouse () in
          button := true;
          let s = { mouse_x; mouse_y; button = true; keypressed = false; key = null } in
          f s;
          Js._true);
  if List.mem Button_up elist
  then
    elt##.onmouseup :=
      Dom_html.handler (fun _ev ->
          let mouse_x, mouse_y = get_pos_mouse () in
          button := false;
          let s = { mouse_x; mouse_y; button = false; keypressed = false; key = null } in
          f s;
          Js._true);
  elt##.onmousemove :=
    Dom_html.handler (fun ev ->
        let cx, cy = compute_real_pos (elt :> #Dom_html.element Js.t) ev in
        mouse_x := cx;
        mouse_y := cy;
        (if List.mem Mouse_motion elist
         then
           let mouse_x, mouse_y = get_pos_mouse () in
           let s =
             { mouse_x; mouse_y; button = !button; keypressed = false; key = null }
           in
           f s);
        Js._true);
  (* EventListener sur le doc car pas de moyen simple de le faire
     sur un canvasElement *)
  if List.mem Key_pressed elist
  then
    doc##.onkeypress :=
      Dom_html.handler (fun ev ->
          (* Uncaught Invalid_argument char_of_int with key € for example *)
          let key =
            try char_of_int (Js.Optdef.get ev##.charCode (fun _ -> 0))
            with Invalid_argument _ -> null
          in
          let mouse_x, mouse_y = get_pos_mouse () in
          let s = { mouse_x; mouse_y; button = !button; keypressed = true; key } in
          f s;
          Js._true)

let loop_at_exit events handler : unit = at_exit (fun _ -> loop events handler)
