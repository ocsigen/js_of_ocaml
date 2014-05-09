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

include Graphics

type context
let _ = Callback.register_exception "Graphics.Graphic_failure" (Graphic_failure "")

let (>>=) = Lwt.bind

let get_context () =
  Js.Unsafe.(fun_call (variable "caml_gr_state_get") [| |])

let set_context ctx =
  Js.Unsafe.(fun_call (variable "caml_gr_state_set") [| inject ctx |])

let create_context canvas w h =
  Js.Unsafe.(fun_call (variable "caml_gr_state_create")
               [| inject canvas; inject w; inject h|])

let document_of_context ctx =
  Js.Unsafe.(fun_call (variable "caml_gr_doc_of_state") [| inject ctx |])

let open_canvas x =
  let ctx = create_context x x##width x##height in
  set_context ctx

let compute_real_pos elt =
  let rec loop elt left top =
    let top = elt##offsetTop - elt##scrollTop + top
    and left = elt##offsetLeft - elt##scrollLeft + left in
    match Js.Opt.to_option elt##offsetParent with
    | None -> top,left
    | Some p -> loop p left top
  in loop elt 0 0

let mouse_pos () =
  let ctx = get_context() in
  let elt = ctx##canvas in
  Lwt_js_events.mousemove elt >>= fun ev ->
  let top,left = compute_real_pos elt in
  Lwt.return ((Js.Optdef.get (ev##pageX) (fun _ -> 0)) - left,
              elt##height - ((Js.Optdef.get (ev##pageY) (fun _ -> 0)) - top))

let button_down () =
  let ctx = get_context() in
  let elt = ctx##canvas in
  Lwt_js_events.mousedown elt >>= fun e ->
  Lwt.return true

let read_key () =
  (* let ctx = get_context() in *)
  (* let elt = ctx##canvas in *)
  let doc = document_of_context (get_context ()) in
  Lwt_js_events.keypress doc >>= fun e ->
  Lwt.return (Char.chr e##keyCode)

let loop elist f : unit =
  let ctx = get_context() in
  let elt = ctx##canvas in
  let doc = document_of_context (get_context ()) in
  let button = ref false in
  let null = char_of_int 0 in
  let mouse_x, mouse_y = ref 0, ref 0 in

  let get_pos_mouse () = !mouse_x, !mouse_y in

  if List.mem Button_down elist then
    elt##onmousedown <- Dom_html.handler (fun ev ->
        let mouse_x, mouse_y = get_pos_mouse () in
        button := true;
        let s = { mouse_x ; mouse_y ; button=true ;
		              keypressed=false ; key=null } in
        f s;
        Js._true);

  if List.mem Button_up elist then
    elt##onmouseup <- Dom_html.handler (fun ev ->
        let mouse_x, mouse_y = get_pos_mouse () in
        button := false;
        let s = { mouse_x ; mouse_y ; button=false ;
		              keypressed=false ; key=null } in
        f s;
        Js._true);


  elt##onmousemove <- Dom_html.handler (fun ev ->
      let cy,cx = compute_real_pos elt in
      mouse_x := (Js.Optdef.get (ev##pageX) (fun _ -> 0)) - cx;
      mouse_y := elt##height - (Js.Optdef.get (ev##pageY) (fun _ -> 0) - cy);
      if List.mem Mouse_motion elist then
        (let mouse_x, mouse_y = get_pos_mouse () in
         let s = { mouse_x ; mouse_y ; button=(!button) ;
		               keypressed=false ; key=null } in
         f s);
      Js._true);

  (* EventListener sur le doc car pas de moyen simple de le faire
     sur un canvasElement *)
  if List.mem Key_pressed elist then
    doc##onkeypress <- Dom_html.handler (fun ev ->
        (* Uncaught Invalid_argument char_of_int with key € for example *)
        let key =
	        try char_of_int (Js.Optdef.get (ev##charCode) (fun _ -> 0))
	        with Invalid_argument _ -> null in
        let mouse_x, mouse_y = get_pos_mouse () in
        let s = { mouse_x ; mouse_y ; button=(!button) ;
		              keypressed=true ; key } in
        f s;
        Js._true)

let loop_at_exit events handler : unit =
  at_exit (fun _ -> loop events handler)
