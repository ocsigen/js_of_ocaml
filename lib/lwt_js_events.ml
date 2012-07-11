(* Js_of_ocaml library
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2010 Vincent Balat
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

let (>>=) = Lwt.bind

let preventDefault ev = (Js.Unsafe.coerce ev)##preventDefault()

let make_event event_kind ?(use_capture = false) target =
  let el = ref Js.null in
  let t, w = Lwt.task () in
  let cancel () = Js.Opt.iter !el Dom_html.removeEventListener in
  Lwt.on_cancel t cancel;
  el := Js.some
    (Dom_html.addEventListener
       target event_kind
       (Dom_html.handler
          (fun (ev : #Dom_html.event Js.t) ->
            cancel ();
            Lwt.wakeup w ev;
            Js.bool true)) (* true because we do not want to prevent default -> 
                              the user can use the preventDefault function
                              above. *)
       (Js.bool use_capture)
    );
  t

let seq_loop evh ?use_capture target handler =
  let cancelled = ref false in
  let cur = ref (fst (Lwt.task ())) in
  let t, w = Lwt.task () in
  Lwt.on_cancel t (fun () -> Lwt.cancel !cur; cancelled := true);
  let rec aux () =
    if not !cancelled (* In the case it has been cancelled
                         during the previous handler,
                         we do not reinstall the event handler *)
    then
      let t = evh ?use_capture target in
      cur := t;
      t >>=
      handler >>= fun () ->
      aux ()
    else Lwt.return ()
  in
  ignore (aux ());
  t

let click ?use_capture target =
  make_event Dom_html.Event.click ?use_capture target
let dblclick ?use_capture target =
  make_event Dom_html.Event.dblclick ?use_capture target
let mousedown ?use_capture target =
  make_event Dom_html.Event.mousedown ?use_capture target
let mouseup ?use_capture target =
  make_event Dom_html.Event.mouseup ?use_capture target
let mouseover ?use_capture target =
  make_event Dom_html.Event.mouseover ?use_capture target
let mousemove ?use_capture target =
  make_event Dom_html.Event.mousemove ?use_capture target
let mouseout ?use_capture target =
  make_event Dom_html.Event.mouseout ?use_capture target

let keypress ?use_capture target =
  make_event Dom_html.Event.keypress ?use_capture target
let keydown ?use_capture target =
  make_event Dom_html.Event.keydown ?use_capture target
let keyup ?use_capture target =
  make_event Dom_html.Event.keyup ?use_capture target

let dragstart ?use_capture target =
  make_event Dom_html.Event.dragstart ?use_capture target
let dragend ?use_capture target =
  make_event Dom_html.Event.dragend ?use_capture target
let dragenter ?use_capture target =
  make_event Dom_html.Event.dragenter ?use_capture target
let dragover ?use_capture target =
  make_event Dom_html.Event.dragover ?use_capture target
let dragleave ?use_capture target =
  make_event Dom_html.Event.dragleave ?use_capture target
let drag ?use_capture target =
  make_event Dom_html.Event.drag ?use_capture target
let drop ?use_capture target =
  make_event Dom_html.Event.drop ?use_capture target

(* special case for mousewheel, because it depends on the browser *)
let mousewheel ?(use_capture=false) target =
  let el = ref Js.null in
  let t, w = Lwt.task () in
  let cancel () = Js.Opt.iter !el Dom_html.removeEventListener in
  Lwt.on_cancel t cancel;
  el := Js.some
    (Dom_html.addMousewheelEventListener
       target
       (fun (ev : #Dom_html.event Js.t) ~dx ~dy ->
         Firebug.console##log(ev);
         cancel ();
         Lwt.wakeup w (ev, (dx, dy));
         Js.bool true) (* true because we do not want to prevent default -> 
                           the user can use the preventDefault function
                           above. *)
       (Js.bool use_capture)
    );
  t

(* let _DOMMouseScroll ?use_capture target =
  make_event Dom_html.Event._DOMMouseScroll ?use_capture target
*)

let touchstart ?use_capture target =
  make_event Dom_html.Event.touchstart ?use_capture target
let touchmove ?use_capture target =
  make_event Dom_html.Event.touchmove ?use_capture target
let touchend ?use_capture target =
  make_event Dom_html.Event.touchend ?use_capture target
let touchcancel ?use_capture target =
  make_event Dom_html.Event.touchcancel ?use_capture target



let clicks ?use_capture t =
  seq_loop click ?use_capture t
let dblclicks ?use_capture t =
  seq_loop dblclick ?use_capture t
let mousedowns ?use_capture t =
  seq_loop mousedown ?use_capture t
let mouseups ?use_capture t =
  seq_loop mouseup ?use_capture t
let mouseovers ?use_capture t =
  seq_loop mouseover ?use_capture t
let mousemoves ?use_capture t =
  seq_loop mousemove ?use_capture t
let mouseouts ?use_capture t =
  seq_loop mouseout ?use_capture t

let keypresses ?use_capture t =
  seq_loop keypress ?use_capture t
let keydowns ?use_capture t =
  seq_loop keydown ?use_capture t
let keyups ?use_capture t =
  seq_loop keyup ?use_capture t

let dragstarts ?use_capture t =
  seq_loop dragstart ?use_capture t
let dragends ?use_capture t =
  seq_loop dragend ?use_capture t
let dragenters ?use_capture t =
  seq_loop dragenter ?use_capture t
let dragovers ?use_capture t =
  seq_loop dragover ?use_capture t
let dragleaves ?use_capture t =
  seq_loop dragleave ?use_capture t
let drags ?use_capture t =
  seq_loop drag ?use_capture t
let drops ?use_capture t =
  seq_loop drop ?use_capture t

let mousewheels ?use_capture t =
  seq_loop mousewheel ?use_capture t

let touchstarts ?use_capture t =
  seq_loop touchstart ?use_capture t
let touchmoves ?use_capture t =
  seq_loop touchmove ?use_capture t
let touchends ?use_capture t =
  seq_loop touchend ?use_capture t
let touchcancels ?use_capture t =
  seq_loop touchcancel ?use_capture t


