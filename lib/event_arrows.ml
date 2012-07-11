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

(** Events with arrows. *)

let (>>=) = Lwt.bind
let (>|=) = Lwt.(>|=)

type canceller = (unit -> unit) option ref (* canceller function *)
type ('a, 'b) t = 'a -> canceller -> ('b * canceller) Lwt.t
let lwt_arr f = fun x c -> f x >>= fun r -> Lwt.return (r, c)
let arr f = fun x c -> Lwt.return (f x, c)
let (>>>) f g = fun x c -> f x c >>= fun (y, c) -> g y c
let (>>>|) f g = f >>> lwt_arr g
let run (a : ('a, 'b) t) x = let c = ref None in ignore (a x c); c

let set_canceller clr c = clr := Some c

let cancel c = match !c with
  | None -> ()
  | Some f -> f ()

let make_event eventkind
    ?(use_capture = false) ?(keep_default = false) ?(propagate = false)
    (target : #Dom_html.eventTarget Js.t) _ c =
  let el = ref Js.null in
  let t, w = Lwt.wait () in
  let cancel () = Js.Opt.iter !el Dom_html.removeEventListener in
  set_canceller c cancel;
  el := Js.some
    (Dom_html.addEventListener
       target eventkind
       (Dom_html.handler
          (fun (ev : #Dom_html.event Js.t) ->
            if not propagate
            then Dom_html.stopPropagation ev;
            cancel ();
            Lwt.wakeup w (ev, c);
            Js.bool keep_default))
       (Js.bool use_capture)
    );
  t

let rec loop_event f ?use_capture ?keep_default ?propagate target handler x c =
  (f ?use_capture ?keep_default ?propagate target >>> handler) () c
  >>= fun (y, c) ->
  loop_event f ?use_capture ?keep_default ?propagate target handler x c

(*  let rec loop f handler = f >>> handler >>> loop f handler *)

let make_state eventkind
    ?(use_capture = false) ?(keep_default = false) ?(propagate = false)
    (target : #Dom_html.eventTarget Js.t) handler _ c =
  let el = ref Js.null in
  let c1 = ref None in
  let cancel0 () = cancel c1; Js.Opt.iter !el Dom_html.removeEventListener in
  set_canceller c cancel0;
  let locked = ref false in
  let state = ref None in
  let rec f (ev : #Dom_html.event Js.t) =
    if !locked
    then state := Some ev (* We keep the more recent state during the handler *)
    else begin
      locked := true;
      ignore (handler ev c1 >|= fun r -> 
              locked := false;
              match !state with
                | None -> ()
                | Some ev -> state := None; f ev);
    end
  in
  el := Js.some
    (Dom_html.addEventListener
       target eventkind
       (Dom_html.handler (fun ev -> 
         if not propagate
         then Dom_html.stopPropagation ev;
         f ev;
         Js.bool keep_default))
       (Js.bool use_capture)
    );
  fst (Lwt.wait ())

let first l x c =
  let cancellers = ref [] in
  let cancel () = List.iter cancel !cancellers in
  set_canceller c cancel;
  let t, w = Lwt.wait () in
  let f x c0 =
    cancel ();
    Lwt.wakeup w (x, c);
    Lwt.return (x, c0)
  in
  cancellers := List.map (fun e -> run (e >>> f) x) l;
  t

let rec iter l x c =
  first l x c >>= fun (y, c) ->
  iter l x c


let click ?use_capture ?keep_default ?propagate t a c =
  make_event Dom_html.Event.click ?use_capture ?keep_default ?propagate t a c
let dblclick ?use_capture ?keep_default ?propagate t a c =
  make_event Dom_html.Event.dblclick ?use_capture ?keep_default ?propagate t a c
let mousedown ?use_capture ?keep_default ?propagate t a c =
  make_event Dom_html.Event.mousedown ?use_capture ?keep_default ?propagate t a c
let mouseup ?use_capture ?keep_default ?propagate t a c =
  make_event Dom_html.Event.mouseup ?use_capture ?keep_default ?propagate t a c
let mouseover ?use_capture ?keep_default ?propagate t a c =
  make_event Dom_html.Event.mouseover ?use_capture ?keep_default ?propagate t a c
let mousemove ?use_capture ?keep_default ?propagate t a c =
  make_event Dom_html.Event.mousemove ?use_capture ?keep_default ?propagate t a c
let mouseout ?use_capture ?keep_default ?propagate t a c =
  make_event Dom_html.Event.mouseout ?use_capture ?keep_default ?propagate t a c

let keypress ?use_capture ?keep_default ?propagate t a c =
  make_event Dom_html.Event.keypress ?use_capture ?keep_default ?propagate t a c
let keydown ?use_capture ?keep_default ?propagate t a c =
  make_event Dom_html.Event.keydown ?use_capture ?keep_default ?propagate t a c
let keyup ?use_capture ?keep_default ?propagate t a c =
  make_event Dom_html.Event.keyup ?use_capture ?keep_default ?propagate t a c

(* TODO: implement with Dom_html.addMousewheelEventListener
let mousewheel ?use_capture ?keep_default ?propagate t a c =
  make_event Dom_html.Event.mousewheel ?use_capture ?keep_default ?propagate t a c
*)

let clicks ?use_capture ?keep_default ?propagate t =
  loop_event click ?use_capture ?keep_default ?propagate t
let dblclicks ?use_capture ?keep_default ?propagate t =
  loop_event dblclick ?use_capture ?keep_default ?propagate t
let mousedowns ?use_capture ?keep_default ?propagate t =
  make_state Dom_html.Event.mousedown ?use_capture ?keep_default ?propagate t
let mouseups ?use_capture ?keep_default ?propagate t =
  make_state Dom_html.Event.mouseup ?use_capture ?keep_default ?propagate t
let mouseovers ?use_capture ?keep_default ?propagate t =
  loop_event mouseover ?use_capture ?keep_default ?propagate t
let mousemoves ?use_capture ?keep_default ?propagate t =
  make_state Dom_html.Event.mousemove ?use_capture ?keep_default ?propagate t
let mouseouts ?use_capture ?keep_default ?propagate t =
  loop_event mouseout ?use_capture ?keep_default ?propagate t
(*VVV make_state? *)

let keypresses ?use_capture ?keep_default ?propagate t =
  loop_event keypress ?use_capture ?keep_default ?propagate t
let keydowns ?use_capture ?keep_default ?propagate t =
  make_state Dom_html.Event.keydown ?use_capture ?keep_default ?propagate t
let keyups ?use_capture ?keep_default ?propagate t =
  make_state Dom_html.Event.keyup ?use_capture ?keep_default ?propagate t

(* TODO: implement with Dom_html.addMousewheelEventListener
let mousewheels ?use_capture ?keep_default ?propagate t =
  loop_event mousewheel ?use_capture ?keep_default ?propagate t
*)


