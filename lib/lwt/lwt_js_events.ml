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

open Js_of_ocaml
open! Import

let ( >>= ) = Lwt.bind

let async f = Lwt.async (fun () -> Lwt_js.yield () >>= f)

let opt_map f = function
  | None -> None
  | Some x -> Some (f x)

let make_event event_kind ?use_capture ?passive target =
  let el = ref Js.null in
  let t, w = Lwt.task () in
  let cancel () = Js.Opt.iter !el Dom_html.removeEventListener in
  Lwt.on_cancel t cancel;
  el :=
    Js.some
      (Dom.addEventListenerWithOptions
         ?capture:(opt_map Js.bool use_capture)
         ?passive:(opt_map Js.bool passive)
         target
         event_kind
         (Dom_html.handler (fun (ev : #Dom_html.event Js.t) ->
              cancel ();
              Lwt.wakeup w ev;
              Js.bool true))
         (* true because we do not want to prevent default ->
                              the user can use the preventDefault function
                              above. *));
  t

let catch_cancel f x =
  Lwt.catch
    (fun () -> f x)
    (function
      | Lwt.Canceled -> Lwt.return ()
      | e -> Lwt.fail e)

let with_error_log f x =
  Lwt.catch
    (fun () -> f x)
    (fun e ->
      Firebug.console##log (Js.string (Printexc.to_string e));
      Lwt.return ())

let seq_loop evh ?(cancel_handler = false) ?use_capture ?passive target handler =
  let cancelled = ref false in
  let cur = ref (Lwt.fail (Failure "Lwt_js_event")) in
  (* Using Lwt.fail as default, to be polymorphic *)
  let cur_handler = ref (Lwt.return ()) in
  let lt, _lw = Lwt.task () in
  Lwt.on_cancel lt (fun () ->
      Lwt.cancel !cur;
      if cancel_handler then Lwt.cancel !cur_handler;
      cancelled := true);
  let rec aux () =
    if
      not !cancelled
      (* In the case it has been cancelled
                         during the previous handler,
                         we do not reinstall the event handler *)
    then (
      let t = evh ?use_capture ?passive target in
      cur := t;
      t
      >>= fun e ->
      cur_handler := with_error_log (handler e) lt;
      !cur_handler >>= aux)
    else Lwt.return ()
  in
  Lwt.async (catch_cancel aux);
  lt

let async_loop evh ?use_capture ?passive target handler =
  let cancelled = ref false in
  let cur = ref (Lwt.fail (Failure "Lwt_js_event")) in
  let lt, _lw = Lwt.task () in
  Lwt.on_cancel lt (fun () ->
      Lwt.cancel !cur;
      cancelled := true);
  let rec aux () =
    if not !cancelled
    then (
      let t = evh ?use_capture ?passive target in
      cur := t;
      t
      >>= fun e ->
      Lwt.async (fun () -> with_error_log (handler e) lt);
      aux ())
    else Lwt.return ()
  in
  Lwt.async (catch_cancel aux);
  lt

let buffered_loop
    evh
    ?(cancel_handler = false)
    ?(cancel_queue = true)
    ?use_capture
    ?passive
    target
    handler =
  let cancelled = ref false in
  let queue = ref [] in
  let cur = ref (Lwt.fail (Failure "Lwt_js_event")) in
  let cur_handler = ref (Lwt.return ()) in
  let lt, _lw = Lwt.task () in
  let spawn = Lwt_condition.create () in
  Lwt.on_cancel lt (fun () ->
      Lwt.cancel !cur;
      if cancel_handler then Lwt.cancel !cur_handler;
      if cancel_queue then queue := [];
      cancelled := true);
  let rec spawner () =
    if not !cancelled
    then (
      let t = evh ?use_capture ?passive target in
      cur := t;
      t
      >>= fun e ->
      queue := e :: !queue;
      Lwt_condition.signal spawn ();
      spawner ())
    else Lwt.return ()
  in
  let rec runner () =
    cur_handler := Lwt.return ();
    if not !cancelled
    then (
      match !queue with
      | [] -> Lwt_condition.wait spawn >>= runner
      | e :: tl ->
          queue := tl;
          cur_handler := with_error_log (handler e) lt;
          !cur_handler >>= runner)
    else Lwt.return ()
  in
  Lwt.async (catch_cancel spawner);
  Lwt.async runner;
  lt

let func_limited_loop event limited_func ?use_capture ?passive target handler =
  let count = ref 0 in
  async_loop event ?use_capture ?passive target (fun ev lt ->
      incr count;
      let nb = !count in
      limited_func () >>= fun _ -> if !count = nb then handler ev lt else Lwt.return ())

let limited_loop event ?(elapsed_time = 0.1) =
  func_limited_loop event (fun () -> Lwt_js.sleep elapsed_time)

let click ?use_capture ?passive target =
  make_event Dom_html.Event.click ?use_capture ?passive target

let copy ?use_capture ?passive target =
  make_event Dom_html.Event.copy ?use_capture ?passive target

let cut ?use_capture ?passive target =
  make_event Dom_html.Event.cut ?use_capture ?passive target

let paste ?use_capture ?passive target =
  make_event Dom_html.Event.paste ?use_capture ?passive target

let dblclick ?use_capture ?passive target =
  make_event Dom_html.Event.dblclick ?use_capture ?passive target

let mousedown ?use_capture ?passive target =
  make_event Dom_html.Event.mousedown ?use_capture ?passive target

let mouseup ?use_capture ?passive target =
  make_event Dom_html.Event.mouseup ?use_capture ?passive target

let mouseover ?use_capture ?passive target =
  make_event Dom_html.Event.mouseover ?use_capture ?passive target

let mousemove ?use_capture ?passive target =
  make_event Dom_html.Event.mousemove ?use_capture ?passive target

let mouseout ?use_capture ?passive target =
  make_event Dom_html.Event.mouseout ?use_capture ?passive target

let keypress ?use_capture ?passive target =
  make_event Dom_html.Event.keypress ?use_capture ?passive target

let keydown ?use_capture ?passive target =
  make_event Dom_html.Event.keydown ?use_capture ?passive target

let keyup ?use_capture ?passive target =
  make_event Dom_html.Event.keyup ?use_capture ?passive target

let change ?use_capture ?passive target =
  make_event Dom_html.Event.change ?use_capture ?passive target

let input ?use_capture ?passive target =
  make_event Dom_html.Event.input ?use_capture ?passive target

let timeupdate ?use_capture ?passive target =
  make_event Dom_html.Event.timeupdate ?use_capture ?passive target

let dragstart ?use_capture ?passive target =
  make_event Dom_html.Event.dragstart ?use_capture ?passive target

let dragend ?use_capture ?passive target =
  make_event Dom_html.Event.dragend ?use_capture ?passive target

let dragenter ?use_capture ?passive target =
  make_event Dom_html.Event.dragenter ?use_capture ?passive target

let dragover ?use_capture ?passive target =
  make_event Dom_html.Event.dragover ?use_capture ?passive target

let dragleave ?use_capture ?passive target =
  make_event Dom_html.Event.dragleave ?use_capture ?passive target

let drag ?use_capture ?passive target =
  make_event Dom_html.Event.drag ?use_capture ?passive target

let drop ?use_capture ?passive target =
  make_event Dom_html.Event.drop ?use_capture ?passive target

let focus ?use_capture ?passive target =
  make_event Dom_html.Event.focus ?use_capture ?passive target

let blur ?use_capture ?passive target =
  make_event Dom_html.Event.blur ?use_capture ?passive target

let scroll ?use_capture ?passive target =
  make_event Dom_html.Event.scroll ?use_capture ?passive target

let submit ?use_capture ?passive target =
  make_event Dom_html.Event.submit ?use_capture ?passive target

let select ?use_capture ?passive target =
  make_event Dom_html.Event.select ?use_capture ?passive target

let abort ?use_capture ?passive target =
  make_event Dom_html.Event.abort ?use_capture ?passive target

let error ?use_capture ?passive target =
  make_event Dom_html.Event.error ?use_capture ?passive target

let load ?use_capture ?passive target =
  make_event Dom_html.Event.load ?use_capture ?passive target

let canplay ?use_capture ?passive target =
  make_event Dom_html.Event.canplay ?use_capture ?passive target

let canplaythrough ?use_capture ?passive target =
  make_event Dom_html.Event.canplaythrough ?use_capture ?passive target

let durationchange ?use_capture ?passive target =
  make_event Dom_html.Event.durationchange ?use_capture ?passive target

let emptied ?use_capture ?passive target =
  make_event Dom_html.Event.emptied ?use_capture ?passive target

let ended ?use_capture ?passive target =
  make_event Dom_html.Event.ended ?use_capture ?passive target

let loadeddata ?use_capture ?passive target =
  make_event Dom_html.Event.loadeddata ?use_capture ?passive target

let loadedmetadata ?use_capture ?passive target =
  make_event Dom_html.Event.loadedmetadata ?use_capture ?passive target

let loadstart ?use_capture ?passive target =
  make_event Dom_html.Event.loadstart ?use_capture ?passive target

let pause ?use_capture ?passive target =
  make_event Dom_html.Event.pause ?use_capture ?passive target

let play ?use_capture ?passive target =
  make_event Dom_html.Event.play ?use_capture ?passive target

let playing ?use_capture ?passive target =
  make_event Dom_html.Event.playing ?use_capture ?passive target

let ratechange ?use_capture ?passive target =
  make_event Dom_html.Event.ratechange ?use_capture ?passive target

let seeked ?use_capture ?passive target =
  make_event Dom_html.Event.seeked ?use_capture ?passive target

let seeking ?use_capture ?passive target =
  make_event Dom_html.Event.seeking ?use_capture ?passive target

let stalled ?use_capture ?passive target =
  make_event Dom_html.Event.stalled ?use_capture ?passive target

let suspend ?use_capture ?passive target =
  make_event Dom_html.Event.suspend ?use_capture ?passive target

let volumechange ?use_capture ?passive target =
  make_event Dom_html.Event.volumechange ?use_capture ?passive target

let waiting ?use_capture ?passive target =
  make_event Dom_html.Event.waiting ?use_capture ?passive target

(* special case for mousewheel, because it depends on the browser *)
let mousewheel ?use_capture ?passive target =
  let el = ref Js.null in
  let t, w = Lwt.task () in
  let cancel () = Js.Opt.iter !el Dom_html.removeEventListener in
  Lwt.on_cancel t cancel;
  el :=
    Js.some
      (Dom_html.addMousewheelEventListenerWithOptions
         ?capture:(opt_map Js.bool use_capture)
         ?passive:(opt_map Js.bool passive)
         target
         (fun (ev : #Dom_html.event Js.t) ~dx ~dy ->
           Firebug.console##log ev;
           cancel ();
           Lwt.wakeup w (ev, (dx, dy));
           Js.bool true)
      (* true because we do not want to prevent default ->
                           the user can use the preventDefault function
                           above. *));
  t

(* let _DOMMouseScroll ?use_capture ?passive target =
   make_event Dom_html.Event._DOMMouseScroll ?use_capture ?passive target
*)

let wheel ?use_capture ?passive target =
  make_event Dom_html.Event.wheel ?use_capture ?passive target

let touchstart ?use_capture ?passive target =
  make_event Dom_html.Event.touchstart ?use_capture ?passive target

let touchmove ?use_capture ?passive target =
  make_event Dom_html.Event.touchmove ?use_capture ?passive target

let touchend ?use_capture ?passive target =
  make_event Dom_html.Event.touchend ?use_capture ?passive target

let touchcancel ?use_capture ?passive target =
  make_event Dom_html.Event.touchcancel ?use_capture ?passive target

let lostpointercapture ?use_capture ?passive target =
  make_event Dom_html.Event.lostpointercapture ?use_capture ?passive target

let gotpointercapture ?use_capture ?passive target =
  make_event Dom_html.Event.gotpointercapture ?use_capture ?passive target

let pointerenter ?use_capture ?passive target =
  make_event Dom_html.Event.pointerenter ?use_capture ?passive target

let pointercancel ?use_capture ?passive target =
  make_event Dom_html.Event.pointercancel ?use_capture ?passive target

let pointerdown ?use_capture ?passive target =
  make_event Dom_html.Event.pointerdown ?use_capture ?passive target

let pointerleave ?use_capture ?passive target =
  make_event Dom_html.Event.pointerleave ?use_capture ?passive target

let pointermove ?use_capture ?passive target =
  make_event Dom_html.Event.pointermove ?use_capture ?passive target

let pointerout ?use_capture ?passive target =
  make_event Dom_html.Event.pointerout ?use_capture ?passive target

let pointerover ?use_capture ?passive target =
  make_event Dom_html.Event.pointerover ?use_capture ?passive target

let pointerup ?use_capture ?passive target =
  make_event Dom_html.Event.pointerup ?use_capture ?passive target

let transitionend ?use_capture ?passive elt =
  make_event Dom_html.Event.transitionend ?use_capture ?passive elt

let transitionstart ?use_capture ?passive elt =
  make_event Dom_html.Event.transitionstart ?use_capture ?passive elt

let transitionrun ?use_capture ?passive elt =
  make_event Dom_html.Event.transitionrun ?use_capture ?passive elt

let transitioncancel ?use_capture ?passive elt =
  make_event Dom_html.Event.transitioncancel ?use_capture ?passive elt

let clicks ?cancel_handler ?use_capture ?passive t =
  seq_loop click ?cancel_handler ?use_capture ?passive t

let copies ?cancel_handler ?use_capture ?passive t =
  seq_loop copy ?cancel_handler ?use_capture ?passive t

let cuts ?cancel_handler ?use_capture ?passive t =
  seq_loop cut ?cancel_handler ?use_capture ?passive t

let pastes ?cancel_handler ?use_capture ?passive t =
  seq_loop paste ?cancel_handler ?use_capture ?passive t

let dblclicks ?cancel_handler ?use_capture ?passive t =
  seq_loop dblclick ?cancel_handler ?use_capture ?passive t

let mousedowns ?cancel_handler ?use_capture ?passive t =
  seq_loop mousedown ?cancel_handler ?use_capture ?passive t

let mouseups ?cancel_handler ?use_capture ?passive t =
  seq_loop mouseup ?cancel_handler ?use_capture ?passive t

let mouseovers ?cancel_handler ?use_capture ?passive t =
  seq_loop mouseover ?cancel_handler ?use_capture ?passive t

let mousemoves ?cancel_handler ?use_capture ?passive t =
  seq_loop mousemove ?cancel_handler ?use_capture ?passive t

let mouseouts ?cancel_handler ?use_capture ?passive t =
  seq_loop mouseout ?cancel_handler ?use_capture ?passive t

let keypresses ?cancel_handler ?use_capture ?passive t =
  seq_loop keypress ?cancel_handler ?use_capture ?passive t

let keydowns ?cancel_handler ?use_capture ?passive t =
  seq_loop keydown ?cancel_handler ?use_capture ?passive t

let keyups ?cancel_handler ?use_capture ?passive t =
  seq_loop keyup ?cancel_handler ?use_capture ?passive t

let changes ?cancel_handler ?use_capture ?passive t =
  seq_loop change ?cancel_handler ?use_capture ?passive t

let inputs ?cancel_handler ?use_capture ?passive t =
  seq_loop input ?cancel_handler ?use_capture ?passive t

let timeupdates ?cancel_handler ?use_capture ?passive t =
  seq_loop timeupdate ?cancel_handler ?use_capture ?passive t

let dragstarts ?cancel_handler ?use_capture ?passive t =
  seq_loop dragstart ?cancel_handler ?use_capture ?passive t

let dragends ?cancel_handler ?use_capture ?passive t =
  seq_loop dragend ?cancel_handler ?use_capture ?passive t

let dragenters ?cancel_handler ?use_capture ?passive t =
  seq_loop dragenter ?cancel_handler ?use_capture ?passive t

let dragovers ?cancel_handler ?use_capture ?passive t =
  seq_loop dragover ?cancel_handler ?use_capture ?passive t

let dragleaves ?cancel_handler ?use_capture ?passive t =
  seq_loop dragleave ?cancel_handler ?use_capture ?passive t

let drags ?cancel_handler ?use_capture ?passive t =
  seq_loop drag ?cancel_handler ?use_capture ?passive t

let drops ?cancel_handler ?use_capture ?passive t =
  seq_loop drop ?cancel_handler ?use_capture ?passive t

let mousewheels ?cancel_handler ?use_capture ?passive t =
  seq_loop mousewheel ?cancel_handler ?use_capture ?passive t

let wheels ?cancel_handler ?use_capture ?passive t =
  seq_loop wheel ?cancel_handler ?use_capture ?passive t

let touchstarts ?cancel_handler ?use_capture ?passive t =
  seq_loop touchstart ?cancel_handler ?use_capture ?passive t

let touchmoves ?cancel_handler ?use_capture ?passive t =
  seq_loop touchmove ?cancel_handler ?use_capture ?passive t

let touchends ?cancel_handler ?use_capture ?passive t =
  seq_loop touchend ?cancel_handler ?use_capture ?passive t

let touchcancels ?cancel_handler ?use_capture ?passive t =
  seq_loop touchcancel ?cancel_handler ?use_capture ?passive t

let focuses ?cancel_handler ?use_capture ?passive t =
  seq_loop focus ?cancel_handler ?use_capture ?passive t

let blurs ?cancel_handler ?use_capture ?passive t =
  seq_loop blur ?cancel_handler ?use_capture ?passive t

let scrolls ?cancel_handler ?use_capture ?passive t =
  seq_loop scroll ?cancel_handler ?use_capture ?passive t

let submits ?cancel_handler ?use_capture ?passive t =
  seq_loop submit ?cancel_handler ?use_capture ?passive t

let selects ?cancel_handler ?use_capture ?passive t =
  seq_loop select ?cancel_handler ?use_capture ?passive t

let aborts ?cancel_handler ?use_capture ?passive t =
  seq_loop abort ?cancel_handler ?use_capture ?passive t

let errors ?cancel_handler ?use_capture ?passive t =
  seq_loop error ?cancel_handler ?use_capture ?passive t

let loads ?cancel_handler ?use_capture ?passive t =
  seq_loop load ?cancel_handler ?use_capture ?passive t

let canplays ?cancel_handler ?use_capture ?passive t =
  seq_loop canplay ?cancel_handler ?use_capture ?passive t

let canplaythroughs ?cancel_handler ?use_capture ?passive t =
  seq_loop canplaythrough ?cancel_handler ?use_capture ?passive t

let durationchanges ?cancel_handler ?use_capture ?passive t =
  seq_loop durationchange ?cancel_handler ?use_capture ?passive t

let emptieds ?cancel_handler ?use_capture ?passive t =
  seq_loop emptied ?cancel_handler ?use_capture ?passive t

let endeds ?cancel_handler ?use_capture ?passive t =
  seq_loop ended ?cancel_handler ?use_capture ?passive t

let loadeddatas ?cancel_handler ?use_capture ?passive t =
  seq_loop loadeddata ?cancel_handler ?use_capture ?passive t

let loadedmetadatas ?cancel_handler ?use_capture ?passive t =
  seq_loop loadedmetadata ?cancel_handler ?use_capture ?passive t

let loadstarts ?cancel_handler ?use_capture ?passive t =
  seq_loop loadstart ?cancel_handler ?use_capture ?passive t

let pauses ?cancel_handler ?use_capture ?passive t =
  seq_loop pause ?cancel_handler ?use_capture ?passive t

let plays ?cancel_handler ?use_capture ?passive t =
  seq_loop play ?cancel_handler ?use_capture ?passive t

let playings ?cancel_handler ?use_capture ?passive t =
  seq_loop playing ?cancel_handler ?use_capture ?passive t

let ratechanges ?cancel_handler ?use_capture ?passive t =
  seq_loop ratechange ?cancel_handler ?use_capture ?passive t

let seekeds ?cancel_handler ?use_capture ?passive t =
  seq_loop seeked ?cancel_handler ?use_capture ?passive t

let seekings ?cancel_handler ?use_capture ?passive t =
  seq_loop seeking ?cancel_handler ?use_capture ?passive t

let stalleds ?cancel_handler ?use_capture ?passive t =
  seq_loop stalled ?cancel_handler ?use_capture ?passive t

let suspends ?cancel_handler ?use_capture ?passive t =
  seq_loop suspend ?cancel_handler ?use_capture ?passive t

let volumechanges ?cancel_handler ?use_capture ?passive t =
  seq_loop volumechange ?cancel_handler ?use_capture ?passive t

let waitings ?cancel_handler ?use_capture ?passive t =
  seq_loop waiting ?cancel_handler ?use_capture ?passive t

let lostpointercaptures ?cancel_handler ?use_capture ?passive t =
  seq_loop lostpointercapture ?cancel_handler ?use_capture ?passive t

let gotpointercaptures ?cancel_handler ?use_capture ?passive t =
  seq_loop gotpointercapture ?cancel_handler ?use_capture ?passive t

let pointerenters ?cancel_handler ?use_capture ?passive t =
  seq_loop pointerenter ?cancel_handler ?use_capture ?passive t

let pointercancels ?cancel_handler ?use_capture ?passive t =
  seq_loop pointercancel ?cancel_handler ?use_capture ?passive t

let pointerdowns ?cancel_handler ?use_capture ?passive t =
  seq_loop pointerdown ?cancel_handler ?use_capture ?passive t

let pointerleaves ?cancel_handler ?use_capture ?passive t =
  seq_loop pointerleave ?cancel_handler ?use_capture ?passive t

let pointermoves ?cancel_handler ?use_capture ?passive t =
  seq_loop pointermove ?cancel_handler ?use_capture ?passive t

let pointerouts ?cancel_handler ?use_capture ?passive t =
  seq_loop pointerout ?cancel_handler ?use_capture ?passive t

let pointerovers ?cancel_handler ?use_capture ?passive t =
  seq_loop pointerover ?cancel_handler ?use_capture ?passive t

let pointerups ?cancel_handler ?use_capture ?passive t =
  seq_loop pointerup ?cancel_handler ?use_capture ?passive t

let transitionends ?cancel_handler ?use_capture ?passive t =
  seq_loop transitionend ?cancel_handler ?use_capture ?passive t

let transitionstarts ?cancel_handler ?use_capture ?passive t =
  seq_loop transitionstart ?cancel_handler ?use_capture ?passive t

let transitionruns ?cancel_handler ?use_capture ?passive t =
  seq_loop transitionrun ?cancel_handler ?use_capture ?passive t

let transitioncancels ?cancel_handler ?use_capture ?passive t =
  seq_loop transitioncancel ?cancel_handler ?use_capture ?passive t

let request_animation_frame () =
  let t, s = Lwt.wait () in
  let (_ : Dom_html.animation_frame_request_id) =
    Dom_html.window##requestAnimationFrame
      (Js.wrap_callback (fun (_ : Js.number_t) -> Lwt.wakeup s ()))
  in
  t

let onload () = make_event Dom_html.Event.load Dom_html.window

let domContentLoaded =
  let complete = Js.string "complete" in
  let doc = Dom_html.window##.document in
  fun () ->
    if doc##.readyState == complete
    then Lwt.return_unit
    else
      let t, w = Lwt.task () in
      let wakeup w _ = if Lwt.is_sleeping t then Lwt.wakeup w () in
      let wakeup_exn w e = if Lwt.is_sleeping t then Lwt.wakeup_exn w e in
      (* https://github.com/dperini/ContentLoaded/blob/master/src/contentloaded.js *)
      let regular = make_event Dom_html.Event.domContentLoaded doc in
      Lwt.on_any regular (wakeup w) (wakeup_exn w);
      (* ie8 *)
      let readystatechange =
        async_loop
          (make_event (Dom.Event.make "readystatechange"))
          doc
          (fun e _ ->
            if doc##.readyState == complete then wakeup w e;
            Lwt.return_unit)
      in
      (* fallback, just in case *)
      let init = make_event Dom_html.Event.load Dom_html.window in
      Lwt.on_any init (wakeup w) (wakeup_exn w);
      (* clean and return *)
      Lwt.bind t (fun _e ->
          Lwt.cancel regular;
          Lwt.cancel readystatechange;
          Lwt.cancel init;
          Lwt.return_unit)

let onunload () = make_event Dom_html.Event.unload Dom_html.window

let onbeforeunload () = make_event Dom_html.Event.beforeunload Dom_html.window

let onresize () = make_event Dom_html.Event.resize Dom_html.window

let onorientationchange () = make_event Dom_html.Event.orientationchange Dom_html.window

let onpopstate () = make_event Dom_html.Event.popstate Dom_html.window

let onhashchange () = make_event Dom_html.Event.hashchange Dom_html.window

let onorientationchange_or_onresize () = Lwt.pick [ onresize (); onorientationchange () ]

let onresizes t = seq_loop (fun ?use_capture:_ ?passive:_ () -> onresize ()) () t

let onorientationchanges t =
  seq_loop (fun ?use_capture:_ ?passive:_ () -> onorientationchange ()) () t

let onpopstates t = seq_loop (fun ?use_capture:_ ?passive:_ () -> onpopstate ()) () t

let onhashchanges t = seq_loop (fun ?use_capture:_ ?passive:_ () -> onhashchange ()) () t

let onorientationchanges_or_onresizes t =
  seq_loop (fun ?use_capture:_ ?passive:_ () -> onorientationchange_or_onresize ()) () t

let limited_onresizes ?elapsed_time t =
  limited_loop (fun ?use_capture:_ ?passive:_ () -> onresize ()) ?elapsed_time () t

let limited_onorientationchanges ?elapsed_time t =
  limited_loop
    (fun ?use_capture:_ ?passive:_ () -> onorientationchange ())
    ?elapsed_time
    ()
    t

let limited_onorientationchanges_or_onresizes ?elapsed_time t =
  limited_loop
    (fun ?use_capture:_ ?passive:_ () -> onorientationchange_or_onresize ())
    ?elapsed_time
    ()
    t
