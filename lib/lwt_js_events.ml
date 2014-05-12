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

let async f = Lwt.async (fun () -> Lwt_js.yield () >>= f)

let make_event event_kind ?(use_capture = false) target =
  let el = ref Js.null in
  let t, w = Lwt.task () in
  let cancel () = Js.Opt.iter !el Dom_html.removeEventListener in
  Lwt.on_cancel t cancel;
  el := Js.some
    (Dom.addEventListener
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

let catch_cancel f x =
  Lwt.catch
    (fun () -> f x)
    (function
    | Lwt.Canceled -> Lwt.return ()
    | e -> Lwt.fail e)

let with_error_log f x =
  Lwt.catch
    (fun () -> f x)
    (fun e -> (Firebug.console##log(Js.string (Printexc.to_string e));
               Lwt.return ()))

let seq_loop evh ?(cancel_handler = false) ?use_capture target handler =
  let cancelled = ref false in
  let cur = ref (Lwt.fail (Failure "Lwt_js_event")) in
  (* Using Lwt.fail as default, to be polymorphic *)
  let cur_handler = ref (Lwt.return ()) in
  let lt, lw = Lwt.task () in
  Lwt.on_cancel lt
    (fun () ->
      Lwt.cancel !cur;
      if cancel_handler then Lwt.cancel !cur_handler;
      cancelled := true);
  let rec aux () =
    if not !cancelled (* In the case it has been cancelled
                         during the previous handler,
                         we do not reinstall the event handler *)
    then begin
      let t = evh ?use_capture target in
      cur := t;
      t >>= fun e ->
      cur_handler := with_error_log (handler e) lt;
      !cur_handler >>= aux
    end
    else Lwt.return ()
  in
  Lwt.async (catch_cancel aux);
  lt

let async_loop evh ?use_capture target handler =
  let cancelled = ref false in
  let cur = ref (Lwt.fail (Failure "Lwt_js_event")) in
  let lt, lw = Lwt.task () in
  Lwt.on_cancel lt (fun () -> Lwt.cancel !cur; cancelled := true);
  let rec aux () =
    if not !cancelled then begin
      let t = evh ?use_capture target in
      cur := t;
      t >>= fun e ->
      Lwt.async (fun () -> with_error_log (handler e) lt);
      aux ()
    end
    else Lwt.return ()
  in
  Lwt.async (catch_cancel aux);
  lt

let buffered_loop evh ?(cancel_handler = false) ?(cancel_queue = true)
    ?use_capture target handler =
  let cancelled = ref false in
  let queue = ref [] in
  let cur = ref (Lwt.fail (Failure "Lwt_js_event")) in
  let cur_handler = ref (Lwt.return ()) in
  let lt, lw = Lwt.task () in
  let spawn = Lwt_condition.create () in
  Lwt.on_cancel lt (fun () ->
    Lwt.cancel !cur ;
    if cancel_handler then Lwt.cancel !cur_handler ;
    if cancel_queue then queue := [] ;
    cancelled := true) ;
  let rec spawner () =
    if not !cancelled then begin
      let t = evh ?use_capture target in
      cur := t;
      t >>= fun e ->
      queue := e :: !queue ;
      Lwt_condition.signal spawn () ;
      spawner ()
    end
    else Lwt.return ()
  in
  let rec runner () =
    cur_handler := Lwt.return ();
    if not !cancelled then
      match !queue with
      | [] -> Lwt_condition.wait spawn >>= runner
      | e :: tl ->
	queue := tl ;
        cur_handler := with_error_log (handler e) lt;
        !cur_handler >>= runner
    else Lwt.return ()
  in
  Lwt.async (catch_cancel spawner) ;
  Lwt.async runner ;
  lt

let func_limited_loop event limited_func ?use_capture target handler =
  let count = ref 0 in
  async_loop event ?use_capture target
    (fun ev lt -> incr count;
      let nb = !count in
      limited_func () >>= (fun _ ->
	if (!count = nb)
	then handler ev lt
	else Lwt.return ()))

let limited_loop event ?(elapsed_time=0.1) =
  func_limited_loop event (fun () -> Lwt_js.sleep elapsed_time)

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
let change ?use_capture target =
  make_event Dom_html.Event.change ?use_capture target
let input ?use_capture target =
  make_event Dom_html.Event.input ?use_capture target
let timeupdate ?use_capture target =
  make_event Dom_html.Event.timeupdate ?use_capture target

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

let focus ?use_capture target =
  make_event Dom_html.Event.focus ?use_capture target
let blur ?use_capture target =
  make_event Dom_html.Event.blur ?use_capture target
let scroll ?use_capture target =
  make_event Dom_html.Event.scroll ?use_capture target
let submit ?use_capture target =
  make_event Dom_html.Event.submit ?use_capture target
let select ?use_capture target =
  make_event Dom_html.Event.select ?use_capture target

let abort ?use_capture target =
  make_event Dom_html.Event.abort ?use_capture target
let error ?use_capture target =
  make_event Dom_html.Event.error ?use_capture target
let load ?use_capture target =
  make_event Dom_html.Event.load ?use_capture target



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



let clicks ?cancel_handler ?use_capture t =
  seq_loop click ?cancel_handler ?use_capture t
let dblclicks ?cancel_handler ?use_capture t =
  seq_loop dblclick ?cancel_handler ?use_capture t
let mousedowns ?cancel_handler ?use_capture t =
  seq_loop mousedown ?cancel_handler ?use_capture t
let mouseups ?cancel_handler ?use_capture t =
  seq_loop mouseup ?cancel_handler ?use_capture t
let mouseovers ?cancel_handler ?use_capture t =
  seq_loop mouseover ?cancel_handler ?use_capture t
let mousemoves ?cancel_handler ?use_capture t =
  seq_loop mousemove ?cancel_handler ?use_capture t
let mouseouts ?cancel_handler ?use_capture t =
  seq_loop mouseout ?cancel_handler ?use_capture t

let keypresses ?cancel_handler ?use_capture t =
  seq_loop keypress ?cancel_handler ?use_capture t
let keydowns ?cancel_handler ?use_capture t =
  seq_loop keydown ?cancel_handler ?use_capture t
let keyups ?cancel_handler ?use_capture t =
  seq_loop keyup ?cancel_handler ?use_capture t
let changes ?cancel_handler ?use_capture t =
  seq_loop change ?cancel_handler ?use_capture t
let inputs ?cancel_handler ?use_capture t =
  seq_loop input ?cancel_handler ?use_capture t
let timeupdates ?cancel_handler ?use_capture t =
  seq_loop timeupdate ?cancel_handler ?use_capture t

let dragstarts ?cancel_handler ?use_capture t =
  seq_loop dragstart ?cancel_handler ?use_capture t
let dragends ?cancel_handler ?use_capture t =
  seq_loop dragend ?cancel_handler ?use_capture t
let dragenters ?cancel_handler ?use_capture t =
  seq_loop dragenter ?cancel_handler ?use_capture t
let dragovers ?cancel_handler ?use_capture t =
  seq_loop dragover ?cancel_handler ?use_capture t
let dragleaves ?cancel_handler ?use_capture t =
  seq_loop dragleave ?cancel_handler ?use_capture t
let drags ?cancel_handler ?use_capture t =
  seq_loop drag ?cancel_handler ?use_capture t
let drops ?cancel_handler ?use_capture t =
  seq_loop drop ?cancel_handler ?use_capture t

let mousewheels ?cancel_handler ?use_capture t =
  seq_loop mousewheel ?cancel_handler ?use_capture t

let touchstarts ?cancel_handler ?use_capture t =
  seq_loop touchstart ?cancel_handler ?use_capture t
let touchmoves ?cancel_handler ?use_capture t =
  seq_loop touchmove ?cancel_handler ?use_capture t
let touchends ?cancel_handler ?use_capture t =
  seq_loop touchend ?cancel_handler ?use_capture t
let touchcancels ?cancel_handler ?use_capture t =
  seq_loop touchcancel ?cancel_handler ?use_capture t

let focuses ?cancel_handler ?use_capture t =
  seq_loop focus ?cancel_handler ?use_capture t
let blurs ?cancel_handler ?use_capture t =
  seq_loop blur ?cancel_handler ?use_capture t
let scrolls ?cancel_handler ?use_capture t =
  seq_loop scroll ?cancel_handler ?use_capture t
let submits ?cancel_handler ?use_capture t =
  seq_loop submit ?cancel_handler ?use_capture t
let selects ?cancel_handler ?use_capture t =
  seq_loop select ?cancel_handler ?use_capture t

let transition_evn =
  let e = Dom_html.createDiv Dom_html.document in
  try
    snd (List.find
           (fun (propname, evname) ->
             Js.Unsafe.get (e##style) propname != Js.undefined)
           [("WebkitTransition", [Dom.Event.make "webkitTransitionEnd"]);
            ("MozTransition", [Dom.Event.make "transitionend"]);
            ("OTransition", [Dom.Event.make "oTransitionEnd";
                             Dom.Event.make "otransitionend"]);
            ("transition", [Dom.Event.make "transitionend"])])
  with Not_found -> []

let transitionend elt =
  match transition_evn with
    | [] -> Lwt.return ()
    | _ -> Lwt.pick
      (List.map
         (fun ev -> make_event ev elt)
         transition_evn) >>= fun _ ->
      Lwt.return ()

let transitionends t =
  seq_loop (fun ?use_capture e -> transitionend e) t

let request_animation_frame () =
  let t, s = Lwt.wait () in
  Dom_html._requestAnimationFrame
    (Js.wrap_callback (fun () -> Lwt.wakeup s ()));
  t

let onload () = make_event Dom_html.Event.load Dom_html.window
let onbeforeunload () = make_event Dom_html.Event.beforeunload Dom_html.window
let onresize () = make_event Dom_html.Event.resize Dom_html.window
let onorientationchange () =
  make_event Dom_html.Event.orientationchange Dom_html.window
let onpopstate () = make_event Dom_html.Event.popstate Dom_html.window
let onhashchange () = make_event Dom_html.Event.hashchange Dom_html.window

let onorientationchange_or_onresize () =
  Lwt.pick [onresize (); onorientationchange ()]

let onresizes t = seq_loop (fun ?use_capture () -> onresize ()) () t
let onorientationchanges t =
  seq_loop (fun ?use_capture () -> onorientationchange ()) () t
let onpopstates t = seq_loop (fun ?use_capture () -> onpopstate ()) () t
let onhashchanges t = seq_loop (fun ?use_capture () -> onhashchange ()) () t

let onorientationchanges_or_onresizes t =
  seq_loop
    (fun ?use_capture () -> onorientationchange_or_onresize ()) () t

let limited_onresizes ?elapsed_time t =
  limited_loop (fun ?use_capture () -> onresize ()) ?elapsed_time () t
let limited_onorientationchanges ?elapsed_time t =
  limited_loop
    (fun ?use_capture () -> onorientationchange ()) ?elapsed_time () t
let limited_onorientationchanges_or_onresizes ?elapsed_time t =
  limited_loop
    (fun ?use_capture () -> onorientationchange_or_onresize ())
    ?elapsed_time () t
