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

(** Programming mouse or keyboard events handlers using Lwt *)

(**

   Reminder:
   Event capturing starts with the outer most element in the DOM and
   works inwards to the HTML element the event took place on (capture phase)
   and then out again (bubbling phase).

*)



(**  {2 Create Lwt threads for events} *)

(** [make_event ev target] creates a Lwt thread that waits
    for the event [ev] to happen on [target] (once).
    This thread is cancellable using
    {% <<a_api project="lwt" | val Lwt.cancel>> %}.
    If you set the optional parameter [~use_capture:true],
    the event will be caught during the capture phase,
    otherwise it is caught during the bubbling phase
    (default).
*)
val make_event :
  (#Dom_html.event as 'a) Js.t Dom_html.Event.typ ->
  ?use_capture:bool -> #Dom_html.eventTarget Js.t -> 'a Js.t Lwt.t

(** [seq_loop (make_event ev) target handler] creates a looping Lwt
    thread that waits for the event [ev] to happen on [target], then
    execute handler, and start again waiting for the event. Events
    happening during the execution of the handler are ignored. See
    [async_loop] and [buffered_loop] for alternative semantics.

    For example, the [clicks] function below is defined by:

    [let clicks ?use_capture t = seq_loop click ?use_capture t]

    The thread returned is cancellable using
    {% <<a_api project="lwt" | val Lwt.cancel>> %}.
    In order for the loop thread to be canceled from within the handler,
    the latter receives the former as its second parameter.

    By default, cancelling the loop will not cancel the potential
    currently running handler. This behaviour can be changed by
    setting the [cancel_handler] parameter to true.
*)
val seq_loop :
  (?use_capture:bool -> 'target -> 'event Lwt.t) ->
  ?cancel_handler:bool ->
  ?use_capture:bool -> 'target -> ('event -> unit Lwt.t -> unit Lwt.t) -> unit Lwt.t

(** [async_loop] is similar to [seq_loop], but each handler runs
    independently. No event is thus missed, but since several
    instances of the handler can be run concurrently, it is up to the
    programmer to ensure that they interact correctly.

    Cancelling the loop will not cancel the potential currently running
    handlers.
*)
val async_loop :
  (?use_capture:bool -> 'target -> 'event Lwt.t) ->
  ?use_capture:bool -> 'target -> ('event -> unit Lwt.t -> unit Lwt.t) -> unit Lwt.t

(** [buffered_loop] is similar to [seq_loop], but any event that
    occurs during an execution of the handler is queued instead of
    being ingnored.

    No event is thus missed, but there can be a non predictible delay
    between its trigger and its treatment. It is thus a good idea to
    use this loop with handlers whose running time is short, so the
    memorized event still makes sense when the handler is eventually
    executed. It is also up to the programmer to ensure that event
    handlers terminate so the queue will eventually be emptied.

    By default, cancelling the loop will not cancel the (potential)
    currently running handler, but any other queued event will be
    dropped. This behaviour can be customized using the two optional
    parameters [cancel_handler] and [cancel_queue].
*)
val buffered_loop :
  (?use_capture:bool -> 'target -> 'event Lwt.t) ->
  ?cancel_handler:bool -> ?cancel_queue:bool ->
  ?use_capture:bool -> 'target -> ('event -> unit Lwt.t -> unit Lwt.t) -> unit Lwt.t


(** [async t] records a thread to be executed later.
    It is implemented by calling yield, then Lwt.async.
    This is useful if you want to create a new event listener
    when you are inside an event handler.
    This avoids the current event to be catched by the new event handler
    (if it propagates).
*)
val async : (unit -> 'a Lwt.t) -> unit

(** [func_limited_loop event delay_fun target handler] will behave like
    [Lwt_js_events.async_loop event target handler] but it will run [delay_fun]
    first, and execut [handler] only when [delay_fun] is finished and
    no other event occurred in the meantime.

    This allows to limit the number of events catched.

    Be careful, it is an asynchrone loop, so if you give too little time,
    several instances of your handler could be run in same time **)
val func_limited_loop :
  (?use_capture:bool -> 'a -> 'b Lwt.t) ->
  (unit -> 'a Lwt.t) ->
  ?use_capture:bool ->
  'a -> ('b -> unit Lwt.t -> unit Lwt.t) -> unit Lwt.t

(** Same as func_limited_loop but take time instead of function
    By default elapsed_time = 0.1s = 100ms **)
val limited_loop:
  (?use_capture:bool -> 'a -> 'b Lwt.t) ->
  ?elapsed_time:float ->
  ?use_capture:bool ->
  'a -> ('b -> unit Lwt.t -> unit Lwt.t) -> unit Lwt.t

(**  {2 Predefined functions for some types of events} *)

val click :
  ?use_capture:bool ->
  #Dom_html.eventTarget Js.t -> Dom_html.mouseEvent Js.t Lwt.t
val dblclick :
  ?use_capture:bool ->
  #Dom_html.eventTarget Js.t -> Dom_html.mouseEvent Js.t Lwt.t
val mousedown :
  ?use_capture:bool ->
  #Dom_html.eventTarget Js.t -> Dom_html.mouseEvent Js.t Lwt.t
val mouseup :
  ?use_capture:bool ->
  #Dom_html.eventTarget Js.t -> Dom_html.mouseEvent Js.t Lwt.t
val mouseover :
  ?use_capture:bool ->
  #Dom_html.eventTarget Js.t -> Dom_html.mouseEvent Js.t Lwt.t
val mousemove :
  ?use_capture:bool ->
  #Dom_html.eventTarget Js.t -> Dom_html.mouseEvent Js.t Lwt.t
val mouseout :
  ?use_capture:bool ->
  #Dom_html.eventTarget Js.t -> Dom_html.mouseEvent Js.t Lwt.t

val keypress :
  ?use_capture:bool ->
  #Dom_html.eventTarget Js.t -> Dom_html.keyboardEvent Js.t Lwt.t
val keydown :
  ?use_capture:bool ->
  #Dom_html.eventTarget Js.t -> Dom_html.keyboardEvent Js.t Lwt.t
val keyup :
  ?use_capture:bool ->
  #Dom_html.eventTarget Js.t -> Dom_html.keyboardEvent Js.t Lwt.t
val input :
  ?use_capture:bool ->
  #Dom_html.eventTarget Js.t -> Dom_html.event Js.t Lwt.t
val timeupdate :
  ?use_capture:bool ->
  #Dom_html.eventTarget Js.t -> Dom_html.event Js.t Lwt.t
val change :
  ?use_capture:bool ->
  #Dom_html.eventTarget Js.t -> Dom_html.event Js.t Lwt.t

val dragstart :
  ?use_capture:bool ->
  #Dom_html.eventTarget Js.t -> Dom_html.dragEvent Js.t Lwt.t
val dragend :
  ?use_capture:bool ->
  #Dom_html.eventTarget Js.t -> Dom_html.dragEvent Js.t Lwt.t
val dragenter :
  ?use_capture:bool ->
  #Dom_html.eventTarget Js.t -> Dom_html.dragEvent Js.t Lwt.t
val dragover :
  ?use_capture:bool ->
  #Dom_html.eventTarget Js.t -> Dom_html.dragEvent Js.t Lwt.t
val dragleave :
  ?use_capture:bool ->
  #Dom_html.eventTarget Js.t -> Dom_html.dragEvent Js.t Lwt.t
val drag :
  ?use_capture:bool ->
  #Dom_html.eventTarget Js.t -> Dom_html.dragEvent Js.t Lwt.t
val drop :
  ?use_capture:bool ->
  #Dom_html.eventTarget Js.t -> Dom_html.dragEvent Js.t Lwt.t

val focus :
  ?use_capture:bool ->
  #Dom_html.eventTarget Js.t -> Dom_html.event Js.t Lwt.t
val blur :
  ?use_capture:bool ->
  #Dom_html.eventTarget Js.t -> Dom_html.event Js.t Lwt.t
val scroll :
  ?use_capture:bool ->
  #Dom_html.eventTarget Js.t -> Dom_html.event Js.t Lwt.t
val submit :
  ?use_capture:bool ->
  #Dom_html.eventTarget Js.t -> Dom_html.event Js.t Lwt.t
val select :
  ?use_capture:bool ->
  #Dom_html.eventTarget Js.t -> Dom_html.event Js.t Lwt.t


(** This function returns the event,
    together with the numbers of ticks the mouse wheel moved.
    Positive means down or right.
    This interface is compatible with all (recent) browsers. *)
val mousewheel :
  ?use_capture:bool ->
  #Dom_html.eventTarget Js.t ->
  (Dom_html.mouseEvent Js.t * (int * int)) Lwt.t

val touchstart :
  ?use_capture:bool ->
  #Dom_html.eventTarget Js.t -> Dom_html.touchEvent Js.t Lwt.t
val touchmove :
  ?use_capture:bool ->
  #Dom_html.eventTarget Js.t -> Dom_html.touchEvent Js.t Lwt.t
val touchend :
  ?use_capture:bool ->
  #Dom_html.eventTarget Js.t -> Dom_html.touchEvent Js.t Lwt.t
val touchcancel :
  ?use_capture:bool ->
  #Dom_html.eventTarget Js.t -> Dom_html.touchEvent Js.t Lwt.t

(** Returns when a CSS transition terminates on the element. *)
val transitionend : #Dom_html.eventTarget Js.t -> unit Lwt.t

val load : ?use_capture:bool ->
  #Dom_html.imageElement Js.t -> Dom_html.event Js.t Lwt.t
val error : ?use_capture:bool ->
  #Dom_html.imageElement Js.t -> Dom_html.event Js.t Lwt.t
val abort : ?use_capture:bool ->
  #Dom_html.imageElement Js.t -> Dom_html.event Js.t Lwt.t


val clicks :
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  #Dom_html.eventTarget Js.t ->
  (Dom_html.mouseEvent Js.t -> unit Lwt.t -> unit Lwt.t) -> unit Lwt.t
val dblclicks :
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  #Dom_html.eventTarget Js.t ->
  (Dom_html.mouseEvent Js.t -> unit Lwt.t -> unit Lwt.t) -> unit Lwt.t
val mousedowns :
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  #Dom_html.eventTarget Js.t ->
  (Dom_html.mouseEvent Js.t -> unit Lwt.t -> unit Lwt.t) -> unit Lwt.t
val mouseups :
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  #Dom_html.eventTarget Js.t ->
  (Dom_html.mouseEvent Js.t -> unit Lwt.t -> unit Lwt.t) -> unit Lwt.t
val mouseovers :
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  #Dom_html.eventTarget Js.t ->
  (Dom_html.mouseEvent Js.t -> unit Lwt.t -> unit Lwt.t) -> unit Lwt.t
val mousemoves :
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  #Dom_html.eventTarget Js.t ->
  (Dom_html.mouseEvent Js.t -> unit Lwt.t -> unit Lwt.t) -> unit Lwt.t
val mouseouts :
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  #Dom_html.eventTarget Js.t ->
  (Dom_html.mouseEvent Js.t -> unit Lwt.t -> unit Lwt.t) -> unit Lwt.t

val keypresses :
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  #Dom_html.eventTarget Js.t ->
  (Dom_html.keyboardEvent Js.t -> unit Lwt.t -> unit Lwt.t) -> unit Lwt.t
val keydowns :
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  #Dom_html.eventTarget Js.t ->
  (Dom_html.keyboardEvent Js.t -> unit Lwt.t -> unit Lwt.t) -> unit Lwt.t
val keyups :
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  #Dom_html.eventTarget Js.t ->
  (Dom_html.keyboardEvent Js.t -> unit Lwt.t -> unit Lwt.t) -> unit Lwt.t
val inputs :
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  #Dom_html.eventTarget Js.t ->
  (Dom_html.event Js.t -> unit Lwt.t -> unit Lwt.t) -> unit Lwt.t
val timeupdates :
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  #Dom_html.eventTarget Js.t ->
  (Dom_html.event Js.t -> unit Lwt.t -> unit Lwt.t) -> unit Lwt.t
val changes :
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  #Dom_html.eventTarget Js.t ->
  (Dom_html.event Js.t -> unit Lwt.t -> unit Lwt.t) -> unit Lwt.t

val dragstarts :
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  #Dom_html.eventTarget Js.t ->
  (Dom_html.dragEvent Js.t -> unit Lwt.t -> unit Lwt.t) -> unit Lwt.t
val dragends :
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  #Dom_html.eventTarget Js.t ->
  (Dom_html.dragEvent Js.t -> unit Lwt.t -> unit Lwt.t) -> unit Lwt.t
val dragenters :
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  #Dom_html.eventTarget Js.t ->
  (Dom_html.dragEvent Js.t -> unit Lwt.t -> unit Lwt.t) -> unit Lwt.t
val dragovers :
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  #Dom_html.eventTarget Js.t ->
  (Dom_html.dragEvent Js.t -> unit Lwt.t -> unit Lwt.t) -> unit Lwt.t
val dragleaves :
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  #Dom_html.eventTarget Js.t ->
  (Dom_html.dragEvent Js.t -> unit Lwt.t -> unit Lwt.t) -> unit Lwt.t
val drags :
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  #Dom_html.eventTarget Js.t ->
  (Dom_html.dragEvent Js.t -> unit Lwt.t -> unit Lwt.t) -> unit Lwt.t
val drops :
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  #Dom_html.eventTarget Js.t ->
  (Dom_html.dragEvent Js.t -> unit Lwt.t -> unit Lwt.t) -> unit Lwt.t

val mousewheels :
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  #Dom_html.eventTarget Js.t ->
  ((Dom_html.mouseEvent Js.t * (int * int)) -> unit Lwt.t -> unit Lwt.t) -> unit Lwt.t

val touchstarts :
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  #Dom_html.eventTarget Js.t ->
  (Dom_html.touchEvent Js.t -> unit Lwt.t -> unit Lwt.t) -> unit Lwt.t
val touchmoves :
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  #Dom_html.eventTarget Js.t ->
  (Dom_html.touchEvent Js.t -> unit Lwt.t -> unit Lwt.t) -> unit Lwt.t
val touchends :
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  #Dom_html.eventTarget Js.t ->
  (Dom_html.touchEvent Js.t -> unit Lwt.t -> unit Lwt.t) -> unit Lwt.t
val touchcancels :
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  #Dom_html.eventTarget Js.t ->
  (Dom_html.touchEvent Js.t -> unit Lwt.t -> unit Lwt.t) -> unit Lwt.t

val focuses :
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  #Dom_html.eventTarget Js.t ->
  (Dom_html.event Js.t -> unit Lwt.t -> unit Lwt.t) -> unit Lwt.t
val blurs :
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  #Dom_html.eventTarget Js.t ->
  (Dom_html.event Js.t -> unit Lwt.t -> unit Lwt.t) -> unit Lwt.t
val scrolls :
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  #Dom_html.eventTarget Js.t ->
  (Dom_html.event Js.t -> unit Lwt.t -> unit Lwt.t) -> unit Lwt.t
val submits :
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  #Dom_html.eventTarget Js.t ->
  (Dom_html.event Js.t -> unit Lwt.t -> unit Lwt.t) -> unit Lwt.t
val selects :
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  #Dom_html.eventTarget Js.t ->
  (Dom_html.event Js.t -> unit Lwt.t -> unit Lwt.t) -> unit Lwt.t

(** Returns when a repaint of the window by the browser starts.
    (see JS method [window.requestAnimationFrame]) *)
val request_animation_frame : unit -> unit Lwt.t

(** Returns when the page is loaded *)
val onload : unit -> Dom_html.event Js.t Lwt.t

val onbeforeunload : unit -> Dom_html.event Js.t Lwt.t
val onresize : unit -> Dom_html.event Js.t Lwt.t
val onorientationchange : unit -> Dom_html.event Js.t Lwt.t
val onpopstate : unit -> Dom_html.event Js.t Lwt.t
val onhashchange : unit -> Dom_html.event Js.t Lwt.t

val onorientationchange_or_onresize : unit -> Dom_html.event Js.t Lwt.t

val onresizes :
  (Dom_html.event Js.t -> unit Lwt.t -> unit Lwt.t) -> unit Lwt.t
val onorientationchanges :
  (Dom_html.event Js.t -> unit Lwt.t -> unit Lwt.t) -> unit Lwt.t
val onpopstates :
  (Dom_html.event Js.t -> unit Lwt.t -> unit Lwt.t) -> unit Lwt.t
val onhashchanges :
  (Dom_html.event Js.t -> unit Lwt.t -> unit Lwt.t) -> unit Lwt.t

val onorientationchanges_or_onresizes :
  (Dom_html.event Js.t -> unit Lwt.t -> unit Lwt.t) -> unit Lwt.t

val limited_onresizes : ?elapsed_time:float ->
  (Dom_html.event Js.t -> unit Lwt.t -> unit Lwt.t) -> unit Lwt.t
val limited_onorientationchanges : ?elapsed_time:float ->
  (Dom_html.event Js.t -> unit Lwt.t -> unit Lwt.t) -> unit Lwt.t
val limited_onorientationchanges_or_onresizes : ?elapsed_time:float ->
  (Dom_html.event Js.t -> unit Lwt.t -> unit Lwt.t) -> unit Lwt.t
