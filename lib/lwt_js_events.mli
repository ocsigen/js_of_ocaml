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

(**

   Reminder:
   Event capturing starts with the outer most element in the DOM and
   works inwards to the HTML element the event took place on (capture phase)
   and then out again (bubbling phase).
*)


(** Call this to prevent the default handler for the event. 
    To stop propagation of the event, call {!Dom_html.stopPropagation}.
*)
val preventDefault : Dom_html.mouseEvent Js.t -> unit


(**  {2 Create Lwt threads for events} *)

(** [make_event ev target] creates a Lwt thread that waits
    for the event [ev] to happen on [target] (once).
    This thread is cancellable using {!Lwt.cancel}.
    If you set the optional parameter [~use_capture:true],
    the event will be caught during the capture phase,
    otherwise it is caught during the bubbling phase
    (default).
*)
val make_event :
  (#Dom_html.event as 'a) Js.t Dom_html.Event.typ ->
  ?use_capture:bool -> #Dom_html.eventTarget Js.t -> 'a Js.t Lwt.t

(** [seq_loop (make_event ev) target handler]
    creates a looping Lwt thread that waits
    for the event [ev] to happen on [target], then execute handler,
    and start again waiting for the event.

    For example, the [clicks] function below is defined by:

[let clicks ?use_capture t = seq_loop click ?use_capture t]

    The thread returned is cancellable using {!Lwt.cancel}.

*)
val seq_loop :
  (?use_capture:'a -> 'b -> 'c Lwt.t) ->
  ?use_capture:'a -> 'b -> ('c -> unit Lwt.t) -> 'd Lwt.t


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



val clicks :
  ?use_capture:bool ->
  #Dom_html.eventTarget Js.t ->
  (Dom_html.mouseEvent Js.t -> unit Lwt.t) -> 'a Lwt.t
val dblclicks :
  ?use_capture:bool ->
  #Dom_html.eventTarget Js.t ->
  (Dom_html.mouseEvent Js.t -> unit Lwt.t) -> 'a Lwt.t
val mousedowns :
  ?use_capture:bool ->
  #Dom_html.eventTarget Js.t ->
  (Dom_html.mouseEvent Js.t -> unit Lwt.t) -> 'a Lwt.t
val mouseups :
  ?use_capture:bool ->
  #Dom_html.eventTarget Js.t ->
  (Dom_html.mouseEvent Js.t -> unit Lwt.t) -> 'a Lwt.t
val mouseovers :
  ?use_capture:bool ->
  #Dom_html.eventTarget Js.t ->
  (Dom_html.mouseEvent Js.t -> unit Lwt.t) -> 'a Lwt.t
val mousemoves :
  ?use_capture:bool ->
  #Dom_html.eventTarget Js.t ->
  (Dom_html.mouseEvent Js.t -> unit Lwt.t) -> 'a Lwt.t
val mouseouts :
  ?use_capture:bool ->
  #Dom_html.eventTarget Js.t ->
  (Dom_html.mouseEvent Js.t -> unit Lwt.t) -> 'a Lwt.t

val keypresses :
  ?use_capture:bool ->
  #Dom_html.eventTarget Js.t ->
  (Dom_html.keyboardEvent Js.t -> unit Lwt.t) -> 'a Lwt.t
val keydowns :
  ?use_capture:bool ->
  #Dom_html.eventTarget Js.t ->
  (Dom_html.keyboardEvent Js.t -> unit Lwt.t) -> 'a Lwt.t
val keyups :
  ?use_capture:bool ->
  #Dom_html.eventTarget Js.t ->
  (Dom_html.keyboardEvent Js.t -> unit Lwt.t) -> 'a Lwt.t

val dragstarts :
  ?use_capture:bool ->
  #Dom_html.eventTarget Js.t ->
  (Dom_html.dragEvent Js.t -> unit Lwt.t) -> 'a Lwt.t
val dragends :
  ?use_capture:bool ->
  #Dom_html.eventTarget Js.t ->
  (Dom_html.dragEvent Js.t -> unit Lwt.t) -> 'a Lwt.t
val dragenters :
  ?use_capture:bool ->
  #Dom_html.eventTarget Js.t ->
  (Dom_html.dragEvent Js.t -> unit Lwt.t) -> 'a Lwt.t
val dragovers :
  ?use_capture:bool ->
  #Dom_html.eventTarget Js.t ->
  (Dom_html.dragEvent Js.t -> unit Lwt.t) -> 'a Lwt.t
val dragleaves :
  ?use_capture:bool ->
  #Dom_html.eventTarget Js.t ->
  (Dom_html.dragEvent Js.t -> unit Lwt.t) -> 'a Lwt.t
val drags :
  ?use_capture:bool ->
  #Dom_html.eventTarget Js.t ->
  (Dom_html.dragEvent Js.t -> unit Lwt.t) -> 'a Lwt.t
val drops :
  ?use_capture:bool ->
  #Dom_html.eventTarget Js.t ->
  (Dom_html.dragEvent Js.t -> unit Lwt.t) -> 'a Lwt.t

val mousewheels :
  ?use_capture:bool ->
  #Dom_html.eventTarget Js.t ->
  ((Dom_html.mouseEvent Js.t * (int * int)) -> unit Lwt.t) -> 'a Lwt.t

val touchstarts :
  ?use_capture:bool ->
  #Dom_html.eventTarget Js.t ->
  (Dom_html.touchEvent Js.t -> unit Lwt.t) -> 'a Lwt.t
val touchmoves :
  ?use_capture:bool ->
  #Dom_html.eventTarget Js.t ->
  (Dom_html.touchEvent Js.t -> unit Lwt.t) -> 'a Lwt.t
val touchends :
  ?use_capture:bool ->
  #Dom_html.eventTarget Js.t ->
  (Dom_html.touchEvent Js.t -> unit Lwt.t) -> 'a Lwt.t
val touchcancels :
  ?use_capture:bool ->
  #Dom_html.eventTarget Js.t ->
  (Dom_html.touchEvent Js.t -> unit Lwt.t) -> 'a Lwt.t
