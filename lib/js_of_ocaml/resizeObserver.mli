(* Js_of_ocaml library
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2019 Alexander Yanin
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

(** ResizeObserver API

    A code example:
    {[
      if ResizeObserver.is_supported ()
      then
        let doc = Dom_html.document in
        let target =
          Js.Opt.get (doc##getElementById (Js.string "observed")) (fun () -> assert false)
        in
        let node = (target :> Dom.node Js.t) in
        let f entries observer =
          Firebug.console##debug entries;
          Firebug.console##debug observer
        in
        ResizeObserver.observe ~node ~f ~box:(Js.string "content-box") ()
    ]}

    @see <https://developer.mozilla.org/en-US/docs/Web/API/ResizeObserver>
      for API documentation
    @see <https://drafts.csswg.org/resize-observer> for W3C draft spec *)

class type resizeObserverSize = object
  method inlineSize : Js.number_t Js.readonly_prop

  method blockSize : Js.number_t Js.readonly_prop
end

class type resizeObserverEntry = object
  method target : Dom.node Js.t Js.readonly_prop

  method contentRect : Dom_html.clientRect Js.t Js.readonly_prop

  method borderBoxSize : resizeObserverSize Js.t Js.js_array Js.t Js.readonly_prop

  method contentBoxSize : resizeObserverSize Js.t Js.js_array Js.t Js.readonly_prop
end

class type resizeObserverOptions = object
  method box : Js.js_string Js.t Js.writeonly_prop
end

class type resizeObserver = object
  method observe : #Dom.node Js.t -> unit Js.meth

  method observe_withOptions :
    #Dom.node Js.t -> resizeObserverOptions Js.t -> unit Js.meth

  method unobserve : #Dom.node Js.t -> unit Js.meth

  method disconnect : unit Js.meth
end

val empty_resize_observer_options : unit -> resizeObserverOptions Js.t

val resizeObserver :
  (   (resizeObserverEntry Js.t Js.js_array Js.t -> resizeObserver Js.t -> unit)
      Js.callback
   -> resizeObserver Js.t)
  Js.constr

val is_supported : unit -> bool

val observe :
     node:#Dom.node Js.t
  -> f:(resizeObserverEntry Js.t Js.js_array Js.t -> resizeObserver Js.t -> unit)
  -> ?box:Js.js_string Js.t
  -> unit
  -> resizeObserver Js.t
(** Helper to create a new observer and connect it to a node. *)
