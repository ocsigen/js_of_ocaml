(* Js_of_ocaml library
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2015 Stéphane Legrand
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

(** MutationObserver API

    A code example:
    {[
      if MutationObserver.is_supported ()
      then
        let doc = Dom_html.document in
        let target =
          Js.Opt.get (doc##getElementById (Js.string "observed")) (fun () -> assert false)
        in
        let node = (target :> Dom.node Js.t) in
        let f records observer =
          Firebug.console##debug records;
          Firebug.console##debug observer
        in
        MutationObserver.observe
          ~node
          ~f
          ~attributes:true
          ~child_list:true
          ~character_data:true
          ()
    ]}

    @see <https://developer.mozilla.org/en-US/docs/Web/API/MutationObserver>
      for API documentation.
    @see <https://dom.spec.whatwg.org/#mutation-observers>
      for the Web Hypertext Application Technology Working Group (WHATWG) spec. *)

class type mutationObserverInit = object
  method childList : bool Js.writeonly_prop

  method attributes : bool Js.writeonly_prop

  method characterData : bool Js.writeonly_prop

  method subtree : bool Js.writeonly_prop

  method attributeOldValue : bool Js.writeonly_prop

  method characterDataOldValue : bool Js.writeonly_prop

  method attributeFilter : Js.js_string Js.t Js.js_array Js.t Js.writeonly_prop
end

class type mutationRecord = object
  method _type : Js.js_string Js.t Js.readonly_prop

  method target : Dom.node Js.t Js.readonly_prop

  method addedNodes : Dom.node Dom.nodeList Js.t Js.readonly_prop

  method removedNodes : Dom.node Dom.nodeList Js.t Js.readonly_prop

  method previousSibling : Dom.node Js.t Js.opt Js.readonly_prop

  method nextSibling : Dom.node Js.t Js.opt Js.readonly_prop

  method attributeName : Js.js_string Js.t Js.opt Js.readonly_prop

  method attributeNamespace : Js.js_string Js.t Js.opt Js.readonly_prop

  method oldValue : Js.js_string Js.t Js.opt Js.readonly_prop
end

class type mutationObserver = object
  method observe : #Dom.node Js.t -> mutationObserverInit Js.t -> unit Js.meth

  method disconnect : unit Js.meth

  method takeRecords : mutationRecord Js.t Js.js_array Js.t Js.meth
end

val empty_mutation_observer_init : unit -> mutationObserverInit Js.t

val mutationObserver :
  (   (mutationRecord Js.t Js.js_array Js.t -> mutationObserver Js.t -> unit) Js.callback
   -> mutationObserver Js.t)
  Js.constr

val is_supported : unit -> bool

val observe :
     node:#Dom.node Js.t
  -> f:(mutationRecord Js.t Js.js_array Js.t -> mutationObserver Js.t -> unit)
  -> ?child_list:bool
  -> ?attributes:bool
  -> ?character_data:bool
  -> ?subtree:bool
  -> ?attribute_old_value:bool
  -> ?character_data_old_value:bool
  -> ?attribute_filter:Js.js_string Js.t list
  -> unit
  -> mutationObserver Js.t
(** Helper to create a new observer and connect it to a node *)
