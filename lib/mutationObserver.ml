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

class type _MutationObserverInit = object
  method childList : bool Js.writeonly_prop
  method attributes : bool Js.writeonly_prop
  method characterData : bool Js.writeonly_prop
  method subtree : bool Js.writeonly_prop
  method attributeOldValue : bool Js.writeonly_prop
  method characterDataOldValue : bool Js.writeonly_prop
  method attributeFilter : Js.js_string Js.t Js.js_array Js.t Js.writeonly_prop
end

class type _MutationRecord = object
  method _type : Js.js_string Js.t Js.readonly_prop
  method target : Dom.node Js.t Js.readonly_prop
  method addedNodes : Dom.node Js.t Dom.nodeList Js.t Js.readonly_prop
  method removedNodes : Dom.node Js.t Dom.nodeList Js.t Js.readonly_prop
  method previousSibling : Dom.node Js.t Js.opt Js.readonly_prop
  method nextSibling : Dom.node Js.t Js.opt Js.readonly_prop
  method attributeName : Js.js_string Js.t Js.opt Js.readonly_prop
  method attributeNamespace : Js.js_string Js.t Js.opt Js.readonly_prop
  method oldValue : Js.js_string Js.t Js.opt Js.readonly_prop
end

class type _MutationObserver = object
  method observe : Dom.node Js.t -> _MutationObserverInit Js.t -> unit Js.meth
  method disconnect : unit Js.meth
  method takeRecords : _MutationRecord Js.t Js.js_array Js.t Js.meth
end

let empty_mutation_observer_init () = Js.Unsafe.obj [||]

let mutationObserver =
  Js.Unsafe.global##_MutationObserver

let is_supported () = Js.Optdef.test mutationObserver
