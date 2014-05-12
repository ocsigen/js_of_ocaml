(* Js_of_ocaml library
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2010 Jérôme Vouillon
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

(** DOM binding

This is a partial binding to the DOM Core API.
*)

open Js

(** {2 DOM objects} *)

(** Specification of [NodeList] objects. *)
class type ['node] nodeList = object
  method item : int -> 'node t opt meth
  method length : int readonly_prop
end

type nodeType =
    OTHER (* Will not happen *)
  | ELEMENT
  | ATTRIBUTE
  | TEXT
  | CDATA_SECTION
  | ENTITY_REFERENCE
  | ENTITY
  | PROCESSING_INSTRUCTION
  | COMMENT
  | DOCUMENT
  | DOCUMENT_TYPE
  | DOCUMENT_FRAGMENT
  | NOTATION

val document_position_disconnected : int
val document_position_preceding : int
val document_position_following : int
val document_position_contains : int
val document_position_contained_by : int
val document_position_implementation_specific : int

(** Specification of [Node] objects. *)
class type node = object
  method nodeName : js_string t readonly_prop
  method nodeValue : js_string t opt readonly_prop
  method nodeType : nodeType readonly_prop
  method parentNode : node t opt prop
  method childNodes : node nodeList t prop
  method firstChild : node t opt prop
  method lastChild : node t opt prop
  method previousSibling : node t opt prop
  method nextSibling : node t opt prop

  method insertBefore : node t -> node t opt -> node t meth
  method replaceChild : node t -> node t -> node t meth
  method removeChild : node t -> node t meth
  method appendChild : node t -> node t meth
  method hasChildNodes : bool t meth
  method cloneNode : bool t -> node t meth
  method compareDocumentPosition : node t -> int meth
end

(** Specification of [Attr] objects. *)
class type attr = object
  inherit node
  method name : js_string t readonly_prop
  method specified : bool t readonly_prop
  method value : js_string t prop
  method ownerElement : element t prop
end

(** Specification of [NamedNodeMap] objects. *)
and ['node] namedNodeMap = object
  method getNamedItem : js_string t -> 'node t opt meth
  method setNamedItem : 'node t -> 'node t opt meth
  method removeNamedItem : js_string t -> 'node t opt meth
  method item : int -> 'node t opt meth
  method length : int readonly_prop
end

(** Specification of [Element] objects. *)
and element = object
  inherit node
  method tagName : js_string t readonly_prop
  method getAttribute : js_string t -> js_string t opt meth
  method setAttribute : js_string t -> js_string t -> unit meth
  method removeAttribute : js_string t -> unit meth
  method hasAttribute : js_string t -> bool t meth
  method getElementsByTagName : js_string t -> element nodeList t meth
  method attributes : attr namedNodeMap t readonly_prop
end

(** Specification of [CharacterData] objects. *)
class type characterData = object
  inherit node
  method data : js_string t prop
  method length : int readonly_prop
  method subjs_stringData : int -> int -> js_string t meth
  method appendData : js_string t -> unit meth
  method insertData : int -> js_string t -> unit meth
  method deleteData : int -> int -> unit meth
  method replaceData : int -> int -> js_string t -> unit meth
end

(** Specification of [Comment] objects *)
class type comment = characterData

(** Specification of [Text] objects. *)
class type text = characterData

(** Specification of [DocumentFragment] objects. *)
class type documentFragment = node

(** Specification of [Document] objects. *)
class type ['element] document = object
  inherit node
  method documentElement : 'element t readonly_prop
  method createDocumentFragment : documentFragment t meth
  method createElement : js_string t -> 'element t meth
  method createElementNS : js_string t -> js_string t -> 'element t meth
  method createTextNode : js_string t -> text t meth
  method createAttribute : js_string t -> attr t meth
  method createComment : js_string t -> comment t meth
  method getElementById : js_string t -> 'element t opt meth
  method getElementsByTagName : js_string t -> 'element nodeList t meth
  method importNode : element t -> bool t -> 'element t meth
  method adoptNode : element t -> 'element t meth
end

(** {2 Helper functions} *)

val insertBefore : #node t -> #node t -> #node t opt -> unit
  (** [insertBefore p n c] inserts node [n] as child of node [p],
      just before node [c], or as last child if [p] is empty.
      The expression [insertBefore n c p] behave the same as
      [p##insertBefore(n, c)] but avoid the need of coercing the
      different objects to [node t]. *)
val replaceChild : #node t -> #node t -> #node t -> unit
  (** The expression [replaceChild p n c] behave the same as
      [p##replaceChild(n, c)] (replace [c] by [n] in [p])
      but avoid the need of coercing the
      different objects to [node t]. *)
val removeChild : #node t -> #node t -> unit
  (** The expression [removeChild n c] behave the same as
      [n##removeChild(c)] (remove [c] from [n])
      but avoid the need of coercing the
      different objects to [node t]. *)
val appendChild : #node t -> #node t -> unit
  (** The expression [appendChild n c] behave the same as
      [n##appendChild(c)] (appends [c] to [n])
      but avoid the need of coercing the
      different objects to [node t]. *)

val list_of_nodeList : 'a nodeList t -> 'a t list


type node_type =
  | Element of element t
  | Attr of attr t
  | Text of text t
  | Other of node t

val nodeType : #node t -> node_type

module CoerceTo : sig
  val element : #node t -> element t opt
  val text : #node t -> text t opt
  val attr : #node t -> attr t opt
end

(** {2 Events} *)

type (-'a, -'b) event_listener
  (** The type of event listener functions.  The first type parameter
      ['a] is the type of the target object; the second parameter
      ['b] is the type of the event object. *)

class type ['a] event = object
  method _type : js_string t readonly_prop
  method target : 'a t opt readonly_prop
  method currentTarget : 'a t opt readonly_prop

  (* Legacy methods *)
  method srcElement : 'a t opt readonly_prop
end

(** {2 Event handlers} *)

val no_handler : ('a, 'b) event_listener
  (** Void event handler (Javascript [null] value). *)
val handler : (('e #event t as 'b) -> bool t) -> ('a, 'b) event_listener
  (** Create an event handler that invokes the provided function.
      If the handler returns false, the default action is prevented. *)
val full_handler : ('a -> ('e #event t as 'b) -> bool t) -> ('a, 'b) event_listener
  (** Create an event handler that invokes the provided function.
      The event target (implicit parameter [this]) is also passed as
      argument to the function.  *)
val invoke_handler : ('a, 'b) event_listener -> 'a -> 'b -> bool t
  (** Invoke an existing handler.  Useful to chain event handlers. *)

val eventTarget : (< .. > as 'a) #event t -> 'a t
  (** Returns which object is the target of this event.
      It raises [Not_found] in case there is no target
      (if the event has not been triggered yet) *)

type event_listener_id

module Event : sig
  type 'a typ
  val make : string -> 'a typ
end

val addEventListener :
  (< .. > t as 'a) -> 'b Event.typ ->
  ('a, 'b) event_listener -> bool t -> event_listener_id
  (** Add an event listener.  This function matches the
      [addEventListener] DOM method, except that it returns
      an id for removing the listener. *)

val removeEventListener : event_listener_id -> unit
  (** Remove the given event listener. *)

val preventDefault : 'a #event Js.t -> unit
(** Call this to prevent the default handler for the event.
    To stop propagation of the event, call {!Dom_html.stopPropagation}. *)


(** {2 Other DOM objects} *)

class type stringList = object
  method item : int -> js_string t opt meth
  method length : int readonly_prop
  method contains : js_string t -> bool t meth
end
