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

open Js

class type ['node] nodeList = object
  method item : int -> 'node t opt meth
  method length : int readonly_prop
end

let list_of_nodeList (nodeList:'a nodeList t) =
  let length = nodeList##length in
  let rec add_item acc i =
    if i < length
    then
      match Opt.to_option (nodeList##item(i)) with
	| None -> add_item acc (i+1)
	| Some e -> add_item (e::acc) (i+1)
    else List.rev acc
  in
  add_item [] 0

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

let document_position_disconnected = 0x01
let document_position_preceding    = 0x02
let document_position_following    = 0x04
let document_position_contains     = 0x08
let document_position_contained_by = 0x10
let document_position_implementation_specific = 0x20

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

let appendChild (p : #node t) (n : #node t) =
  ignore (p##appendChild ((n :> node t)))

let removeChild (p : #node t) (n : #node t) =
  ignore (p##removeChild ((n :> node t)))

let replaceChild (p : #node t) (n : #node t) (o : #node t) =
  ignore (p##replaceChild ((n :> node t), (o :> node t)))

let insertBefore (p : #node t) (n : #node t) (o : #node t opt) =
  ignore (p##insertBefore ((n :> node t), (o :> node t opt)))

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

class type comment = characterData

class type text = characterData

class type documentFragment = node

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

type node_type =
  | Element of element t
  | Attr of attr t
  | Text of text t
  | Other of node t

let nodeType e =
  match e##nodeType with
    | ELEMENT -> Element (Js.Unsafe.coerce e)
    | ATTRIBUTE -> Attr (Js.Unsafe.coerce e)
    | CDATA_SECTION
    | TEXT -> Text (Js.Unsafe.coerce e)
    | _ -> Other (e:>node t)

module CoerceTo = struct

  let cast (e:#node Js.t) t =
    if e##nodeType = t
    then Js.some (Js.Unsafe.coerce e)
    else Js.null

  let element e : element Js.t Js.opt = cast e ELEMENT

  let text e : text Js.t Js.opt =
    if e##nodeType == TEXT || e##nodeType == CDATA_SECTION
    then Js.some (Js.Unsafe.coerce e)
    else Js.null

  let attr e : attr Js.t Js.opt = cast e ATTRIBUTE

end

type ('a, 'b) event_listener = ('a, 'b -> bool t) meth_callback opt
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

let no_handler : ('a, 'b) event_listener = Js.null
let window_event () : 'a #event t = Js.Unsafe.variable "event"
(* The function preventDefault must be called explicitely when
   using addEventListener... *)
let handler f =
  Js.some (Js.wrap_callback
    (fun e ->
      (* depending on the internet explorer version, e can be 0, null
	 or undefined. This is the only way I know to test them all *)
      if not (Obj.magic e)
      then
        let e = window_event () in
        let res = f e in
        if not (Js.to_bool res)
        then e##returnValue <- res;
	res
      else
	let res = f e in
        if not (Js.to_bool res) then
          (Js.Unsafe.coerce e)##preventDefault ();
        res))
let full_handler f =
  Js.some (Js.wrap_meth_callback
    (fun this e ->
      (* depending on the internet explorer version, e can be 0, null
	 or undefined. This is the only way I know to test them all *)
      if not (Obj.magic e)
      then
        let e = window_event () in
        let res = f this e in
        if not (Js.to_bool res)
        then e##returnValue <- res;
        res
      else
        let res = f this e in
        if not (Js.to_bool res) then
          (Js.Unsafe.coerce e)##preventDefault ();
        res))
let invoke_handler
  (f : ('a, 'b) event_listener) (this : 'a) (event : 'b) : bool t =
  Js.Unsafe.call f this [|Js.Unsafe.inject event|]

let eventTarget (e: (< .. > as 'a) #event t) : 'a t =
  let target =
    Opt.get (e##target) (fun () ->
    Opt.get (e##srcElement) (fun () -> raise Not_found))
  in
  if Js.instanceof target (Js.Unsafe.global ## _Node)
  then
    (* Workaround for Safari bug *)
    let target' : node Js.t = Js.Unsafe.coerce target in
    if target'##nodeType == TEXT then
      Js.Unsafe.coerce (Opt.get (target'##parentNode) (fun () -> assert false))
    else
      target
  else target

module Event = struct
  type 'a typ = Js.js_string Js.t
  let make s = Js.string s
end

type event_listener_id = unit -> unit

let addEventListener (e : (< .. > as 'a) t) typ h capt =
  if (Js.Unsafe.coerce e)##addEventListener == Js.undefined then begin
    let ev = (Js.string "on")##concat(typ) in
    let callback = fun e -> Js.Unsafe.call (h, e, [||]) in
    (Js.Unsafe.coerce e)##attachEvent(ev, callback);
    fun () -> (Js.Unsafe.coerce e)##detachEvent(ev, callback)
  end else begin
    (Js.Unsafe.coerce e)##addEventListener(typ, h, capt);
    fun () -> (Js.Unsafe.coerce e)##removeEventListener (typ, h, capt)
  end

let removeEventListener id = id ()

let preventDefault ev =
  if Js.Optdef.test ((Js.Unsafe.coerce ev)##preventDefault) (* IE hack *)
  then (Js.Unsafe.coerce ev)##preventDefault()
  else (Js.Unsafe.coerce ev)##returnValue <- Js.bool false (* IE < 9 *)

class type stringList = object
  method item : int -> js_string t opt meth
  method length : int readonly_prop
  method contains : js_string t -> bool t meth
end
