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
and namedNodeMap = object
  method getNamedItem : js_string t -> node t opt meth
  method setNamedItem : node t -> node t opt meth
  method removeNamedItem : js_string t -> node t opt meth
  method item : int -> node t opt meth
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
  method attributes : namedNodeMap t readonly_prop
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

class type text = characterData

class type documentFragment = node

class type ['element] document = object
  inherit node
  method documentElement : 'element t readonly_prop
  method createDocumentFragment : documentFragment t meth
  method createElement : js_string t -> 'element t meth
  method createTextNode : js_string t -> text t meth
  method createAttribute : js_string t -> attr t meth
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
