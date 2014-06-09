(* Js_of_ocaml library
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2014 Hugo Heuzard
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

let js_string_of_float f = (Js.number_of_float f)##toString()
let js_string_of_int i = (Js.number_of_float (float_of_int i))##toString()

module Xml = struct

  type 'a wrap = 'a
  type 'a list_wrap = 'a list

  type uri = string
  let uri_of_string s = s
  let string_of_uri s = s
  type aname = string

  class type biggest_event = object
    inherit Dom_html.event
    inherit Dom_html.mouseEvent
    inherit Dom_html.keyboardEvent
  end

  type biggest_event_handler = biggest_event Js.t -> bool
  type event_handler = Dom_html.event Js.t -> bool
  type mouse_event_handler = Dom_html.mouseEvent Js.t -> bool
  type keyboard_event_handler = Dom_html.keyboardEvent Js.t -> bool
  type attrib_k =
    | Event of biggest_event_handler
    | Attr of Dom.attr Js.t
  type attrib = aname * attrib_k

  let attr name v =
    let a = Dom_html.document##createAttribute(Js.string name) in
    a##value <- v;
    name,Attr a

  let event_handler_of_function f =
    (fun e -> f (Js.Unsafe.coerce e))


  let float_attrib name value : attrib = attr name (js_string_of_float value)
  let int_attrib name value = attr name (js_string_of_int value)
  let string_attrib name value = attr name (Js.string value)
  let space_sep_attrib name values = attr name (Js.string (String.concat " " values))
  let comma_sep_attrib name values = attr name (Js.string (String.concat "," values))
  let event_handler_attrib name (value : event_handler) =       name,Event (value :> (biggest_event_handler))
  let mouse_event_handler_attrib name (value : mouse_event_handler) = name,Event (value :> (biggest_event_handler))
  let keyboard_event_handler_attrib name (value : keyboard_event_handler) = name,Event (value :> (biggest_event_handler))
  let uri_attrib name value = attr name (Js.string value)
  let uris_attrib name values = attr name (Js.string (String.concat " " values))

  (** Element *)

  type elt = Dom.node Js.t
  type ename = string

  let empty () = (Dom_html.document##createDocumentFragment() :> Dom.node Js.t)

  let comment c = (Dom_html.document##createComment (Js.string c) :> Dom.node Js.t)

  let pcdata s = (Dom_html.document##createTextNode (Js.string s) :> Dom.node Js.t)
  let encodedpcdata s = (Dom_html.document##createTextNode (Js.string s) :> Dom.node Js.t)
  let entity e =
    let entity = Dom_html.decode_html_entities (Js.string ("&" ^ e ^ ";")) in
    (Dom_html.document##createTextNode(entity) :> Dom.node Js.t)

  let attach_attribs e l =
    List.iter (fun (n,att) ->
        match att with
        | Attr a -> (Js.Unsafe.coerce e)##setAttributeNode(a)
        | Event h -> Js.Unsafe.set e (Js.string n) (fun ev -> Js.bool (h ev))
      ) l

  let leaf ?(a=[]) name =
    let e = Dom_html.document##createElement(Js.string name) in
    attach_attribs e a;
    (e :> Dom.node Js.t)

  let node ?(a=[]) name children =
    let e = Dom_html.document##createElement(Js.string name) in
    attach_attribs e a;
    List.iter (fun c -> ignore (e##appendChild(c))) children;
    (e :> Dom.node Js.t)

  let cdata s = pcdata s
  let cdata_script s = cdata s
  let cdata_style s = cdata s
end

module Svg = Svg_f.Make(Xml)
module Html5 = Html5_f.Make(Xml)(Svg)

module Xml_wrap = struct
  type 'a t = 'a React.signal
  type 'a tlist = 'a list React.signal
  let return = React.S.const
  let fmap f = React.S.map f
  let nil () = React.S.const []
  let singleton x = React.S.map (fun x -> [x]) x
  let cons x xs = React.S.l2 (fun x xs -> x::xs) x xs
  let map f x = React.S.map (fun x -> List.map f x) x
  let append x y = React.S.l2 (fun x y -> x@y) x y
end

module R = struct
  module Xml_wed = struct
    type 'a wrap = 'a Xml_wrap.t
    type 'a list_wrap = 'a Xml_wrap.tlist
    type uri = Xml.uri
    let string_of_uri = Xml.string_of_uri
    let uri_of_string = Xml.uri_of_string
    type aname = Xml.aname
    type event_handler = Xml.event_handler
    type mouse_event_handler = Xml.mouse_event_handler
    type keyboard_event_handler = Xml.keyboard_event_handler
    type attrib = Xml.attrib

    let attr name f s =
      let a = Dom_html.document##createAttribute(Js.string name) in
      let _ = Xml_wrap.fmap (fun s -> match f s with
          | None -> ()
          | Some v -> a##value <- v) s in
      name,Xml.Attr a

    let float_attrib name s = attr name (fun f -> Some (js_string_of_float f)) s
    let int_attrib name s = attr name (fun f -> Some (js_string_of_int f)) s
    let string_attrib name s = attr name (fun f -> Some (Js.string f)) s
    let space_sep_attrib name s = attr name (fun f -> Some (Js.string (String.concat " " f))) s
    let comma_sep_attrib name s = attr name (fun f -> Some (Js.string (String.concat "," f))) s
    let event_handler_attrib name s = Xml.event_handler_attrib name s
    let mouse_event_handler_attrib name s = Xml.mouse_event_handler_attrib name s
    let keyboard_event_handler_attrib name s = Xml.keyboard_event_handler_attrib name s
    let string_attrib name s = attr name (fun f -> Some (Js.string f)) s
    let uri_attrib name s = attr name (fun f -> Some (Js.string f)) s
    let uris_attrib name s = attr name (fun f -> Some (Js.string (String.concat " " f))) s

    type elt = Xml.elt
    type ename = Xml.ename

    let empty = Xml.empty
    let comment = Xml.comment
    let pcdata s =
      let e = Dom_html.document##createTextNode(Js.string "") in
      let _ = React.S.map (fun s -> e##data <- Js.string s) s in
      (e :> Dom.node Js.t)
    let encodedpcdata s = pcdata s
    let entity s = Xml.entity s
    let leaf = Xml.leaf
    let node ?(a=[]) name l =
      let e = Dom_html.document##createElement(Js.string name) in
      Xml.attach_attribs e a;
      let _ = React.S.map (fun c ->
          while Js.to_bool e##hasChildNodes() do
            Js.Opt.iter (e##lastChild) (fun f -> ignore (e##removeChild(f)))
          done;
          List.iter (fun c -> ignore (e##appendChild(c))) c
        ) l in
      (e :> Dom.node Js.t)
    let cdata = Xml.cdata
    let cdata_script = Xml.cdata_script
    let cdata_style = Xml.cdata_style
  end

  module Svg = Svg_f.MakeWrapped(Xml_wrap)(Xml_wed)
  module Html5 = Html5_f.MakeWrapped(Xml_wrap)(Xml_wed)(Svg)
end

module To_dom = Tyxml_cast.MakeTo(struct
    type 'a elt = 'a Html5.elt
    let elt x = x
  end)

module Of_dom = Tyxml_cast.MakeOf(struct
    type 'a elt = 'a Html5.elt
    let elt x = x
  end)
