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

(** Tyxml interface *)

module Xml : Xml_sigs.Wrapped
  with type uri = string
   and type event_handler = Dom_html.event Js.t -> bool
   and type mouse_event_handler = Dom_html.mouseEvent Js.t -> bool
   and type keyboard_event_handler = Dom_html.keyboardEvent Js.t -> bool
   and type elt = Dom.node Js.t

module D : sig
  module Svg : Svg_sigs.T
  with type Xml.uri = Xml.uri
   and type Xml.event_handler = Xml.event_handler
   and type Xml.mouse_event_handler = Xml.mouse_event_handler
   and type Xml.keyboard_event_handler = Xml.keyboard_event_handler
   and type Xml.attrib = Xml.attrib
   and type Xml.elt = Xml.elt
   and type 'a Xml.wrap = 'a
   and type 'a wrap = 'a

  module Raw : Html5_sigs.T
    with type Xml.uri = Xml.uri
     and type Xml.event_handler = Xml.event_handler
     and type Xml.mouse_event_handler = Xml.mouse_event_handler
     and type Xml.keyboard_event_handler = Xml.keyboard_event_handler
     and type Xml.attrib = Xml.attrib
     and type Xml.elt = Xml.elt
     and type 'a Xml.wrap = 'a
     and type 'a wrap = 'a
     and type +'a attrib = Xml.attrib
     and module Svg := Svg
  include module type of Raw
end

module R: sig
  module Raw : Html5_sigs.T
    with type Xml.uri = Xml.uri
     and type Xml.event_handler = Xml.event_handler
     and type Xml.mouse_event_handler = Xml.mouse_event_handler
     and type Xml.keyboard_event_handler = Xml.keyboard_event_handler
     and type Xml.attrib = Xml.attrib
     and type Xml.elt = Xml.elt
     and module Svg := D.Svg
     and type 'a elt = 'a D.elt
     and type 'a Xml.wrap = 'a React.signal
     and type 'a wrap = 'a React.signal
     and type 'a attrib = 'a D.attrib
     and type uri = D.uri
  include module type of Raw
end

module To_dom : Tyxml_cast_sigs.TO with type 'a elt = 'a D.elt
module Of_dom : Tyxml_cast_sigs.OF with type 'a elt = 'a D.elt
