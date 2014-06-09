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

module Xml_wrap : Xml_wrap.T
  with type 'a t = 'a React.signal
   and type 'a tlist = 'a list React.signal

module Svg : Svg_sigs.T
  with module Xml := Xml
   and type 'a wrap = 'a
   and type 'a list_wrap = 'a list

module Html5 : Html5_sigs.T
  with module Xml := Xml
   and module Svg := Svg
   and type 'a wrap = 'a
   and type 'a list_wrap = 'a list

module R : sig
  module Svg : Svg_sigs.T
    with module Xml := Xml
     and type 'a wrap = 'a React.signal
     and type 'a list_wrap = 'a list React.signal
     and type 'a elt = 'a Svg.elt
     and type 'a attrib = 'a Svg.attrib

  module Html5 : Html5_sigs.T
    with module Xml := Xml
     and module Svg := Svg
     and type 'a wrap = 'a React.signal
     and type 'a list_wrap = 'a list React.signal
     and type 'a elt = 'a Html5.elt
     and type 'a attrib = 'a Html5.attrib
     and type uri = Html5.uri
end

module To_dom : Tyxml_cast_sigs.TO with type 'a elt = 'a Html5.elt
module Of_dom : Tyxml_cast_sigs.OF with type 'a elt = 'a Html5.elt
