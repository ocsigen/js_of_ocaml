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

open Js_of_ocaml
open! Import

let js_string_of_float f = (Js.number_of_float f)##toString

let js_string_of_int i = (Js.number_of_float (float_of_int i))##toString

module type XML =
  Xml_sigs.T
    with type uri = string
     and type event_handler = Dom_html.event Js.t -> bool
     and type mouse_event_handler = Dom_html.mouseEvent Js.t -> bool
     and type keyboard_event_handler = Dom_html.keyboardEvent Js.t -> bool
     and type elt = Dom.node Js.t

class type ['a, 'b] weakMap = object
  method set : 'a -> 'b -> unit Js.meth

  method get : 'a -> 'b Js.Optdef.t Js.meth
end

let retain =
  let map : (Dom.node Js.t, Obj.t Js.js_array Js.t) weakMap Js.t =
    let weakMap = Js.Unsafe.global##._WeakMap in
    new%js weakMap
  in
  fun (type a) node ~(keepme : a) ->
    let prev =
      Js.Optdef.case (map##get node) (fun () -> new%js Js.array_empty) (fun x -> x)
    in
    let (_ : int) = prev##push (Obj.repr keepme) in
    map##set node prev

module Xml = struct
  module W = Xml_wrap.NoWrap

  type 'a wrap = 'a

  type 'a list_wrap = 'a list

  type uri = string

  let uri_of_string s = s

  let string_of_uri s = s

  type aname = string

  type event_handler = Dom_html.event Js.t -> bool

  type mouse_event_handler = Dom_html.mouseEvent Js.t -> bool

  type keyboard_event_handler = Dom_html.keyboardEvent Js.t -> bool

  type touch_event_handler = Dom_html.touchEvent Js.t -> bool

  type attrib_k =
    | Event of event_handler
    | MouseEvent of mouse_event_handler
    | KeyboardEvent of keyboard_event_handler
    | TouchEvent of touch_event_handler
    | Attr of Js.js_string Js.t option React.S.t

  type attrib = aname * attrib_k

  let attr name v = name, Attr (React.S.const (Some v))

  let float_attrib name value : attrib = attr name (js_string_of_float value)

  let int_attrib name value = attr name (js_string_of_int value)

  let string_attrib name value = attr name (Js.string value)

  let space_sep_attrib name values = attr name (Js.string (String.concat " " values))

  let comma_sep_attrib name values = attr name (Js.string (String.concat "," values))

  let event_handler_attrib name (value : event_handler) = name, Event value

  let mouse_event_handler_attrib name (value : mouse_event_handler) =
    name, MouseEvent value

  let keyboard_event_handler_attrib name (value : keyboard_event_handler) =
    name, KeyboardEvent value

  let touch_event_handler_attrib name (value : touch_event_handler) =
    name, TouchEvent value

  let uri_attrib name value = attr name (Js.string value)

  let uris_attrib name values = attr name (Js.string (String.concat " " values))

  (** Element *)

  type elt = Dom.node Js.t

  type ename = string

  let empty () = (Dom_html.document##createDocumentFragment :> Dom.node Js.t)

  let comment c = (Dom_html.document##createComment (Js.string c) :> Dom.node Js.t)

  let pcdata s = (Dom_html.document##createTextNode (Js.string s) :> Dom.node Js.t)

  let encodedpcdata s = (Dom_html.document##createTextNode (Js.string s) :> Dom.node Js.t)

  let entity =
    let string_fold s ~pos ~init ~f =
      let r = ref init in
      for i = pos to String.length s - 1 do
        let c = s.[i] in
        r := f !r c
      done;
      !r
    in
    let invalid_entity e = failwith (Printf.sprintf "Invalid entity %S" e) in
    let int_of_char = function
      | '0' .. '9' as x -> Some (Char.code x - Char.code '0')
      | 'a' .. 'f' as x -> Some (Char.code x - Char.code 'a' + 10)
      | 'A' .. 'F' as x -> Some (Char.code x - Char.code 'A' + 10)
      | _ -> None
    in
    let parse_int ~pos ~base e =
      string_fold e ~pos ~init:0 ~f:(fun acc x ->
          match int_of_char x with
          | Some d when d < base -> (acc * base) + d
          | Some _ | None -> invalid_entity e)
    in
    let is_alpha_num = function
      | '0' .. '9' | 'a' .. 'z' | 'A' .. 'Z' -> true
      | _ -> false
    in
    fun e ->
      let len = String.length e in
      let str =
        if len >= 1 && Char.equal e.[0] '#'
        then
          let i =
            if len >= 2 && (Char.equal e.[1] 'x' || Char.equal e.[1] 'X')
            then parse_int ~pos:2 ~base:16 e
            else parse_int ~pos:1 ~base:10 e
          in
          Js.string_constr##fromCharCode i
        else if
          string_fold e ~pos:0 ~init:true ~f:(fun acc x ->
              (* This is not quite right according to
                       https://www.xml.com/axml/target.html#NT-Name.
                       but it seems to cover all html5 entities
                       https://dev.w3.org/html5/html-author/charref *)
              acc && is_alpha_num x)
        then
          match e with
          | "quot" -> Js.string "\""
          | "amp" -> Js.string "&"
          | "apos" -> Js.string "'"
          | "lt" -> Js.string "<"
          | "gt" -> Js.string ">"
          | "" -> invalid_entity e
          | _ -> Dom_html.decode_html_entities (Js.string ("&" ^ e ^ ";"))
        else invalid_entity e
      in
      (Dom_html.document##createTextNode str :> Dom.node Js.t)

  (* TODO: fix get_prop
     it only work when html attribute and dom property names correspond.
     find a way to get dom property name corresponding to html attribute
  *)

  let get_prop node name =
    if Js.Optdef.test (Js.Unsafe.get node name) then Some name else None

  let iter_prop_protected node name f =
    match get_prop node name with
    | Some n -> ( try f n with _ -> ())
    | None -> ()

  let attach_attribs node l =
    List.iter
      (fun (n', att) ->
        let n = Js.string n' in
        match att with
        | Attr a ->
            let (keepme : unit React.S.t) =
              React.S.map
                (function
                  | Some v -> (
                      ignore (node##setAttribute n v);
                      match n' with
                      | "style" -> node##.style##.cssText := v
                      | _ ->
                          iter_prop_protected node n (fun name ->
                              Js.Unsafe.set node name v))
                  | None -> (
                      ignore (node##removeAttribute n);
                      match n' with
                      | "style" -> node##.style##.cssText := Js.string ""
                      | _ ->
                          iter_prop_protected node n (fun name ->
                              Js.Unsafe.set node name Js.null)))
                a
            in
            retain (node :> Dom.node Js.t) ~keepme
        | Event h -> Js.Unsafe.set node n (Js.wrap_callback (fun ev -> Js.bool (h ev)))
        | MouseEvent h ->
            Js.Unsafe.set node n (Js.wrap_callback (fun ev -> Js.bool (h ev)))
        | KeyboardEvent h ->
            Js.Unsafe.set node n (Js.wrap_callback (fun ev -> Js.bool (h ev)))
        | TouchEvent h ->
            Js.Unsafe.set node n (Js.wrap_callback (fun ev -> Js.bool (h ev))))
      l

  let leaf ?(a = []) name =
    let e = Dom_html.document##createElement (Js.string name) in
    attach_attribs e a;
    (e :> Dom.node Js.t)

  let node ?(a = []) name children =
    let e = Dom_html.document##createElement (Js.string name) in
    attach_attribs e a;
    List.iter (fun c -> ignore (e##appendChild c)) children;
    (e :> Dom.node Js.t)

  let cdata s = pcdata s

  let cdata_script s = cdata s

  let cdata_style s = cdata s
end

module Xml_Svg = struct
  include Xml

  let leaf ?(a = []) name =
    let e = Dom_html.document##createElementNS Dom_svg.xmlns (Js.string name) in
    attach_attribs e a;
    (e :> Dom.node Js.t)

  let node ?(a = []) name children =
    let e = Dom_html.document##createElementNS Dom_svg.xmlns (Js.string name) in
    attach_attribs e a;
    List.iter (fun c -> ignore (e##appendChild c)) children;
    (e :> Dom.node Js.t)
end

module Svg = Svg_f.Make (Xml_Svg)
module Html = Html_f.Make (Xml) (Svg)
module Html5 = Html

module To_dom = Tyxml_cast.MakeTo (struct
  type 'a elt = 'a Html.elt

  let elt = Html.toelt
end)

module Of_dom = Tyxml_cast.MakeOf (struct
  type 'a elt = 'a Html.elt

  let elt = Html.tot
end)

module Register = struct
  let removeChildren (node : #Dom.element Js.t) =
    let l = node##.childNodes in
    for i = 0 to l##.length - 1 do
      Js.Opt.iter (l##item i) (fun x -> ignore (node##removeChild x))
    done

  let add_to ?(keep = true) node content =
    if not keep then removeChildren node;
    List.iter (fun x -> Dom.appendChild node (To_dom.of_element x)) content

  let id ?keep id content =
    let node = Dom_html.getElementById id in
    add_to ?keep node content

  let body ?keep content = add_to ?keep Dom_html.document##.body content

  let head ?keep content = add_to ?keep Dom_html.document##.head content

  let html ?head body =
    (match head with
    | Some h -> Dom_html.document##.head := To_dom.of_head h
    | None -> ());
    Dom_html.document##.body := To_dom.of_body body;
    ()
end

module Wrap = struct
  type 'a t = 'a React.signal

  type 'a tlist = 'a ReactiveData.RList.t

  type ('a, 'b) ft = 'a -> 'b

  let return = React.S.const

  let fmap f = React.S.map f

  let nil () = ReactiveData.RList.empty

  let singleton = ReactiveData.RList.singleton_s

  let cons x xs = ReactiveData.RList.concat (singleton x) xs

  let map f = ReactiveData.RList.map f

  let append x y = ReactiveData.RList.concat x y
end

module Util = struct
  open ReactiveData
  open RList

  let insertAt dom i x =
    let nodes = dom##.childNodes in
    assert (i <= nodes##.length);
    if i = nodes##.length
    then ignore (dom##appendChild (x :> Dom.node Js.t))
    else ignore (dom##insertBefore x (nodes##item i))

  let merge_one_patch (dom : Dom.node Js.t) (p : Dom.node Js.t p) =
    match p with
    | I (i, x) ->
        let i = if i < 0 then dom##.childNodes##.length + 1 + i else i in
        insertAt dom i x
    | R i ->
        let i = if i < 0 then dom##.childNodes##.length + i else i in
        let nodes = dom##.childNodes in
        assert (i >= 0 && i < nodes##.length);
        Js.Opt.iter (nodes##item i) (fun n -> Dom.removeChild dom n)
    | U (i, x) -> (
        let i = if i < 0 then dom##.childNodes##.length + i else i in
        match Js.Opt.to_option (dom##.childNodes##item i) with
        | Some old -> ignore (dom##replaceChild x old)
        | _ -> assert false)
    | X (i, move) -> (
        let i = if i < 0 then dom##.childNodes##.length + i else i in
        if move = 0
        then ()
        else
          match Js.Opt.to_option (dom##.childNodes##item i) with
          | Some i' -> insertAt dom (i + if move > 0 then move + 1 else move) i'
          | _ -> assert false)

  let rec removeChildren dom =
    match Js.Opt.to_option dom##.lastChild with
    | None -> ()
    | Some c ->
        ignore (dom##removeChild c);
        removeChildren dom

  let merge_msg (dom : Dom.node Js.t) (msg : Dom.node Js.t msg) =
    match msg with
    | Set l ->
        (* Format.eprintf "replace all@."; *)
        removeChildren dom;
        List.iter (fun l -> ignore (dom##appendChild l)) l
    | Patch p ->
        (* Format.eprintf "patch@."; *)
        List.iter (merge_one_patch dom) p

  let update_children (dom : Dom.node Js.t) (nodes : Dom.node Js.t t) =
    removeChildren dom;
    let keepme : unit React.S.t = fold (fun () msg -> merge_msg dom msg) nodes () in
    retain (dom : Dom.node Js.t) ~keepme;
    ()
end

module R = struct
  let filter_attrib (name, a) on =
    match a with
    | Xml.Event _ | Xml.MouseEvent _ | Xml.KeyboardEvent _ | Xml.TouchEvent _ ->
        raise (Invalid_argument "filter_attrib not implemented for event handler")
    | Xml.Attr a -> name, Xml.Attr (React.S.l2 (fun on a -> if on then a else None) on a)

  let attach_attribs = Xml.attach_attribs

  module Xml = struct
    module W = Wrap

    type 'a wrap = 'a W.t

    type 'a list_wrap = 'a W.tlist

    type uri = Xml.uri

    let string_of_uri = Xml.string_of_uri

    let uri_of_string = Xml.uri_of_string

    type aname = Xml.aname

    type event_handler = Xml.event_handler

    type mouse_event_handler = Xml.mouse_event_handler

    type keyboard_event_handler = Xml.keyboard_event_handler

    type touch_event_handler = Xml.touch_event_handler

    type attrib = Xml.attrib

    let attr name f s =
      let a = W.fmap f s in
      name, Xml.Attr a

    let float_attrib name s = attr name (fun f -> Some (js_string_of_float f)) s

    let int_attrib name s = attr name (fun f -> Some (js_string_of_int f)) s

    let string_attrib name s = attr name (fun f -> Some (Js.string f)) s

    let space_sep_attrib name s =
      attr name (fun f -> Some (Js.string (String.concat " " f))) s

    let comma_sep_attrib name s =
      attr name (fun f -> Some (Js.string (String.concat "," f))) s

    let event_handler_attrib name s = Xml.event_handler_attrib name s

    let mouse_event_handler_attrib name s = Xml.mouse_event_handler_attrib name s

    let keyboard_event_handler_attrib name s = Xml.keyboard_event_handler_attrib name s

    let touch_event_handler_attrib name s = Xml.touch_event_handler_attrib name s

    let uri_attrib name s = attr name (fun f -> Some (Js.string f)) s

    let uris_attrib name s = attr name (fun f -> Some (Js.string (String.concat " " f))) s

    type elt = Xml.elt

    type ename = Xml.ename

    let empty = Xml.empty

    let comment = Xml.comment

    let pcdata s =
      let e = Dom_html.document##createTextNode (Js.string "") in
      let keepme = React.S.map (fun s -> e##.data := Js.string s) s in
      retain (e :> Dom.node Js.t) ~keepme;
      (e :> Dom.node Js.t)

    let encodedpcdata s = pcdata s

    let entity s = Xml.entity s

    let leaf = Xml.leaf

    let node ?(a = []) name l =
      let e = Dom_html.document##createElement (Js.string name) in
      attach_attribs e a;
      Util.update_children (e :> Dom.node Js.t) l;
      (e :> Dom.node Js.t)

    let cdata = Xml.cdata

    let cdata_script = Xml.cdata_script

    let cdata_style = Xml.cdata_style
  end

  module Xml_Svg = struct
    include Xml

    let leaf = Xml_Svg.leaf

    let node ?(a = []) name l =
      let e = Dom_html.document##createElementNS Dom_svg.xmlns (Js.string name) in
      attach_attribs e a;
      Util.update_children (e :> Dom.node Js.t) l;
      (e :> Dom.node Js.t)
  end

  module Svg = Svg_f.Make (Xml_Svg)
  module Html = Html_f.Make (Xml) (Svg)
  module Html5 = Html
end
