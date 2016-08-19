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


module type XML =
  Xml_sigs.T
  with type uri = string
   and type event_handler = Dom_html.event Js.t -> bool
   and type mouse_event_handler = Dom_html.mouseEvent Js.t -> bool
   and type keyboard_event_handler = Dom_html.keyboardEvent Js.t -> bool
   and type elt = Dom.node Js.t


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
  type attrib_k =
    | Event of event_handler
    | MouseEvent of mouse_event_handler
    | KeyboardEvent of keyboard_event_handler
    | Attr of Js.js_string Js.t option React.S.t
  type attrib = aname * attrib_k

  let attr name v = name,Attr (React.S.const (Some v))

  let float_attrib name value : attrib = attr name (js_string_of_float value)
  let int_attrib name value = attr name (js_string_of_int value)
  let string_attrib name value = attr name (Js.string value)
  let space_sep_attrib name values = attr name (Js.string (String.concat " " values))
  let comma_sep_attrib name values = attr name (Js.string (String.concat "," values))
  let event_handler_attrib name (value : event_handler) = name,Event value
  let mouse_event_handler_attrib name (value : mouse_event_handler) = name,MouseEvent value
  let keyboard_event_handler_attrib name (value : keyboard_event_handler) = name,KeyboardEvent value
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

  (* TODO: fix get_prop
     it only work when html attribute and dom property names correspond.
     find a way to get dom property name corresponding to html attribute
  *)

  let get_prop node name =
    if Js.Optdef.test (Js.Unsafe.get node name)
    then Some name
    else None

  let iter_prop_protected node name f =
    match get_prop node name with
    | Some n -> begin try f n with _ -> () end
    | None -> ()

  let attach_attribs node l =
    List.iter (fun (n',att) ->
      let n = Js.string n' in
        match att with
        | Attr a ->
          (* Note that once we have weak pointers working, we'll need to React.S.retain *)
          let _ : unit React.S.t = React.S.map (function
          | Some v ->
            ignore(node##setAttribute(n, v));
            begin match n' with
            | "style" -> node##style##cssText <- v;
            | _ -> iter_prop_protected node n (fun name -> Js.Unsafe.set node name v)
            end
          | None ->
            ignore(node##removeAttribute(n));
            begin match n' with
            | "style" -> node##style##cssText <- Js.string "";
            | _ -> iter_prop_protected node n (fun name -> Js.Unsafe.set node name Js.null)
            end
          ) a
          in ()
        | Event h -> Js.Unsafe.set node n (fun ev -> Js.bool (h ev))
        | MouseEvent h -> Js.Unsafe.set node n (fun ev -> Js.bool (h ev))
        | KeyboardEvent h -> Js.Unsafe.set node n (fun ev -> Js.bool (h ev))
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

module Xml_Svg = struct
  include Xml

  let leaf ?(a = []) name =
    let e =
      Dom_html.document##createElementNS(Dom_svg.xmlns, Js.string name) in
    attach_attribs e a;
    (e :> Dom.node Js.t)

  let node ?(a = []) name children =
    let e =
      Dom_html.document##createElementNS(Dom_svg.xmlns, Js.string name) in
    attach_attribs e a;
    List.iter (fun c -> ignore (e##appendChild(c))) children;
    (e :> Dom.node Js.t)

end


module Svg = Svg_f.Make(Xml_Svg)
module Html = Html_f.Make(Xml)(Svg)
module Html5 = Html

module To_dom = Tyxml_cast.MakeTo(struct
    type 'a elt = 'a Html.elt
    let elt = Html.toelt
  end)

module Of_dom = Tyxml_cast.MakeOf(struct
    type 'a elt = 'a Html.elt
    let elt = Html.tot
  end)

module Register = struct

  let removeChildren (node : #Dom.element Js.t) =
    let l = node##childNodes in
    for i = 0 to (l##length) - 1 do
      Js.Opt.iter l##item(i) (fun x -> ignore node##removeChild(x))
    done

  let add_to ?(keep=true) node content =
    if not keep then removeChildren node ;
    List.iter
      (fun x -> Dom.appendChild node (To_dom.of_element x))
      content

  let id ?keep id content =
    let node = Dom_html.getElementById id in
    add_to ?keep node content

  let body ?keep content =
    add_to ?keep Dom_html.document##body content

  let head ?keep content =
    add_to ?keep Dom_html.document##head content

  let html ?head body =
    begin match head with
      | Some h -> Dom_html.document##head <- (To_dom.of_head h)
      | None -> ()
    end ;
    Dom_html.document##body <- (To_dom.of_body body) ;
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
    let nodes = dom##childNodes in
    assert (i <= nodes##length);
    if i = nodes##length
    then ignore(dom##appendChild((x :> Dom.node Js.t)))
    else ignore(dom##insertBefore(x,nodes##item(i)))

  let merge_one_patch (dom : Dom.node Js.t) (p : Dom.node Js.t p) =
    match p with
    | I (i,x) ->
      let i = if i < 0 then dom##childNodes##length + 1 + i else i in
      insertAt dom i x
    | R i ->
      let i = if i < 0 then dom##childNodes##length + i else i in
      let nodes = dom##childNodes in
      assert (i >= 0 && i < nodes##length);
      Js.Opt.iter (nodes##item(i)) (fun n -> Dom.removeChild dom n)
    | U (i,x) ->
      let i = if i < 0 then dom##childNodes##length + i else i in
      (match Js.Opt.to_option dom##childNodes##item(i) with
       | Some old -> ignore(dom##replaceChild(x,old))
       | _ -> assert false)
    | X (i,move) ->
      let i = if i < 0 then dom##childNodes##length + i else i in
      if move = 0
      then ()
      else
        begin
          match Js.Opt.to_option dom##childNodes##item(i) with
          | Some i' -> insertAt dom (i+ if move > 0 then move + 1 else move) i'
          | _ -> assert false
        end

  let rec removeChildren dom =
    match Js.Opt.to_option dom##lastChild with
    | None -> ()
    | Some c ->
      ignore(dom##removeChild(c));
      removeChildren dom

  let merge_msg (dom : Dom.node Js.t) (msg : Dom.node Js.t msg)  =
    match msg with
    | Set l ->
      (* Format.eprintf "replace all@."; *)
      removeChildren dom;
      List.iter (fun l -> ignore(dom##appendChild(l))) l;
    | Patch p ->
      (* Format.eprintf "patch@."; *)
      List.iter (merge_one_patch dom) p

  let update_children (dom : Dom.node Js.t) (nodes : Dom.node Js.t t) =
    removeChildren dom;
    (* Note that once we have weak pointers working, we'll need to React.S.retain *)
    let _s : unit React.S.t = fold (fun () msg -> merge_msg dom msg) nodes ()
    in ()
end


module R = struct

  let filter_attrib (name,a) on =
    match a with
    | Xml.Event _
    | Xml.MouseEvent _
    | Xml.KeyboardEvent _ ->
      raise (Invalid_argument "filter_attrib not implemented for event handler")
    | Xml.Attr a ->
      name,
      Xml.Attr
        (React.S.l2
           (fun on a -> if on then a else None) on a)

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
    type attrib = Xml.attrib

    let attr name f s =
      let a = W.fmap f s in
      name,Xml.Attr a

    let float_attrib name s = attr name (fun f -> Some (js_string_of_float f)) s
    let int_attrib name s = attr name (fun f -> Some (js_string_of_int f)) s
    let string_attrib name s = attr name (fun f -> Some (Js.string f)) s
    let space_sep_attrib name s = attr name (fun f -> Some (Js.string (String.concat " " f))) s
    let comma_sep_attrib name s = attr name (fun f -> Some (Js.string (String.concat "," f))) s
    let event_handler_attrib name s = Xml.event_handler_attrib name s
    let mouse_event_handler_attrib name s = Xml.mouse_event_handler_attrib name s
    let keyboard_event_handler_attrib name s = Xml.keyboard_event_handler_attrib name s
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
      let e =
        Dom_html.document##createElementNS(Dom_svg.xmlns,Js.string name) in
      attach_attribs e a;
      Util.update_children (e :> Dom.node Js.t) l;
      (e :> Dom.node Js.t)
  end

  module Svg = Svg_f.Make(Xml_Svg)
  module Html = Html_f.Make(Xml)(Svg)
  module Html5 = Html

end
