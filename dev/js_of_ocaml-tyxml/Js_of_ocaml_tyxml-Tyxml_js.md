
# Module `Js_of_ocaml_tyxml.Tyxml_js`

Tyxml interface. Example of use for HTML:

```ocaml
 module T = Tyxml_js.Html
 let html = T.(
   div ~a:[a_class ["several"; "css"; "class"]; a_id "id-of-div"] [
     ul ~a:[a_class ["one-css-class"]; a_id "id-of-ul"] [
       li [
         a ~a:[a_id "id-of-a"; a_href "/url/file.html"] [
           pcdata "Go to /url/file.html"
         ]
       ]
     ]
   ]
 )
```
see [https://ocsigen.org/tyxml/](https://ocsigen.org/tyxml/) the Tyxml project website.
see [https://ocsigen.org/tyxml/dev/api/Html\_sigs.T](https://ocsigen.org/tyxml/dev/api/Html_sigs.T) Html\_sigs.T to have a list of available functions to build HTML.
```ocaml
module type XML =
  Xml_sigs.T
    with type uri = string
     and type event_handler =
           Js_of_ocaml.Dom_html.event Js_of_ocaml.Js.t ->
           bool
     and type mouse_event_handler =
           Js_of_ocaml.Dom_html.mouseEvent Js_of_ocaml.Js.t ->
           bool
     and type keyboard_event_handler =
           Js_of_ocaml.Dom_html.keyboardEvent Js_of_ocaml.Js.t ->
           bool
     and type elt = Js_of_ocaml.Dom.node Js_of_ocaml.Js.t
```
```ocaml
module Xml : XML with module W = Xml_wrap.NoWrap
```
```ocaml
module Svg : Svg_sigs.Make(Xml).T with module Xml.W = Xml_wrap.NoWrap
```
```ocaml
module Html : Html_sigs.Make(Xml)(Svg).T with module Xml.W = Xml_wrap.NoWrap
```
```ocaml
module Html5 : 
  Html_sigs.Make(Xml)(Svg).T
    with module Xml.W = Xml_wrap.NoWrap
     and type 'a elt = 'a Html.elt
     and type +'a attrib = 'a Html.attrib
```
```ocaml
module Register : sig ... end
```
```ocaml
module Wrap : 
  Xml_wrap.T
    with type 'a t = 'a React.signal
     and type 'a tlist = 'a ReactiveData.RList.t
     and type ('a, 'b) ft = 'a -> 'b
```
```ocaml
module Util : sig ... end
```
```ocaml
module R : sig ... end
```
```ocaml
module To_dom : Tyxml_cast_sigs.TO with type 'a elt = 'a Html.elt
```
```ocaml
module Of_dom : Tyxml_cast_sigs.OF with type 'a elt = 'a Html.elt
```