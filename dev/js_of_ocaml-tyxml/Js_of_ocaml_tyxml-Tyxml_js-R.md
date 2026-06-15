
# Module `Tyxml_js.R`

```ocaml
module Xml : XML with module W = Wrap
```
```ocaml
module Svg : 
  Svg_sigs.Make(Xml).T
    with type +'a elt = 'a Svg.elt
     and type +'a attrib = 'a Svg.attrib
```
```ocaml
module Html : 
  Html_sigs.Make(Xml)(Svg).T
    with type +'a elt = 'a Html.elt
     and type +'a attrib = 'a Html.attrib
```
```ocaml
val filter_attrib : 'a Html.attrib -> bool React.signal -> 'a Html.attrib
```
```ocaml
module Html5 : 
  Html_sigs.Make(Xml)(Svg).T
    with type +'a elt = 'a Html.elt
     and type +'a attrib = 'a Html.attrib
```