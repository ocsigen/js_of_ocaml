
# Class type `Dom_svg.graphicsElement`

```ocaml
inherit element
```
```ocaml
inherit tests
```
```ocaml
method transform : animatedTransformList Js_of_ocaml__.Js.t
                     Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method getBBox : rect Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
```ocaml
method getCTM : matrix Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
```ocaml
method getScreenCTM : matrix Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
```ocaml
method nearestViewportElement : element Js_of_ocaml__.Js.t Js_of_ocaml__.Js.opt
                                  Js_of_ocaml__.Js.optdef
                                  Js_of_ocaml__.Js.readonly_prop
```
deprecated Removed in SVG 2 (and from Safari); gated behind a pref in Firefox but still present, deprecated, in Chrome/Edge. optdef reflects its absence in engines that dropped it.
```ocaml
method farthestViewportElement : element Js_of_ocaml__.Js.t
                                   Js_of_ocaml__.Js.opt
                                   Js_of_ocaml__.Js.optdef
                                   Js_of_ocaml__.Js.readonly_prop
```
deprecated Removed in SVG 2 (and from Safari); gated behind a pref in Firefox but still present, deprecated, in Chrome/Edge. optdef reflects its absence in engines that dropped it.