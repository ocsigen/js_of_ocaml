
# Class type `Dom_svg.locatable`

deprecated Replaced by SVGGraphicsElement in SVG 2.
```ocaml
method nearestViewportElement : element Js_of_ocaml__.Js.t Js_of_ocaml__.Js.opt
                                  Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method farthestViewportElement : element Js_of_ocaml__.Js.t
                                   Js_of_ocaml__.Js.opt
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
method getTransformToElement : element Js_of_ocaml__.Js.t ->
  matrix Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
deprecated Removed in SVG 2. Use getScreenCTM and matrix inversion.