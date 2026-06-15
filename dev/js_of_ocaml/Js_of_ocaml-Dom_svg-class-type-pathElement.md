
# Class type `Dom_svg.pathElement`

```ocaml
inherit element
```
```ocaml
inherit tests
```
```ocaml
inherit langSpace
```
```ocaml
inherit externalResourcesRequired
```
```ocaml
inherit stylable
```
```ocaml
inherit transformable
```
```ocaml
inherit animatedPathData
```
```ocaml
method pathLength : animatedNumber Js_of_ocaml__.Js.t
                      Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method getTotalLength : Js_of_ocaml__.Js.number_t Js_of_ocaml__.Js.meth
```
```ocaml
method getPointAtLength : Js_of_ocaml__.Js.number_t ->
  point Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
```ocaml
method getPathSegAtLength : Js_of_ocaml__.Js.number_t -> int
```
```ocaml
method createSVGPathSegClosePath : pathSegClosePath Js_of_ocaml__.Js.meth
```
```ocaml
method createSVGPathSegMovetoAbs : Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  pathSegMoveto Js_of_ocaml__.Js.meth
```
```ocaml
method createSVGPathSegMovetoRel : Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  pathSegMoveto Js_of_ocaml__.Js.meth
```
```ocaml
method createSVGPathSegLinetoAbs : Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  pathSegLineto Js_of_ocaml__.Js.meth
```
```ocaml
method createSVGPathSegLinetoRel : Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  pathSegLineto Js_of_ocaml__.Js.meth
```
```ocaml
method createSVGPathSegCurvetoCubicAbs : Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  pathSegCurvetoCubic Js_of_ocaml__.Js.meth
```
```ocaml
method createSVGPathSegCurvetoCubicRel : Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  pathSegCurvetoCubic Js_of_ocaml__.Js.meth
```
```ocaml
method createSVGPathSegCurvetoQuadraticAbs : Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  pathSegCurvetoQuadratic Js_of_ocaml__.Js.meth
```
```ocaml
method createSVGPathSegCurvetoQuadraticRel : Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  pathSegCurvetoQuadratic Js_of_ocaml__.Js.meth
```
```ocaml
method createSVGPathSegArcAbs : Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  bool Js_of_ocaml__.Js.t ->
  bool Js_of_ocaml__.Js.t ->
  pathSegArc Js_of_ocaml__.Js.meth
```
```ocaml
method createSVGPathSegArcRel : Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  bool Js_of_ocaml__.Js.t ->
  bool Js_of_ocaml__.Js.t ->
  pathSegArc Js_of_ocaml__.Js.meth
```
```ocaml
method createSVGPathSegLinetoHorizontalAbs : Js_of_ocaml__.Js.number_t ->
  pathSegLinetoHorizontal Js_of_ocaml__.Js.meth
```
```ocaml
method createSVGPathSegLinetoHorizontalRel : Js_of_ocaml__.Js.number_t ->
  pathSegLinetoHorizontal Js_of_ocaml__.Js.meth
```
```ocaml
method createSVGPathSegLinetoVerticalAbs : Js_of_ocaml__.Js.number_t ->
  pathSegLinetoVertical Js_of_ocaml__.Js.meth
```
```ocaml
method createSVGPathSegLinetoVerticalRel : Js_of_ocaml__.Js.number_t ->
  pathSegLinetoVertical Js_of_ocaml__.Js.meth
```
```ocaml
method createSVGPathSegCurvetoCubicSmoothAbs : Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  pathSegCurvetoCubicSmooth Js_of_ocaml__.Js.meth
```
```ocaml
method createSVGPathSegCurvetoCubicSmoothRel : Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  pathSegCurvetoCubicSmooth Js_of_ocaml__.Js.meth
```
```ocaml
method createSVGPathSegCurvetoQuadraticSmoothAbs : Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  pathSegCurvetoQuadraticSmooth Js_of_ocaml__.Js.meth
```
```ocaml
method createSVGPathSegCurvetoQuadraticSmoothRel : Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  pathSegCurvetoQuadraticSmooth Js_of_ocaml__.Js.meth
```