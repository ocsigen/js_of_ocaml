
# Class type `Dom_svg.svgElement`

```ocaml
inherit graphicsElement
```
```ocaml
inherit langSpace
```
```ocaml
inherit externalResourcesRequired
```
```ocaml
inherit fitToViewBox
```
```ocaml
inherit zoomAndPan
```
```ocaml
method x : animatedLength Js_of_ocaml__.Js.t Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method y : animatedLength Js_of_ocaml__.Js.t Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method width : animatedLength Js_of_ocaml__.Js.t Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method height : animatedLength Js_of_ocaml__.Js.t
                  Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method contentScriptType : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                             Js_of_ocaml__.Js.optdef
                             Js_of_ocaml__.Js.prop
```
deprecated Removed in SVG 2.
```ocaml
method contentStyleType : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                            Js_of_ocaml__.Js.optdef
                            Js_of_ocaml__.Js.prop
```
deprecated Removed in SVG 2.
```ocaml
method viewport : rect Js_of_ocaml__.Js.t Js_of_ocaml__.Js.optdef
                    Js_of_ocaml__.Js.readonly_prop
```
deprecated Removed in SVG 2.
```ocaml
method pixelUnitToMillimeterX : Js_of_ocaml__.Js.number_t
                                  Js_of_ocaml__.Js.optdef
                                  Js_of_ocaml__.Js.readonly_prop
```
deprecated Removed in SVG 2.
```ocaml
method pixelUnitToMillimeterY : Js_of_ocaml__.Js.number_t
                                  Js_of_ocaml__.Js.optdef
                                  Js_of_ocaml__.Js.readonly_prop
```
deprecated Removed in SVG 2.
```ocaml
method screenPixelUnitToMillimeterX : Js_of_ocaml__.Js.number_t
                                        Js_of_ocaml__.Js.optdef
                                        Js_of_ocaml__.Js.readonly_prop
```
deprecated Removed in SVG 2.
```ocaml
method screenPixelUnitToMillimeterY : Js_of_ocaml__.Js.number_t
                                        Js_of_ocaml__.Js.optdef
                                        Js_of_ocaml__.Js.readonly_prop
```
deprecated Removed in SVG 2.
```ocaml
method useCurrentView : bool Js_of_ocaml__.Js.t Js_of_ocaml__.Js.optdef
                          Js_of_ocaml__.Js.readonly_prop
```
deprecated Removed in SVG 2.
```ocaml
method currentView : viewSpec Js_of_ocaml__.Js.t Js_of_ocaml__.Js.optdef
                       Js_of_ocaml__.Js.readonly_prop
```
deprecated Removed in SVG 2.
```ocaml
method currentScale : Js_of_ocaml__.Js.number_t Js_of_ocaml__.Js.prop
```
```ocaml
method currentTranslate : point Js_of_ocaml__.Js.t
                            Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method suspendRedraw : int -> suspendHandleID Js_of_ocaml__.Js.meth
```
deprecated Deprecated in SVG 2. Has no effect.
```ocaml
method unsuspendRedraw : suspendHandleID -> unit Js_of_ocaml__.Js.meth
```
deprecated Deprecated in SVG 2. Has no effect.
```ocaml
method unsuspendRedrawAll : unit Js_of_ocaml__.Js.meth
```
deprecated Deprecated in SVG 2. Has no effect.
```ocaml
method forceRedraw : unit Js_of_ocaml__.Js.meth
```
deprecated Deprecated in SVG 2. Has no effect.
```ocaml
method pauseAnimations : unit Js_of_ocaml__.Js.meth
```
```ocaml
method unpauseAnimations : unit Js_of_ocaml__.Js.meth
```
```ocaml
method animationsPaused : bool Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
```ocaml
method getCurrentTime : Js_of_ocaml__.Js.number_t Js_of_ocaml__.Js.meth
```
```ocaml
method setCurrentTime : int -> unit Js_of_ocaml__.Js.meth
```
```ocaml
method getIntersectionList : rect Js_of_ocaml__.Js.t ->
  element Js_of_ocaml__.Js.t Js_of_ocaml__.Js.opt ->
  element Js_of_ocaml__.Dom.nodeList Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
```ocaml
method getEnclosureList : rect Js_of_ocaml__.Js.t ->
  element Js_of_ocaml__.Js.t Js_of_ocaml__.Js.opt ->
  element Js_of_ocaml__.Dom.nodeList Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
```ocaml
method checkIntersection : element Js_of_ocaml__.Js.t ->
  rect Js_of_ocaml__.Js.t ->
  bool Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
```ocaml
method checkEnclosure : element Js_of_ocaml__.Js.t ->
  rect Js_of_ocaml__.Js.t ->
  bool Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
```ocaml
method deselectAll : unit Js_of_ocaml__.Js.meth
```
deprecated Deprecated in SVG 2. Use Selection API.
```ocaml
method createSVGNumber : Js_of_ocaml__.Js.number Js_of_ocaml__.Js.t
                           Js_of_ocaml__.Js.meth
```
```ocaml
method createSVGLength : length Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
```ocaml
method createSVGAngle : angle Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
```ocaml
method createSVGPoint : point Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
```ocaml
method createSVGMatrix : matrix Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
```ocaml
method createSVGRect : rect Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
```ocaml
method createSVGTransform : transform Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
```ocaml
method createSVGTransformFromMatrix : matrix Js_of_ocaml__.Js.t ->
  transform Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
```ocaml
method getElementById : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  Js_of_ocaml__.Dom.element Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```