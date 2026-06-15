
# Class type `Dom_html.canvasRenderingContext2D`

```ocaml
method canvas : canvasElement Js_of_ocaml__.Js.t Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method save : unit Js_of_ocaml__.Js.meth
```
```ocaml
method restore : unit Js_of_ocaml__.Js.meth
```
```ocaml
method scale : Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method rotate : Js_of_ocaml__.Js.number_t -> unit Js_of_ocaml__.Js.meth
```
```ocaml
method translate : Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method transform : Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method setTransform : Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method globalAlpha : Js_of_ocaml__.Js.number_t Js_of_ocaml__.Js.prop
```
```ocaml
method globalCompositeOperation : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                                    Js_of_ocaml__.Js.prop
```
```ocaml
method strokeStyle : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                       Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method strokeStyle_gradient : canvasGradient Js_of_ocaml__.Js.t
                                Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method strokeStyle_pattern : canvasPattern Js_of_ocaml__.Js.t
                               Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method fillStyle : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                     Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method fillStyle_gradient : canvasGradient Js_of_ocaml__.Js.t
                              Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method fillStyle_pattern : canvasPattern Js_of_ocaml__.Js.t
                             Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method createLinearGradient : Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  canvasGradient Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
```ocaml
method createRadialGradient : Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  canvasGradient Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
```ocaml
method createPattern : imageElement Js_of_ocaml__.Js.t ->
  Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  canvasPattern Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
```ocaml
method createPattern_fromCanvas : canvasElement Js_of_ocaml__.Js.t ->
  Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  canvasPattern Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
```ocaml
method createPattern_fromVideo : videoElement Js_of_ocaml__.Js.t ->
  Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  canvasPattern Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
```ocaml
method lineWidth : Js_of_ocaml__.Js.number_t Js_of_ocaml__.Js.prop
```
```ocaml
method lineCap : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                   Js_of_ocaml__.Js.prop
```
```ocaml
method lineJoin : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                    Js_of_ocaml__.Js.prop
```
```ocaml
method miterLimit : Js_of_ocaml__.Js.number_t Js_of_ocaml__.Js.prop
```
```ocaml
method shadowOffsetX : Js_of_ocaml__.Js.number_t Js_of_ocaml__.Js.prop
```
```ocaml
method shadowOffsetY : Js_of_ocaml__.Js.number_t Js_of_ocaml__.Js.prop
```
```ocaml
method shadowBlur : Js_of_ocaml__.Js.number_t Js_of_ocaml__.Js.prop
```
```ocaml
method shadowColor : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                       Js_of_ocaml__.Js.prop
```
```ocaml
method clearRect : Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method fillRect : Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method strokeRect : Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method beginPath : unit Js_of_ocaml__.Js.meth
```
```ocaml
method closePath : unit Js_of_ocaml__.Js.meth
```
```ocaml
method moveTo : Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method lineTo : Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method quadraticCurveTo : Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method bezierCurveTo : Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method arcTo : Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method rect : Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method arc : Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  bool Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method ellipse : Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  bool Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method fill : unit Js_of_ocaml__.Js.meth
```
```ocaml
method stroke : unit Js_of_ocaml__.Js.meth
```
```ocaml
method clip : unit Js_of_ocaml__.Js.meth
```
```ocaml
method isPointInPath : Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  bool Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
```ocaml
method drawFocusRing : element Js_of_ocaml__.Js.t ->
  Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  bool Js_of_ocaml__.Js.t ->
  bool Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
```ocaml
method font : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                Js_of_ocaml__.Js.prop
```
```ocaml
method textAlign : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                     Js_of_ocaml__.Js.prop
```
```ocaml
method textBaseline : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                        Js_of_ocaml__.Js.prop
```
```ocaml
method fillText : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method fillText_withWidth : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method strokeText : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method strokeText_withWidth : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method measureText : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  textMetrics Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
```ocaml
method drawImage : imageElement Js_of_ocaml__.Js.t ->
  Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method drawImage_withSize : imageElement Js_of_ocaml__.Js.t ->
  Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method drawImage_full : imageElement Js_of_ocaml__.Js.t ->
  Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method drawImage_fromCanvas : canvasElement Js_of_ocaml__.Js.t ->
  Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method drawImage_fromCanvasWithSize : canvasElement Js_of_ocaml__.Js.t ->
  Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method drawImage_fullFromCanvas : canvasElement Js_of_ocaml__.Js.t ->
  Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method drawImage_fromVideoWithVideo : videoElement Js_of_ocaml__.Js.t ->
  Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method drawImage_fromVideoWithSize : videoElement Js_of_ocaml__.Js.t ->
  Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method drawImage_fullFromVideo : videoElement Js_of_ocaml__.Js.t ->
  Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method createImageData : int ->
  int ->
  imageData Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
```ocaml
method getImageData : Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  imageData Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
```ocaml
method putImageData : imageData Js_of_ocaml__.Js.t ->
  Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  unit Js_of_ocaml__.Js.meth
```