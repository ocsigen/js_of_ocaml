
# Class type `Dom_html.pointerEvent`

```ocaml
inherit mouseEvent
```
```ocaml
method pointerId : int Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method width : Js_of_ocaml__.Js.number_t Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method height : Js_of_ocaml__.Js.number_t Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method pressure : Js_of_ocaml__.Js.number_t Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method tangentialPressure : Js_of_ocaml__.Js.number_t
                              Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method tiltX : int Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method tiltY : int Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method twist : int Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method altitudeAngle : Js_of_ocaml__.Js.number_t Js_of_ocaml__.Js.readonly_prop
```
Angle in radians between the pointer axis and the surface (Pointer Events level 3\).

```ocaml
method azimuthAngle : Js_of_ocaml__.Js.number_t Js_of_ocaml__.Js.readonly_prop
```
Angle in radians of the pointer projection on the surface (Pointer Events level 3\).

```ocaml
method pointerType : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                       Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method isPrimary : bool Js_of_ocaml__.Js.t Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method getCoalescedEvents : pointerEvent Js_of_ocaml__.Js.t
                              Js_of_ocaml__.Js.js_array
                              Js_of_ocaml__.Js.t
                              Js_of_ocaml__.Js.meth
```
```ocaml
method getPredictedEvents : pointerEvent Js_of_ocaml__.Js.t
                              Js_of_ocaml__.Js.js_array
                              Js_of_ocaml__.Js.t
                              Js_of_ocaml__.Js.meth
```
Predicted future pointer events, as computed by the browser.
