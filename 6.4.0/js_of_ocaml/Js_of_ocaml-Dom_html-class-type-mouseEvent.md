
# Class type `Dom_html.mouseEvent`

```ocaml
inherit event
```
```ocaml
method relatedTarget : element Js_of_ocaml__.Js.t Js_of_ocaml__.Js.opt
                         Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method clientX : Js_of_ocaml__.Js.number_t Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method clientY : Js_of_ocaml__.Js.number_t Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method x : Js_of_ocaml__.Js.number_t Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method y : Js_of_ocaml__.Js.number_t Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method screenX : Js_of_ocaml__.Js.number_t Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method screenY : Js_of_ocaml__.Js.number_t Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method offsetX : Js_of_ocaml__.Js.number_t Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method offsetY : Js_of_ocaml__.Js.number_t Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method ctrlKey : bool Js_of_ocaml__.Js.t Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method shiftKey : bool Js_of_ocaml__.Js.t Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method altKey : bool Js_of_ocaml__.Js.t Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method metaKey : bool Js_of_ocaml__.Js.t Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method button : int Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method buttons : int Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method getModifierState : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  bool Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
```ocaml
method which : mouse_button Js_of_ocaml__.Js.optdef
                 Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method fromElement : element Js_of_ocaml__.Js.t Js_of_ocaml__.Js.opt
                       Js_of_ocaml__.Js.optdef
                       Js_of_ocaml__.Js.readonly_prop
```
deprecated Use relatedTarget instead.
```ocaml
method toElement : element Js_of_ocaml__.Js.t Js_of_ocaml__.Js.opt
                     Js_of_ocaml__.Js.optdef
                     Js_of_ocaml__.Js.readonly_prop
```
deprecated Use relatedTarget instead.
```ocaml
method pageX : Js_of_ocaml__.Js.number_t Js_of_ocaml__.Js.optdef
                 Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method pageY : Js_of_ocaml__.Js.number_t Js_of_ocaml__.Js.optdef
                 Js_of_ocaml__.Js.readonly_prop
```