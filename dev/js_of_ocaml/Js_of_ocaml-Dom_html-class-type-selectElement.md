
# Class type `Dom_html.selectElement`

```ocaml
inherit element
```
```ocaml
method _type : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                 Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method selectedIndex : int Js_of_ocaml__.Js.prop
```
```ocaml
method selectedOptions : optionElement collection Js_of_ocaml__.Js.t
                           Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method value : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                 Js_of_ocaml__.Js.prop
```
```ocaml
method length : int Js_of_ocaml__.Js.prop
```
```ocaml
method form : formElement Js_of_ocaml__.Js.t Js_of_ocaml__.Js.opt
                Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method options : optionElement collection Js_of_ocaml__.Js.t
                   Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method disabled : bool Js_of_ocaml__.Js.t Js_of_ocaml__.Js.prop
```
```ocaml
method multiple : bool Js_of_ocaml__.Js.t Js_of_ocaml__.Js.prop
```
```ocaml
method name : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method size : int Js_of_ocaml__.Js.prop
```
```ocaml
method tabIndex : int Js_of_ocaml__.Js.prop
```
```ocaml
method add : optGroupElement Js_of_ocaml__.Js.t ->
  optGroupElement Js_of_ocaml__.Js.t Js_of_ocaml__.Js.opt ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method remove : int -> unit Js_of_ocaml__.Js.meth
```
```ocaml
method required : bool Js_of_ocaml__.Js.t Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method labels : labelElement Js_of_ocaml__.Dom.nodeList Js_of_ocaml__.Js.t
                  Js_of_ocaml__.Js.opt
                  Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method validity : validityState Js_of_ocaml__.Js.t
                    Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method validationMessage : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                             Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method willValidate : bool Js_of_ocaml__.Js.t Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method checkValidity : bool Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
```ocaml
method reportValidity : bool Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
```ocaml
method setCustomValidity : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method onchange : ('self Js_of_ocaml__.Js.t, event Js_of_ocaml__.Js.t)
                    event_listener
                    Js_of_ocaml__.Js.prop
```
```ocaml
method oninput : ('self Js_of_ocaml__.Js.t, event Js_of_ocaml__.Js.t)
                   event_listener
                   Js_of_ocaml__.Js.prop
```