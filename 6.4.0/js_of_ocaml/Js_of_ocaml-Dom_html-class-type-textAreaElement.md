
# Class type `Dom_html.textAreaElement`

```ocaml
inherit element
```
```ocaml
method defaultValue : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                        Js_of_ocaml__.Js.prop
```
```ocaml
method form : formElement Js_of_ocaml__.Js.t Js_of_ocaml__.Js.opt
                Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method accessKey : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                     Js_of_ocaml__.Js.prop
```
```ocaml
method autocomplete : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                        Js_of_ocaml__.Js.prop
```
```ocaml
method cols : int Js_of_ocaml__.Js.prop
```
```ocaml
method dirName : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                   Js_of_ocaml__.Js.prop
```
```ocaml
method disabled : bool Js_of_ocaml__.Js.t Js_of_ocaml__.Js.prop
```
```ocaml
method maxLength : int Js_of_ocaml__.Js.prop
```
```ocaml
method minLength : int Js_of_ocaml__.Js.prop
```
```ocaml
method name : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method readOnly : bool Js_of_ocaml__.Js.t Js_of_ocaml__.Js.prop
```
```ocaml
method rows : int Js_of_ocaml__.Js.prop
```
```ocaml
method selectionDirection : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                              Js_of_ocaml__.Js.prop
```
```ocaml
method selectionEnd : int Js_of_ocaml__.Js.prop
```
```ocaml
method selectionStart : int Js_of_ocaml__.Js.prop
```
```ocaml
method tabIndex : int Js_of_ocaml__.Js.prop
```
```ocaml
method _type : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                 Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method value : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                 Js_of_ocaml__.Js.prop
```
```ocaml
method wrap : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                Js_of_ocaml__.Js.prop
```
```ocaml
method select : unit Js_of_ocaml__.Js.meth
```
```ocaml
method setSelectionRange : int -> int -> unit Js_of_ocaml__.Js.meth
```
```ocaml
method setSelectionRange_direction : int ->
  int ->
  Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method setRangeText : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method setRangeText_full : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  int ->
  int ->
  Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method required : bool Js_of_ocaml__.Js.t Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method placeholder : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                       Js_of_ocaml__.Js.prop
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
method onselect : ('self Js_of_ocaml__.Js.t, event Js_of_ocaml__.Js.t)
                    event_listener
                    Js_of_ocaml__.Js.prop
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
```ocaml
method onblur : ('self Js_of_ocaml__.Js.t, focusEvent Js_of_ocaml__.Js.t)
                  event_listener
                  Js_of_ocaml__.Js.prop
```
```ocaml
method onfocus : ('self Js_of_ocaml__.Js.t, focusEvent Js_of_ocaml__.Js.t)
                   event_listener
                   Js_of_ocaml__.Js.prop
```