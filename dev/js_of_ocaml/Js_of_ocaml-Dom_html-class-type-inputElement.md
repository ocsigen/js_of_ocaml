
# Class type `Dom_html.inputElement`

```ocaml
inherit submitterElement
```
```ocaml
method defaultValue : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                        Js_of_ocaml__.Js.prop
```
```ocaml
method defaultChecked : bool Js_of_ocaml__.Js.t Js_of_ocaml__.Js.prop
```
```ocaml
method form : formElement Js_of_ocaml__.Js.t Js_of_ocaml__.Js.opt
                Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method accept : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                  Js_of_ocaml__.Js.prop
```
```ocaml
method accessKey : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                     Js_of_ocaml__.Js.prop
```
```ocaml
method align : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                 Js_of_ocaml__.Js.prop
```
deprecated Use CSS instead.
```ocaml
method alt : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
               Js_of_ocaml__.Js.prop
```
```ocaml
method autocomplete : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                        Js_of_ocaml__.Js.prop
```
```ocaml
method capture : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                   Js_of_ocaml__.Js.prop
```
```ocaml
method checked : bool Js_of_ocaml__.Js.t Js_of_ocaml__.Js.prop
```
```ocaml
method dirName : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                   Js_of_ocaml__.Js.prop
```
```ocaml
method disabled : bool Js_of_ocaml__.Js.t Js_of_ocaml__.Js.prop
```
```ocaml
method formAction : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                      Js_of_ocaml__.Js.prop
```
```ocaml
method formEnctype : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                       Js_of_ocaml__.Js.prop
```
```ocaml
method formMethod : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                      Js_of_ocaml__.Js.prop
```
```ocaml
method formNoValidate : bool Js_of_ocaml__.Js.t Js_of_ocaml__.Js.prop
```
```ocaml
method formTarget : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                      Js_of_ocaml__.Js.prop
```
```ocaml
method indeterminate : bool Js_of_ocaml__.Js.t Js_of_ocaml__.Js.prop
```
```ocaml
method list : dataListElement Js_of_ocaml__.Js.t Js_of_ocaml__.Js.opt
                Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method max : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
               Js_of_ocaml__.Js.prop
```
```ocaml
method maxLength : int Js_of_ocaml__.Js.prop
```
```ocaml
method min : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
               Js_of_ocaml__.Js.prop
```
```ocaml
method minLength : int Js_of_ocaml__.Js.prop
```
```ocaml
method multiple : bool Js_of_ocaml__.Js.t Js_of_ocaml__.Js.prop
```
```ocaml
method name : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method pattern : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                   Js_of_ocaml__.Js.prop
```
```ocaml
method readOnly : bool Js_of_ocaml__.Js.t Js_of_ocaml__.Js.prop
```
```ocaml
method required : bool Js_of_ocaml__.Js.t Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method size : int Js_of_ocaml__.Js.prop
```
```ocaml
method src : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
               Js_of_ocaml__.Js.prop
```
```ocaml
method step : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                Js_of_ocaml__.Js.prop
```
```ocaml
method tabIndex : int Js_of_ocaml__.Js.prop
```
```ocaml
method _type : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                 Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method useMap : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                  Js_of_ocaml__.Js.prop
```
```ocaml
method value : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                 Js_of_ocaml__.Js.prop
```
```ocaml
method valueAsDate : Js_of_ocaml__.Js.date Js_of_ocaml__.Js.t
                       Js_of_ocaml__.Js.opt
                       Js_of_ocaml__.Js.prop
```
```ocaml
method valueAsNumber : Js_of_ocaml__.Js.number_t Js_of_ocaml__.Js.prop
```
```ocaml
method stepUp : unit Js_of_ocaml__.Js.meth
```
```ocaml
method stepUp_n : int -> unit Js_of_ocaml__.Js.meth
```
```ocaml
method stepDown : unit Js_of_ocaml__.Js.meth
```
```ocaml
method stepDown_n : int -> unit Js_of_ocaml__.Js.meth
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
method files : Js_of_ocaml__.File.fileList Js_of_ocaml__.Js.t
                 Js_of_ocaml__.Js.opt
                 Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method placeholder : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                       Js_of_ocaml__.Js.prop
```
```ocaml
method selectionDirection : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                              Js_of_ocaml__.Js.prop
```
```ocaml
method selectionStart : int Js_of_ocaml__.Js.prop
```
```ocaml
method selectionEnd : int Js_of_ocaml__.Js.prop
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
method popoverTargetElement : element Js_of_ocaml__.Js.t Js_of_ocaml__.Js.opt
                                Js_of_ocaml__.Js.prop
```
```ocaml
method popoverTargetAction : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                               Js_of_ocaml__.Js.prop
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