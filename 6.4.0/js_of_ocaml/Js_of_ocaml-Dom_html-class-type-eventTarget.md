
# Class type `Dom_html.eventTarget`

Common properties of event target objects: `onclick`, `onkeypress`, ...

```ocaml
method onclick : ('self Js_of_ocaml__.Js.t, mouseEvent Js_of_ocaml__.Js.t)
                   event_listener
                   Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method ondblclick : ('self Js_of_ocaml__.Js.t, mouseEvent Js_of_ocaml__.Js.t)
                      event_listener
                      Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method onmousedown : ('self Js_of_ocaml__.Js.t, mouseEvent Js_of_ocaml__.Js.t)
                       event_listener
                       Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method onmouseup : ('self Js_of_ocaml__.Js.t, mouseEvent Js_of_ocaml__.Js.t)
                     event_listener
                     Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method onmouseover : ('self Js_of_ocaml__.Js.t, mouseEvent Js_of_ocaml__.Js.t)
                       event_listener
                       Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method onmousemove : ('self Js_of_ocaml__.Js.t, mouseEvent Js_of_ocaml__.Js.t)
                       event_listener
                       Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method onmouseout : ('self Js_of_ocaml__.Js.t, mouseEvent Js_of_ocaml__.Js.t)
                      event_listener
                      Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method onkeypress : ('self Js_of_ocaml__.Js.t,
                      keyboardEvent Js_of_ocaml__.Js.t)
                      event_listener
                      Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method onkeydown : ('self Js_of_ocaml__.Js.t, keyboardEvent Js_of_ocaml__.Js.t)
                     event_listener
                     Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method onkeyup : ('self Js_of_ocaml__.Js.t, keyboardEvent Js_of_ocaml__.Js.t)
                   event_listener
                   Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method onscroll : ('self Js_of_ocaml__.Js.t, event Js_of_ocaml__.Js.t)
                    event_listener
                    Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method onwheel : ('self Js_of_ocaml__.Js.t, wheelEvent Js_of_ocaml__.Js.t)
                   event_listener
                   Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method ondragstart : ('self Js_of_ocaml__.Js.t, dragEvent Js_of_ocaml__.Js.t)
                       event_listener
                       Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method ondragend : ('self Js_of_ocaml__.Js.t, dragEvent Js_of_ocaml__.Js.t)
                     event_listener
                     Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method ondragenter : ('self Js_of_ocaml__.Js.t, dragEvent Js_of_ocaml__.Js.t)
                       event_listener
                       Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method ondragover : ('self Js_of_ocaml__.Js.t, dragEvent Js_of_ocaml__.Js.t)
                      event_listener
                      Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method ondragleave : ('self Js_of_ocaml__.Js.t, dragEvent Js_of_ocaml__.Js.t)
                       event_listener
                       Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method ondrag : ('self Js_of_ocaml__.Js.t, dragEvent Js_of_ocaml__.Js.t)
                  event_listener
                  Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method ondrop : ('self Js_of_ocaml__.Js.t, dragEvent Js_of_ocaml__.Js.t)
                  event_listener
                  Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method onanimationstart : ('self Js_of_ocaml__.Js.t,
                            animationEvent Js_of_ocaml__.Js.t)
                            event_listener
                            Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method onanimationend : ('self Js_of_ocaml__.Js.t,
                          animationEvent Js_of_ocaml__.Js.t)
                          event_listener
                          Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method onanimationiteration : ('self Js_of_ocaml__.Js.t,
                                animationEvent Js_of_ocaml__.Js.t)
                                event_listener
                                Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method onanimationcancel : ('self Js_of_ocaml__.Js.t,
                             animationEvent Js_of_ocaml__.Js.t)
                             event_listener
                             Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method ontransitionrun : ('self Js_of_ocaml__.Js.t,
                           transitionEvent Js_of_ocaml__.Js.t)
                           event_listener
                           Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method ontransitionstart : ('self Js_of_ocaml__.Js.t,
                             transitionEvent Js_of_ocaml__.Js.t)
                             event_listener
                             Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method ontransitionend : ('self Js_of_ocaml__.Js.t,
                           transitionEvent Js_of_ocaml__.Js.t)
                           event_listener
                           Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method ontransitioncancel : ('self Js_of_ocaml__.Js.t,
                              transitionEvent Js_of_ocaml__.Js.t)
                              event_listener
                              Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method ongotpointercapture : ('self Js_of_ocaml__.Js.t,
                               pointerEvent Js_of_ocaml__.Js.t)
                               event_listener
                               Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method onlostpointercapture : ('self Js_of_ocaml__.Js.t,
                                pointerEvent Js_of_ocaml__.Js.t)
                                event_listener
                                Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method onpointerenter : ('self Js_of_ocaml__.Js.t,
                          pointerEvent Js_of_ocaml__.Js.t)
                          event_listener
                          Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method onpointercancel : ('self Js_of_ocaml__.Js.t,
                           pointerEvent Js_of_ocaml__.Js.t)
                           event_listener
                           Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method onpointerdown : ('self Js_of_ocaml__.Js.t,
                         pointerEvent Js_of_ocaml__.Js.t)
                         event_listener
                         Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method onpointerleave : ('self Js_of_ocaml__.Js.t,
                          pointerEvent Js_of_ocaml__.Js.t)
                          event_listener
                          Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method onpointermove : ('self Js_of_ocaml__.Js.t,
                         pointerEvent Js_of_ocaml__.Js.t)
                         event_listener
                         Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method onpointerout : ('self Js_of_ocaml__.Js.t,
                        pointerEvent Js_of_ocaml__.Js.t)
                        event_listener
                        Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method onpointerover : ('self Js_of_ocaml__.Js.t,
                         pointerEvent Js_of_ocaml__.Js.t)
                         event_listener
                         Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method onpointerup : ('self Js_of_ocaml__.Js.t,
                       pointerEvent Js_of_ocaml__.Js.t)
                       event_listener
                       Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method onbeforetoggle : ('self Js_of_ocaml__.Js.t,
                          toggleEvent Js_of_ocaml__.Js.t)
                          event_listener
                          Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method ontoggle : ('self Js_of_ocaml__.Js.t, toggleEvent Js_of_ocaml__.Js.t)
                    event_listener
                    Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method dispatchEvent : event Js_of_ocaml__.Js.t ->
  bool Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```