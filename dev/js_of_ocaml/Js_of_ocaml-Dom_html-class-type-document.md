
# Class type `Dom_html.document`

```ocaml
inherit element Js_of_ocaml__.Dom.document
```
```ocaml
inherit nodeSelector
```
```ocaml
inherit eventTarget
```
```ocaml
method title : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                 Js_of_ocaml__.Js.prop
```
```ocaml
method referrer : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                    Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method domain : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                  Js_of_ocaml__.Js.prop
```
```ocaml
method _URL : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method lastModified : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                        Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method head : headElement Js_of_ocaml__.Js.t Js_of_ocaml__.Js.prop
```
```ocaml
method body : bodyElement Js_of_ocaml__.Js.t Js_of_ocaml__.Js.prop
```
```ocaml
method documentElement : htmlElement Js_of_ocaml__.Js.t
                           Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method currentScript : element Js_of_ocaml__.Js.t Js_of_ocaml__.Js.opt
                         Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method scrollingElement : element Js_of_ocaml__.Js.t Js_of_ocaml__.Js.opt
                            Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method images : imageElement collection Js_of_ocaml__.Js.t
                  Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method fonts : Js_of_ocaml__.FontFace.fontFaceSet Js_of_ocaml__.Js.t
                 Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method applets : element collection Js_of_ocaml__.Js.t
                   Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method links : element collection Js_of_ocaml__.Js.t
                 Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method forms : formElement collection Js_of_ocaml__.Js.t
                 Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method anchors : element collection Js_of_ocaml__.Js.t
                   Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method embeds : embedElement collection Js_of_ocaml__.Js.t
                  Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method plugins : element collection Js_of_ocaml__.Js.t
                   Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method scripts : scriptElement collection Js_of_ocaml__.Js.t
                   Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method cookie : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                  Js_of_ocaml__.Js.prop
```
```ocaml
method designMode : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                      Js_of_ocaml__.Js.prop
```
```ocaml
method open_ : unit Js_of_ocaml__.Js.meth
```
```ocaml
method close : unit Js_of_ocaml__.Js.meth
```
```ocaml
method write : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method execCommand : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  bool Js_of_ocaml__.Js.t ->
  Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t Js_of_ocaml__.Js.opt ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method createRange : range Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
```ocaml
method readyState : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                      Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method getElementsByClassName : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  element Js_of_ocaml__.Dom.collection Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
```ocaml
method getElementsByName : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  element Js_of_ocaml__.Dom.nodeList Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
```ocaml
method activeElement : element Js_of_ocaml__.Js.t Js_of_ocaml__.Js.opt
                         Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method hidden : bool Js_of_ocaml__.Js.t Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method visibilityState : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                           Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method fullscreenElement : element Js_of_ocaml__.Js.t Js_of_ocaml__.Js.opt
                             Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method fullscreenEnabled : bool Js_of_ocaml__.Js.t
                             Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method pointerLockElement : element Js_of_ocaml__.Js.t Js_of_ocaml__.Js.opt
                              Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method hasFocus : bool Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
```ocaml
method elementFromPoint : Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  element Js_of_ocaml__.Js.t Js_of_ocaml__.Js.opt Js_of_ocaml__.Js.meth
```
```ocaml
method elementsFromPoint : Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  element Js_of_ocaml__.Js.t Js_of_ocaml__.Js.js_array Js_of_ocaml__.Js.t
    Js_of_ocaml__.Js.meth
```
```ocaml
method getSelection : selection Js_of_ocaml__.Js.t Js_of_ocaml__.Js.opt
                        Js_of_ocaml__.Js.meth
```
```ocaml
method getAnimations : animation Js_of_ocaml__.Js.t Js_of_ocaml__.Js.js_array
                         Js_of_ocaml__.Js.t
                         Js_of_ocaml__.Js.meth
```
```ocaml
method timeline : documentTimeline Js_of_ocaml__.Js.t
                    Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method exitFullscreen : unit Js_of_ocaml__.Promise.t Js_of_ocaml__.Js.meth
```
```ocaml
method exitPointerLock : unit Js_of_ocaml__.Js.meth
```
```ocaml
method onreadystatechange : (document Js_of_ocaml__.Js.t,
                              event Js_of_ocaml__.Js.t)
                              event_listener
                              Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method onvisibilitychange : (document Js_of_ocaml__.Js.t,
                              event Js_of_ocaml__.Js.t)
                              event_listener
                              Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method onfullscreenchange : (document Js_of_ocaml__.Js.t,
                              event Js_of_ocaml__.Js.t)
                              event_listener
                              Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method onfullscreenerror : (document Js_of_ocaml__.Js.t,
                             event Js_of_ocaml__.Js.t)
                             event_listener
                             Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method onpointerlockchange : (document Js_of_ocaml__.Js.t,
                               event Js_of_ocaml__.Js.t)
                               event_listener
                               Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method onpointerlockerror : (document Js_of_ocaml__.Js.t,
                              event Js_of_ocaml__.Js.t)
                              event_listener
                              Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method onwebkitfullscreenchange : (document Js_of_ocaml__.Js.t,
                                    event Js_of_ocaml__.Js.t)
                                    event_listener
                                    Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
inherit eventTarget
```