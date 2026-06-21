
# Class type `Dom_html.window`

Specification of window objects

```ocaml
inherit eventTarget
```
```ocaml
method document : document Js_of_ocaml__.Js.t Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method applicationCache : applicationCache Js_of_ocaml__.Js.t
                            Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method name : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                Js_of_ocaml__.Js.prop
```
```ocaml
method location : location Js_of_ocaml__.Js.t Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method history : history Js_of_ocaml__.Js.t Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method undoManager : undoManager Js_of_ocaml__.Js.t
                       Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method navigator : navigator Js_of_ocaml__.Js.t Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method getSelection : selection Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
```ocaml
method close : unit Js_of_ocaml__.Js.meth
```
```ocaml
method closed : bool Js_of_ocaml__.Js.t Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method stop : unit Js_of_ocaml__.Js.meth
```
```ocaml
method focus : unit Js_of_ocaml__.Js.meth
```
```ocaml
method blur : unit Js_of_ocaml__.Js.meth
```
```ocaml
method scrollX : Js_of_ocaml__.Js.number_t Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method scrollY : Js_of_ocaml__.Js.number_t Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method pageXOffset : Js_of_ocaml__.Js.number_t Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method pageYOffset : Js_of_ocaml__.Js.number_t Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method scroll : Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method scroll_options : scrollToOptions Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method scrollTo : Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method scrollTo_options : scrollToOptions Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method scrollBy : Js_of_ocaml__.Js.number_t ->
  Js_of_ocaml__.Js.number_t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method scrollBy_options : scrollToOptions Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method sessionStorage : storage Js_of_ocaml__.Js.t Js_of_ocaml__.Js.optdef
                          Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method localStorage : storage Js_of_ocaml__.Js.t Js_of_ocaml__.Js.optdef
                        Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method top : window Js_of_ocaml__.Js.t Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method parent : window Js_of_ocaml__.Js.t Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method opener : window Js_of_ocaml__.Js.t Js_of_ocaml__.Js.opt
                  Js_of_ocaml__.Js.prop
```
```ocaml
method frameElement : element Js_of_ocaml__.Js.t Js_of_ocaml__.Js.opt
                        Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method open_ : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t Js_of_ocaml__.Js.opt ->
  window Js_of_ocaml__.Js.t Js_of_ocaml__.Js.opt Js_of_ocaml__.Js.meth
```
```ocaml
method postMessage : 'a. 'a -> unit Js_of_ocaml__.Js.meth
```
`postMessage(message)` — `targetOrigin` defaults to `"/"` (same origin).

```ocaml
method postMessage_targetOrigin : 'a. 'a ->
  Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
`postMessage(message, targetOrigin)`.

```ocaml
method postMessage_transfer : 'a 'b. 'a ->
  Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  'b Js_of_ocaml__.Js.js_array Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
`postMessage(message, targetOrigin, transfer)`.

```ocaml
method postMessage_options : 'a. 'a ->
  windowPostMessageOptions Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
`postMessage(message, options)`.

```ocaml
method alert : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method confirm : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  bool Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
```ocaml
method prompt : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t Js_of_ocaml__.Js.opt
    Js_of_ocaml__.Js.meth
```
```ocaml
method print : unit Js_of_ocaml__.Js.meth
```
```ocaml
method setInterval : (unit -> unit) Js_of_ocaml__.Js.callback ->
  Js_of_ocaml__.Js.number_t ->
  interval_id Js_of_ocaml__.Js.meth
```
```ocaml
method clearInterval : interval_id -> unit Js_of_ocaml__.Js.meth
```
```ocaml
method setTimeout : (unit -> unit) Js_of_ocaml__.Js.callback ->
  Js_of_ocaml__.Js.number_t ->
  timeout_id Js_of_ocaml__.Js.meth
```
```ocaml
method clearTimeout : timeout_id -> unit Js_of_ocaml__.Js.meth
```
```ocaml
method requestAnimationFrame : (Js_of_ocaml__.Js.number_t ->
                                 unit)
                                 Js_of_ocaml__.Js.callback ->
  animation_frame_request_id Js_of_ocaml__.Js.meth
```
```ocaml
method cancelAnimationFrame : animation_frame_request_id ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method screen : screen Js_of_ocaml__.Js.t Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method innerWidth : int Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method innerHeight : int Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method outerWidth : int Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method outerHeight : int Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method getComputedStyle : element Js_of_ocaml__.Js.t ->
  cssStyleDeclaration Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
```ocaml
method getComputedStyle_pseudoElt : element Js_of_ocaml__.Js.t ->
  Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  cssStyleDeclaration Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
```ocaml
method atob : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
```ocaml
method btoa : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
```ocaml
method onload : (window Js_of_ocaml__.Js.t, event Js_of_ocaml__.Js.t)
                  event_listener
                  Js_of_ocaml__.Js.prop
```
```ocaml
method onunload : (window Js_of_ocaml__.Js.t, event Js_of_ocaml__.Js.t)
                    event_listener
                    Js_of_ocaml__.Js.prop
```
```ocaml
method onbeforeunload : (window Js_of_ocaml__.Js.t,
                          beforeUnloadEvent Js_of_ocaml__.Js.t)
                          event_listener
                          Js_of_ocaml__.Js.prop
```
```ocaml
method onblur : (window Js_of_ocaml__.Js.t, focusEvent Js_of_ocaml__.Js.t)
                  event_listener
                  Js_of_ocaml__.Js.prop
```
```ocaml
method onfocus : (window Js_of_ocaml__.Js.t, focusEvent Js_of_ocaml__.Js.t)
                   event_listener
                   Js_of_ocaml__.Js.prop
```
```ocaml
method onresize : (window Js_of_ocaml__.Js.t, event Js_of_ocaml__.Js.t)
                    event_listener
                    Js_of_ocaml__.Js.prop
```
```ocaml
method onorientationchange : (window Js_of_ocaml__.Js.t,
                               event Js_of_ocaml__.Js.t)
                               event_listener
                               Js_of_ocaml__.Js.prop
```
```ocaml
method onpopstate : (window Js_of_ocaml__.Js.t,
                      popStateEvent Js_of_ocaml__.Js.t)
                      event_listener
                      Js_of_ocaml__.Js.prop
```
```ocaml
method onhashchange : (window Js_of_ocaml__.Js.t,
                        hashChangeEvent Js_of_ocaml__.Js.t)
                        event_listener
                        Js_of_ocaml__.Js.prop
```
```ocaml
method ononline : (window Js_of_ocaml__.Js.t, event Js_of_ocaml__.Js.t)
                    event_listener
                    Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method onoffline : (window Js_of_ocaml__.Js.t, event Js_of_ocaml__.Js.t)
                     event_listener
                     Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method _URL : _URL Js_of_ocaml__.Js.t Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method devicePixelRatio : Js_of_ocaml__.Js.number_t
                            Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method matchMedia : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  mediaQueryList Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```