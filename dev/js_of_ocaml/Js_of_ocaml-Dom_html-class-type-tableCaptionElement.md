
# Class type `Dom_html.tableCaptionElement`

```ocaml
inherit Js_of_ocaml__.Dom.element
```
```ocaml
inherit nodeSelector
```
```ocaml
method id : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t Js_of_ocaml__.Js.prop
```
```ocaml
method title : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                 Js_of_ocaml__.Js.prop
```
```ocaml
method lang : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                Js_of_ocaml__.Js.prop
```
```ocaml
method dir : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
               Js_of_ocaml__.Js.prop
```
```ocaml
method className : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                     Js_of_ocaml__.Js.prop
```
```ocaml
method classList : tokenList Js_of_ocaml__.Js.t Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method closest : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  element Js_of_ocaml__.Js.t Js_of_ocaml__.Js.opt Js_of_ocaml__.Js.meth
```
```ocaml
method slot : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                Js_of_ocaml__.Js.prop
```
```ocaml
method shadowRoot : shadowRoot Js_of_ocaml__.Js.t Js_of_ocaml__.Js.opt
                      Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method assignedSlot : element Js_of_ocaml__.Js.t Js_of_ocaml__.Js.opt
                        Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method attachShadow : shadowRootInit Js_of_ocaml__.Js.t ->
  shadowRoot Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
```ocaml
method style : cssStyleDeclaration Js_of_ocaml__.Js.t Js_of_ocaml__.Js.prop
```
```ocaml
method innerHTML : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                     Js_of_ocaml__.Js.prop
```
```ocaml
method outerHTML : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                     Js_of_ocaml__.Js.prop
```
```ocaml
method textContent : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                       Js_of_ocaml__.Js.opt
                       Js_of_ocaml__.Js.prop
```
```ocaml
method innerText : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                     Js_of_ocaml__.Js.prop
```
```ocaml
method dataset : domStringMap Js_of_ocaml__.Js.t Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method tabIndex : int Js_of_ocaml__.Js.prop
```
```ocaml
method hidden : bool Js_of_ocaml__.Js.t Js_of_ocaml__.Js.writeonly_prop
```
Setter only: reading `hidden` in some browsers can return the string `"until-found"` rather than a boolean, so the getter is not exposed.

```ocaml
method draggable : bool Js_of_ocaml__.Js.t Js_of_ocaml__.Js.prop
```
```ocaml
method spellcheck : bool Js_of_ocaml__.Js.t Js_of_ocaml__.Js.prop
```
```ocaml
method translate : bool Js_of_ocaml__.Js.t Js_of_ocaml__.Js.prop
```
```ocaml
method contentEditable : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                           Js_of_ocaml__.Js.prop
```
```ocaml
method isContentEditable : bool Js_of_ocaml__.Js.t
                             Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method accessKey : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                     Js_of_ocaml__.Js.prop
```
```ocaml
method autofocus : bool Js_of_ocaml__.Js.t Js_of_ocaml__.Js.prop
```
```ocaml
method inputMode : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                     Js_of_ocaml__.Js.prop
```
```ocaml
method enterKeyHint : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                        Js_of_ocaml__.Js.prop
```
```ocaml
method nonce : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                 Js_of_ocaml__.Js.prop
```
```ocaml
method clientLeft : int Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method clientTop : int Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method clientWidth : int Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method clientHeight : int Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method offsetLeft : int Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method offsetTop : int Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method offsetParent : element Js_of_ocaml__.Js.t Js_of_ocaml__.Js.opt
                        Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method offsetWidth : int Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method offsetHeight : int Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method scrollLeft : Js_of_ocaml__.Js.number_t Js_of_ocaml__.Js.prop
```
```ocaml
method scrollTop : Js_of_ocaml__.Js.number_t Js_of_ocaml__.Js.prop
```
```ocaml
method scrollWidth : int Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method scrollHeight : int Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method popover : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                   Js_of_ocaml__.Js.opt
                   Js_of_ocaml__.Js.prop
```
```ocaml
method getClientRects : clientRectList Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
```ocaml
method getBoundingClientRect : clientRect Js_of_ocaml__.Js.t
                                 Js_of_ocaml__.Js.meth
```
```ocaml
method scrollIntoView : bool Js_of_ocaml__.Js.t -> unit Js_of_ocaml__.Js.meth
```
```ocaml
method scrollIntoView_options : scrollIntoViewOptions Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method scrollTo_options : scrollToOptions Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method scrollBy_options : scrollToOptions Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method click : unit Js_of_ocaml__.Js.meth
```
```ocaml
method focus : unit Js_of_ocaml__.Js.meth
```
```ocaml
method focus_options : focusOptions Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method blur : unit Js_of_ocaml__.Js.meth
```
```ocaml
method requestFullscreen : unit Js_of_ocaml__.Promise.t Js_of_ocaml__.Js.meth
```
```ocaml
method requestPointerLock : unit Js_of_ocaml__.Promise.t Js_of_ocaml__.Js.meth
```
```ocaml
method animate : 'a 'b. 'a ->
  'b ->
  animation Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
```ocaml
method animate_keyframes : 'a. 'a ->
  animation Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
`animate(keyframes)` — no options/duration.

```ocaml
method animate_duration : 'a. 'a ->
  Js_of_ocaml__.Js.number_t ->
  animation Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
`animate(keyframes, duration)` — duration in milliseconds.

```ocaml
method animate_options : 'a. 'a ->
  keyframeAnimationOptions Js_of_ocaml__.Js.t ->
  animation Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
`animate(keyframes, options)` with a typed options dictionary.

```ocaml
method getAnimations : animation Js_of_ocaml__.Js.t Js_of_ocaml__.Js.js_array
                         Js_of_ocaml__.Js.t
                         Js_of_ocaml__.Js.meth
```
```ocaml
method hidePopover : unit Js_of_ocaml__.Js.meth
```
```ocaml
method showPopover : unit Js_of_ocaml__.Js.meth
```
```ocaml
method showPopover_options : showPopover_options Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method togglePopover : bool Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
```ocaml
method togglePopover_force : bool Js_of_ocaml__.Js.t ->
  bool Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
`togglePopover(force)` — `force=true` forces the popover open, `false` forces it closed; the no-arg form toggles. Returns the new open state.

```ocaml
method togglePopover_options : togglePopover_options Js_of_ocaml__.Js.t ->
  bool Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
```ocaml
inherit eventTarget
```