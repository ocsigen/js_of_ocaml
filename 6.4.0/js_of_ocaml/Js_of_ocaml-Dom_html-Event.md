
# Module `Dom_html.Event`

Event types: `mousedown`, `keypress`, ...

```ocaml
type 'a typ = 'a Dom.Event.typ
```
```ocaml
val cancel : event Js.t typ
```
```ocaml
val click : mouseEvent Js.t typ
```
```ocaml
val close : event Js.t typ
```
```ocaml
val copy : clipboardEvent Js.t typ
```
```ocaml
val cut : clipboardEvent Js.t typ
```
```ocaml
val paste : clipboardEvent Js.t typ
```
```ocaml
val dblclick : mouseEvent Js.t typ
```
```ocaml
val mousedown : mouseEvent Js.t typ
```
```ocaml
val mouseup : mouseEvent Js.t typ
```
```ocaml
val mouseover : mouseEvent Js.t typ
```
```ocaml
val mousemove : mouseEvent Js.t typ
```
```ocaml
val mouseout : mouseEvent Js.t typ
```
```ocaml
val keypress : keyboardEvent Js.t typ
```
```ocaml
val keydown : keyboardEvent Js.t typ
```
```ocaml
val keyup : keyboardEvent Js.t typ
```
```ocaml
val mousewheel : mousewheelEvent Js.t typ
```
```ocaml
val wheel : wheelEvent Js.t typ
```
```ocaml
val touchstart : touchEvent Js.t typ
```
```ocaml
val touchmove : touchEvent Js.t typ
```
```ocaml
val touchend : touchEvent Js.t typ
```
```ocaml
val touchcancel : touchEvent Js.t typ
```
```ocaml
val dragstart : dragEvent Js.t typ
```
```ocaml
val dragend : dragEvent Js.t typ
```
```ocaml
val dragenter : dragEvent Js.t typ
```
```ocaml
val dragover : dragEvent Js.t typ
```
```ocaml
val dragleave : dragEvent Js.t typ
```
```ocaml
val drag : dragEvent Js.t typ
```
```ocaml
val drop : dragEvent Js.t typ
```
```ocaml
val hashchange : hashChangeEvent Js.t typ
```
```ocaml
val change : event Js.t typ
```
```ocaml
val input : event Js.t typ
```
```ocaml
val timeupdate : event Js.t typ
```
```ocaml
val submit : submitEvent Js.t typ
```
```ocaml
val scroll : event Js.t typ
```
```ocaml
val focus : focusEvent Js.t typ
```
```ocaml
val blur : focusEvent Js.t typ
```
```ocaml
val load : event Js.t typ
```
```ocaml
val unload : event Js.t typ
```
```ocaml
val beforeunload : beforeUnloadEvent Js.t typ
```
```ocaml
val resize : event Js.t typ
```
```ocaml
val orientationchange : event Js.t typ
```
```ocaml
val popstate : popStateEvent Js.t typ
```
```ocaml
val error : event Js.t typ
```
```ocaml
val abort : event Js.t typ
```
```ocaml
val select : event Js.t typ
```
```ocaml
val online : event Js.t typ
```
```ocaml
val offline : event Js.t typ
```
```ocaml
val checking : event Js.t typ
```
```ocaml
val noupdate : event Js.t typ
```
```ocaml
val downloading : event Js.t typ
```
```ocaml
val progress : event Js.t typ
```
```ocaml
val updateready : event Js.t typ
```
```ocaml
val cached : event Js.t typ
```
```ocaml
val obsolete : event Js.t typ
```
```ocaml
val domContentLoaded : event Js.t typ
```
```ocaml
val animationstart : animationEvent Js.t typ
```
```ocaml
val animationend : animationEvent Js.t typ
```
```ocaml
val animationiteration : animationEvent Js.t typ
```
```ocaml
val animationcancel : animationEvent Js.t typ
```
```ocaml
val transitionrun : transitionEvent Js.t typ
```
```ocaml
val transitionstart : transitionEvent Js.t typ
```
```ocaml
val transitionend : transitionEvent Js.t typ
```
```ocaml
val transitioncancel : transitionEvent Js.t typ
```
```ocaml
val canplay : mediaEvent Js.t typ
```
```ocaml
val canplaythrough : mediaEvent Js.t typ
```
```ocaml
val durationchange : mediaEvent Js.t typ
```
```ocaml
val emptied : mediaEvent Js.t typ
```
```ocaml
val ended : mediaEvent Js.t typ
```
```ocaml
val gotpointercapture : pointerEvent Js.t typ
```
```ocaml
val loadeddata : mediaEvent Js.t typ
```
```ocaml
val loadedmetadata : mediaEvent Js.t typ
```
```ocaml
val loadstart : mediaEvent Js.t typ
```
```ocaml
val lostpointercapture : pointerEvent Js.t typ
```
```ocaml
val message : messageEvent Js.t typ
```
```ocaml
val pause : mediaEvent Js.t typ
```
```ocaml
val play : mediaEvent Js.t typ
```
```ocaml
val playing : mediaEvent Js.t typ
```
```ocaml
val pointerenter : pointerEvent Js.t typ
```
```ocaml
val pointercancel : pointerEvent Js.t typ
```
```ocaml
val pointerdown : pointerEvent Js.t typ
```
```ocaml
val pointerleave : pointerEvent Js.t typ
```
```ocaml
val pointermove : pointerEvent Js.t typ
```
```ocaml
val pointerout : pointerEvent Js.t typ
```
```ocaml
val pointerover : pointerEvent Js.t typ
```
```ocaml
val pointerup : pointerEvent Js.t typ
```
```ocaml
val ratechange : mediaEvent Js.t typ
```
```ocaml
val seeked : mediaEvent Js.t typ
```
```ocaml
val seeking : mediaEvent Js.t typ
```
```ocaml
val stalled : mediaEvent Js.t typ
```
```ocaml
val suspend : mediaEvent Js.t typ
```
```ocaml
val volumechange : mediaEvent Js.t typ
```
```ocaml
val waiting : mediaEvent Js.t typ
```
```ocaml
val beforetoggle : toggleEvent Js.t typ
```
```ocaml
val toggle : toggleEvent Js.t typ
```
```ocaml
val compositionstart : compositionEvent Js.t typ
```
```ocaml
val compositionupdate : compositionEvent Js.t typ
```
```ocaml
val compositionend : compositionEvent Js.t typ
```
```ocaml
val pageshow : pageTransitionEvent Js.t typ
```
```ocaml
val pagehide : pageTransitionEvent Js.t typ
```
```ocaml
val loadend : progressEvent Js.t typ
```
```ocaml
val make : string -> 'a typ
```