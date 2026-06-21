
# Module `Dom_events.Typ`

```ocaml
type 'a typ = 'a Dom.Event.typ
```
```ocaml
val cancel : Dom_html.event Js.t typ
```
```ocaml
val click : Dom_html.mouseEvent Js.t typ
```
```ocaml
val close : Dom_html.event Js.t typ
```
```ocaml
val copy : Dom_html.clipboardEvent Js.t typ
```
```ocaml
val cut : Dom_html.clipboardEvent Js.t typ
```
```ocaml
val paste : Dom_html.clipboardEvent Js.t typ
```
```ocaml
val dblclick : Dom_html.mouseEvent Js.t typ
```
```ocaml
val mousedown : Dom_html.mouseEvent Js.t typ
```
```ocaml
val mouseup : Dom_html.mouseEvent Js.t typ
```
```ocaml
val mouseover : Dom_html.mouseEvent Js.t typ
```
```ocaml
val mousemove : Dom_html.mouseEvent Js.t typ
```
```ocaml
val mouseout : Dom_html.mouseEvent Js.t typ
```
```ocaml
val keypress : Dom_html.keyboardEvent Js.t typ
```
```ocaml
val keydown : Dom_html.keyboardEvent Js.t typ
```
```ocaml
val keyup : Dom_html.keyboardEvent Js.t typ
```
```ocaml
val mousewheel : Dom_html.mousewheelEvent Js.t typ
```
```ocaml
val wheel : Dom_html.wheelEvent Js.t typ
```
```ocaml
val touchstart : Dom_html.touchEvent Js.t typ
```
```ocaml
val touchmove : Dom_html.touchEvent Js.t typ
```
```ocaml
val touchend : Dom_html.touchEvent Js.t typ
```
```ocaml
val touchcancel : Dom_html.touchEvent Js.t typ
```
```ocaml
val dragstart : Dom_html.dragEvent Js.t typ
```
```ocaml
val dragend : Dom_html.dragEvent Js.t typ
```
```ocaml
val dragenter : Dom_html.dragEvent Js.t typ
```
```ocaml
val dragover : Dom_html.dragEvent Js.t typ
```
```ocaml
val dragleave : Dom_html.dragEvent Js.t typ
```
```ocaml
val drag : Dom_html.dragEvent Js.t typ
```
```ocaml
val drop : Dom_html.dragEvent Js.t typ
```
```ocaml
val hashchange : Dom_html.hashChangeEvent Js.t typ
```
```ocaml
val change : Dom_html.event Js.t typ
```
```ocaml
val input : Dom_html.event Js.t typ
```
```ocaml
val timeupdate : Dom_html.event Js.t typ
```
```ocaml
val submit : Dom_html.submitEvent Js.t typ
```
```ocaml
val scroll : Dom_html.event Js.t typ
```
```ocaml
val focus : Dom_html.focusEvent Js.t typ
```
```ocaml
val blur : Dom_html.focusEvent Js.t typ
```
```ocaml
val load : Dom_html.event Js.t typ
```
```ocaml
val unload : Dom_html.event Js.t typ
```
```ocaml
val beforeunload : Dom_html.beforeUnloadEvent Js.t typ
```
```ocaml
val resize : Dom_html.event Js.t typ
```
```ocaml
val orientationchange : Dom_html.event Js.t typ
```
```ocaml
val popstate : Dom_html.popStateEvent Js.t typ
```
```ocaml
val error : Dom_html.event Js.t typ
```
```ocaml
val abort : Dom_html.event Js.t typ
```
```ocaml
val select : Dom_html.event Js.t typ
```
```ocaml
val online : Dom_html.event Js.t typ
```
```ocaml
val offline : Dom_html.event Js.t typ
```
```ocaml
val checking : Dom_html.event Js.t typ
```
```ocaml
val noupdate : Dom_html.event Js.t typ
```
```ocaml
val downloading : Dom_html.event Js.t typ
```
```ocaml
val progress : Dom_html.event Js.t typ
```
```ocaml
val updateready : Dom_html.event Js.t typ
```
```ocaml
val cached : Dom_html.event Js.t typ
```
```ocaml
val obsolete : Dom_html.event Js.t typ
```
```ocaml
val domContentLoaded : Dom_html.event Js.t typ
```
```ocaml
val animationstart : Dom_html.animationEvent Js.t typ
```
```ocaml
val animationend : Dom_html.animationEvent Js.t typ
```
```ocaml
val animationiteration : Dom_html.animationEvent Js.t typ
```
```ocaml
val animationcancel : Dom_html.animationEvent Js.t typ
```
```ocaml
val transitionrun : Dom_html.transitionEvent Js.t typ
```
```ocaml
val transitionstart : Dom_html.transitionEvent Js.t typ
```
```ocaml
val transitionend : Dom_html.transitionEvent Js.t typ
```
```ocaml
val transitioncancel : Dom_html.transitionEvent Js.t typ
```
```ocaml
val canplay : Dom_html.mediaEvent Js.t typ
```
```ocaml
val canplaythrough : Dom_html.mediaEvent Js.t typ
```
```ocaml
val durationchange : Dom_html.mediaEvent Js.t typ
```
```ocaml
val emptied : Dom_html.mediaEvent Js.t typ
```
```ocaml
val ended : Dom_html.mediaEvent Js.t typ
```
```ocaml
val gotpointercapture : Dom_html.pointerEvent Js.t typ
```
```ocaml
val loadeddata : Dom_html.mediaEvent Js.t typ
```
```ocaml
val loadedmetadata : Dom_html.mediaEvent Js.t typ
```
```ocaml
val loadstart : Dom_html.mediaEvent Js.t typ
```
```ocaml
val lostpointercapture : Dom_html.pointerEvent Js.t typ
```
```ocaml
val message : Dom_html.messageEvent Js.t typ
```
```ocaml
val pause : Dom_html.mediaEvent Js.t typ
```
```ocaml
val play : Dom_html.mediaEvent Js.t typ
```
```ocaml
val playing : Dom_html.mediaEvent Js.t typ
```
```ocaml
val pointerenter : Dom_html.pointerEvent Js.t typ
```
```ocaml
val pointercancel : Dom_html.pointerEvent Js.t typ
```
```ocaml
val pointerdown : Dom_html.pointerEvent Js.t typ
```
```ocaml
val pointerleave : Dom_html.pointerEvent Js.t typ
```
```ocaml
val pointermove : Dom_html.pointerEvent Js.t typ
```
```ocaml
val pointerout : Dom_html.pointerEvent Js.t typ
```
```ocaml
val pointerover : Dom_html.pointerEvent Js.t typ
```
```ocaml
val pointerup : Dom_html.pointerEvent Js.t typ
```
```ocaml
val ratechange : Dom_html.mediaEvent Js.t typ
```
```ocaml
val seeked : Dom_html.mediaEvent Js.t typ
```
```ocaml
val seeking : Dom_html.mediaEvent Js.t typ
```
```ocaml
val stalled : Dom_html.mediaEvent Js.t typ
```
```ocaml
val suspend : Dom_html.mediaEvent Js.t typ
```
```ocaml
val volumechange : Dom_html.mediaEvent Js.t typ
```
```ocaml
val waiting : Dom_html.mediaEvent Js.t typ
```
```ocaml
val beforetoggle : Dom_html.toggleEvent Js.t typ
```
```ocaml
val toggle : Dom_html.toggleEvent Js.t typ
```
```ocaml
val compositionstart : Dom_html.compositionEvent Js.t typ
```
```ocaml
val compositionupdate : Dom_html.compositionEvent Js.t typ
```
```ocaml
val compositionend : Dom_html.compositionEvent Js.t typ
```
```ocaml
val pageshow : Dom_html.pageTransitionEvent Js.t typ
```
```ocaml
val pagehide : Dom_html.pageTransitionEvent Js.t typ
```
```ocaml
val loadend : Dom_html.progressEvent Js.t typ
```
```ocaml
val make : string -> 'a typ
```