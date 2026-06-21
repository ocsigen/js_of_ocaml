
# Module `Js_of_ocaml_lwt.Lwt_js_events`

Programming mouse or keyboard events handlers using Lwt

Reminder: Event capturing starts with the outer most element in the DOM and works inwards to the HTML element the event took place on (capture phase) and then out again (bubbling phase).

Examples of use:

Waiting for a click on `elt1` before continuing:

```ocaml
let%lwt _ = Lwt_js_events.click elt1 in
```
Doing some operation for each value change in input element `inp`:

```ocaml
Lwt_js_events.(async (fun () ->
   clicks inp1 (fun ev _ -> ...)
))
```
Defining a thread that waits for ESC key on an element:

```ocaml
let rec esc elt =
   let%lwt ev = keydown elt in
   if ev##.keyCode = 27
   then Lwt.return ev
   else esc elt
```
Waiting for a click or escape key before continuing:

```ocaml
let%lwt () =
    Lwt.pick [(let%lwt _ = esc Dom_html.document in Lwt.return ());
              (let%lwt _ = click Dom_html.document in Lwt.return ())]
  in ...
```

### Create Lwt threads for events

```ocaml
val make_event : 
  (Js_of_ocaml.Dom_html.event as 'a) Js_of_ocaml.Js.t
    Js_of_ocaml.Dom_html.Event.typ ->
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  'a Js_of_ocaml.Js.t Lwt.t
```
`make_event ev target` creates a Lwt thread that waits for the event `ev` to happen on `target` (once). This thread isa cancellable using `Lwt.cancel`. If you set the optional parameter `~use_capture:true`, the event will be caught during the capture phase, otherwise it is caught during the bubbling phase (default). If you set the optional parameter `~passive:true`, the user agent will ignore `preventDefault` calls inside the event callback.

```ocaml
val seq_loop : 
  (?use_capture:bool -> ?passive:bool -> 'target -> 'event Lwt.t) ->
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  'target ->
  ('event -> unit Lwt.t -> unit Lwt.t) ->
  unit Lwt.t
```
`seq_loop (make_event ev) target handler` creates a looping Lwt thread that waits for the event `ev` to happen on `target`, then execute handler, and start again waiting for the event. Events happening during the execution of the handler are ignored. See `async_loop` and `buffered_loop` for alternative semantics.

For example, the `clicks` function below is defined by:

`let clicks ?use_capture ?passive t = seq_loop click ?use_capture ?passive t`

The thread returned is cancellable using `Lwt.cancel`. In order for the loop thread to be canceled from within the handler, the latter receives the former as its second parameter.

By default, cancelling the loop will not cancel the potential currently running handler. This behaviour can be changed by setting the `cancel_handler` parameter to true.

```ocaml
val async_loop : 
  (?use_capture:bool -> ?passive:bool -> 'target -> 'event Lwt.t) ->
  ?use_capture:bool ->
  ?passive:bool ->
  'target ->
  ('event -> unit Lwt.t -> unit Lwt.t) ->
  unit Lwt.t
```
`async_loop` is similar to `seq_loop`, but each handler runs independently. No event is thus missed, but since several instances of the handler can be run concurrently, it is up to the programmer to ensure that they interact correctly.

Cancelling the loop will not cancel the potential currently running handlers.

```ocaml
val buffered_loop : 
  (?use_capture:bool -> ?passive:bool -> 'target -> 'event Lwt.t) ->
  ?cancel_handler:bool ->
  ?cancel_queue:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  'target ->
  ('event -> unit Lwt.t -> unit Lwt.t) ->
  unit Lwt.t
```
`buffered_loop` is similar to `seq_loop`, but any event that occurs during an execution of the handler is queued instead of being ignored.

No event is thus missed, but there can be a non predictable delay between its trigger and its treatment. It is thus a good idea to use this loop with handlers whose running time is short, so the memorized event still makes sense when the handler is eventually executed. It is also up to the programmer to ensure that event handlers terminate so the queue will eventually be emptied.

By default, cancelling the loop will not cancel the (potential) currently running handler, but any other queued event will be dropped. This behaviour can be customized using the two optional parameters `cancel_handler` and `cancel_queue`.

```ocaml
val async : (unit -> unit Lwt.t) -> unit
```
`async t` records a thread to be executed later. It is implemented by calling yield, then Lwt.async. This is useful if you want to create a new event listener when you are inside an event handler. This avoids the current event to be caught by the new event handler (if it propagates).

```ocaml
val func_limited_loop : 
  (?use_capture:bool -> ?passive:bool -> 'a -> 'b Lwt.t) ->
  (unit -> 'a Lwt.t) ->
  ?use_capture:bool ->
  ?passive:bool ->
  'a ->
  ('b -> unit Lwt.t -> unit Lwt.t) ->
  unit Lwt.t
```
`func_limited_loop event delay_fun target handler` will behave like `Lwt_js_events.async_loop event target handler` but it will run `delay_fun` first, and execute `handler` only when `delay_fun` is finished and no other event occurred in the meantime.

This allows to limit the number of events caught.

Be careful, it is an asynchrone loop, so if you give too little time, several instances of your handler could be run in same time \*

```ocaml
val limited_loop : 
  (?use_capture:bool -> ?passive:bool -> 'a -> 'b Lwt.t) ->
  ?elapsed_time:float ->
  ?use_capture:bool ->
  ?passive:bool ->
  'a ->
  ('b -> unit Lwt.t -> unit Lwt.t) ->
  unit Lwt.t
```
Same as func\_limited\_loop but take time instead of function By default elapsed\_time \= 0\.1s \= 100ms \*


### Predefined functions for some types of events

```ocaml
val click : 
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  Js_of_ocaml.Dom_html.mouseEvent Js_of_ocaml.Js.t Lwt.t
```
```ocaml
val copy : 
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  Js_of_ocaml.Dom_html.clipboardEvent Js_of_ocaml.Js.t Lwt.t
```
```ocaml
val cut : 
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  Js_of_ocaml.Dom_html.clipboardEvent Js_of_ocaml.Js.t Lwt.t
```
```ocaml
val paste : 
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  Js_of_ocaml.Dom_html.clipboardEvent Js_of_ocaml.Js.t Lwt.t
```
```ocaml
val dblclick : 
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  Js_of_ocaml.Dom_html.mouseEvent Js_of_ocaml.Js.t Lwt.t
```
```ocaml
val mousedown : 
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  Js_of_ocaml.Dom_html.mouseEvent Js_of_ocaml.Js.t Lwt.t
```
```ocaml
val mouseup : 
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  Js_of_ocaml.Dom_html.mouseEvent Js_of_ocaml.Js.t Lwt.t
```
```ocaml
val mouseover : 
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  Js_of_ocaml.Dom_html.mouseEvent Js_of_ocaml.Js.t Lwt.t
```
```ocaml
val mousemove : 
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  Js_of_ocaml.Dom_html.mouseEvent Js_of_ocaml.Js.t Lwt.t
```
```ocaml
val mouseout : 
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  Js_of_ocaml.Dom_html.mouseEvent Js_of_ocaml.Js.t Lwt.t
```
```ocaml
val keypress : 
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  Js_of_ocaml.Dom_html.keyboardEvent Js_of_ocaml.Js.t Lwt.t
```
```ocaml
val keydown : 
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  Js_of_ocaml.Dom_html.keyboardEvent Js_of_ocaml.Js.t Lwt.t
```
```ocaml
val keyup : 
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  Js_of_ocaml.Dom_html.keyboardEvent Js_of_ocaml.Js.t Lwt.t
```
```ocaml
val input : 
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  Js_of_ocaml.Dom_html.event Js_of_ocaml.Js.t Lwt.t
```
```ocaml
val timeupdate : 
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  Js_of_ocaml.Dom_html.event Js_of_ocaml.Js.t Lwt.t
```
```ocaml
val change : 
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  Js_of_ocaml.Dom_html.event Js_of_ocaml.Js.t Lwt.t
```
```ocaml
val dragstart : 
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  Js_of_ocaml.Dom_html.dragEvent Js_of_ocaml.Js.t Lwt.t
```
```ocaml
val dragend : 
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  Js_of_ocaml.Dom_html.dragEvent Js_of_ocaml.Js.t Lwt.t
```
```ocaml
val dragenter : 
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  Js_of_ocaml.Dom_html.dragEvent Js_of_ocaml.Js.t Lwt.t
```
```ocaml
val dragover : 
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  Js_of_ocaml.Dom_html.dragEvent Js_of_ocaml.Js.t Lwt.t
```
```ocaml
val dragleave : 
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  Js_of_ocaml.Dom_html.dragEvent Js_of_ocaml.Js.t Lwt.t
```
```ocaml
val drag : 
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  Js_of_ocaml.Dom_html.dragEvent Js_of_ocaml.Js.t Lwt.t
```
```ocaml
val drop : 
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  Js_of_ocaml.Dom_html.dragEvent Js_of_ocaml.Js.t Lwt.t
```
```ocaml
val focus : 
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  Js_of_ocaml.Dom_html.focusEvent Js_of_ocaml.Js.t Lwt.t
```
```ocaml
val blur : 
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  Js_of_ocaml.Dom_html.focusEvent Js_of_ocaml.Js.t Lwt.t
```
```ocaml
val scroll : 
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  Js_of_ocaml.Dom_html.event Js_of_ocaml.Js.t Lwt.t
```
```ocaml
val submit : 
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  Js_of_ocaml.Dom_html.submitEvent Js_of_ocaml.Js.t Lwt.t
```
```ocaml
val select : 
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  Js_of_ocaml.Dom_html.event Js_of_ocaml.Js.t Lwt.t
```
```ocaml
val mousewheel : 
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  (Js_of_ocaml.Dom_html.mouseEvent Js_of_ocaml.Js.t * (int * int)) Lwt.t
```
This function returns the event, together with the numbers of ticks the mouse wheel moved. Positive means down or right. This interface is compatible with all (recent) browsers.

```ocaml
val wheel : 
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  Js_of_ocaml.Dom_html.mousewheelEvent Js_of_ocaml.Js.t Lwt.t
```
```ocaml
val touchstart : 
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  Js_of_ocaml.Dom_html.touchEvent Js_of_ocaml.Js.t Lwt.t
```
```ocaml
val touchmove : 
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  Js_of_ocaml.Dom_html.touchEvent Js_of_ocaml.Js.t Lwt.t
```
```ocaml
val touchend : 
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  Js_of_ocaml.Dom_html.touchEvent Js_of_ocaml.Js.t Lwt.t
```
```ocaml
val touchcancel : 
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  Js_of_ocaml.Dom_html.touchEvent Js_of_ocaml.Js.t Lwt.t
```
```ocaml
val lostpointercapture : 
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  Js_of_ocaml.Dom_html.pointerEvent Js_of_ocaml.Js.t Lwt.t
```
```ocaml
val gotpointercapture : 
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  Js_of_ocaml.Dom_html.pointerEvent Js_of_ocaml.Js.t Lwt.t
```
```ocaml
val pointerenter : 
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  Js_of_ocaml.Dom_html.pointerEvent Js_of_ocaml.Js.t Lwt.t
```
```ocaml
val pointercancel : 
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  Js_of_ocaml.Dom_html.pointerEvent Js_of_ocaml.Js.t Lwt.t
```
```ocaml
val pointerdown : 
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  Js_of_ocaml.Dom_html.pointerEvent Js_of_ocaml.Js.t Lwt.t
```
```ocaml
val pointerleave : 
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  Js_of_ocaml.Dom_html.pointerEvent Js_of_ocaml.Js.t Lwt.t
```
```ocaml
val pointermove : 
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  Js_of_ocaml.Dom_html.pointerEvent Js_of_ocaml.Js.t Lwt.t
```
```ocaml
val pointerout : 
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  Js_of_ocaml.Dom_html.pointerEvent Js_of_ocaml.Js.t Lwt.t
```
```ocaml
val pointerover : 
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  Js_of_ocaml.Dom_html.pointerEvent Js_of_ocaml.Js.t Lwt.t
```
```ocaml
val pointerup : 
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  Js_of_ocaml.Dom_html.pointerEvent Js_of_ocaml.Js.t Lwt.t
```
```ocaml
val transitionend : 
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  Js_of_ocaml.Dom_html.transitionEvent Js_of_ocaml.Js.t Lwt.t
```
Returns when a CSS transition terminates on the element.

```ocaml
val transitionstart : 
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  Js_of_ocaml.Dom_html.transitionEvent Js_of_ocaml.Js.t Lwt.t
```
```ocaml
val transitionrun : 
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  Js_of_ocaml.Dom_html.transitionEvent Js_of_ocaml.Js.t Lwt.t
```
```ocaml
val transitioncancel : 
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  Js_of_ocaml.Dom_html.transitionEvent Js_of_ocaml.Js.t Lwt.t
```
```ocaml
val load : 
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.imageElement Js_of_ocaml.Js.t ->
  Js_of_ocaml.Dom_html.event Js_of_ocaml.Js.t Lwt.t
```
```ocaml
val error : 
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.imageElement Js_of_ocaml.Js.t ->
  Js_of_ocaml.Dom_html.event Js_of_ocaml.Js.t Lwt.t
```
```ocaml
val abort : 
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.imageElement Js_of_ocaml.Js.t ->
  Js_of_ocaml.Dom_html.event Js_of_ocaml.Js.t Lwt.t
```
```ocaml
val canplay : 
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  Js_of_ocaml.Dom_html.event Js_of_ocaml.Js.t Lwt.t
```
```ocaml
val canplaythrough : 
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  Js_of_ocaml.Dom_html.event Js_of_ocaml.Js.t Lwt.t
```
```ocaml
val durationchange : 
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  Js_of_ocaml.Dom_html.event Js_of_ocaml.Js.t Lwt.t
```
```ocaml
val emptied : 
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  Js_of_ocaml.Dom_html.event Js_of_ocaml.Js.t Lwt.t
```
```ocaml
val ended : 
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  Js_of_ocaml.Dom_html.event Js_of_ocaml.Js.t Lwt.t
```
```ocaml
val loadeddata : 
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  Js_of_ocaml.Dom_html.event Js_of_ocaml.Js.t Lwt.t
```
```ocaml
val loadedmetadata : 
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  Js_of_ocaml.Dom_html.event Js_of_ocaml.Js.t Lwt.t
```
```ocaml
val loadstart : 
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  Js_of_ocaml.Dom_html.event Js_of_ocaml.Js.t Lwt.t
```
```ocaml
val pause : 
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  Js_of_ocaml.Dom_html.event Js_of_ocaml.Js.t Lwt.t
```
```ocaml
val play : 
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  Js_of_ocaml.Dom_html.event Js_of_ocaml.Js.t Lwt.t
```
```ocaml
val playing : 
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  Js_of_ocaml.Dom_html.event Js_of_ocaml.Js.t Lwt.t
```
```ocaml
val ratechange : 
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  Js_of_ocaml.Dom_html.event Js_of_ocaml.Js.t Lwt.t
```
```ocaml
val seeked : 
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  Js_of_ocaml.Dom_html.event Js_of_ocaml.Js.t Lwt.t
```
```ocaml
val seeking : 
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  Js_of_ocaml.Dom_html.event Js_of_ocaml.Js.t Lwt.t
```
```ocaml
val stalled : 
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  Js_of_ocaml.Dom_html.event Js_of_ocaml.Js.t Lwt.t
```
```ocaml
val suspend : 
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  Js_of_ocaml.Dom_html.event Js_of_ocaml.Js.t Lwt.t
```
```ocaml
val volumechange : 
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  Js_of_ocaml.Dom_html.event Js_of_ocaml.Js.t Lwt.t
```
```ocaml
val waiting : 
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  Js_of_ocaml.Dom_html.event Js_of_ocaml.Js.t Lwt.t
```
```ocaml
val clicks : 
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  (Js_of_ocaml.Dom_html.mouseEvent Js_of_ocaml.Js.t ->
    unit Lwt.t ->
    unit Lwt.t) ->
  unit Lwt.t
```
```ocaml
val copies : 
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  (Js_of_ocaml.Dom_html.clipboardEvent Js_of_ocaml.Js.t ->
    unit Lwt.t ->
    unit Lwt.t) ->
  unit Lwt.t
```
```ocaml
val cuts : 
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  (Js_of_ocaml.Dom_html.clipboardEvent Js_of_ocaml.Js.t ->
    unit Lwt.t ->
    unit Lwt.t) ->
  unit Lwt.t
```
```ocaml
val pastes : 
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  (Js_of_ocaml.Dom_html.clipboardEvent Js_of_ocaml.Js.t ->
    unit Lwt.t ->
    unit Lwt.t) ->
  unit Lwt.t
```
```ocaml
val dblclicks : 
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  (Js_of_ocaml.Dom_html.mouseEvent Js_of_ocaml.Js.t ->
    unit Lwt.t ->
    unit Lwt.t) ->
  unit Lwt.t
```
```ocaml
val mousedowns : 
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  (Js_of_ocaml.Dom_html.mouseEvent Js_of_ocaml.Js.t ->
    unit Lwt.t ->
    unit Lwt.t) ->
  unit Lwt.t
```
```ocaml
val mouseups : 
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  (Js_of_ocaml.Dom_html.mouseEvent Js_of_ocaml.Js.t ->
    unit Lwt.t ->
    unit Lwt.t) ->
  unit Lwt.t
```
```ocaml
val mouseovers : 
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  (Js_of_ocaml.Dom_html.mouseEvent Js_of_ocaml.Js.t ->
    unit Lwt.t ->
    unit Lwt.t) ->
  unit Lwt.t
```
```ocaml
val mousemoves : 
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  (Js_of_ocaml.Dom_html.mouseEvent Js_of_ocaml.Js.t ->
    unit Lwt.t ->
    unit Lwt.t) ->
  unit Lwt.t
```
```ocaml
val mouseouts : 
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  (Js_of_ocaml.Dom_html.mouseEvent Js_of_ocaml.Js.t ->
    unit Lwt.t ->
    unit Lwt.t) ->
  unit Lwt.t
```
```ocaml
val keypresses : 
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  (Js_of_ocaml.Dom_html.keyboardEvent Js_of_ocaml.Js.t ->
    unit Lwt.t ->
    unit Lwt.t) ->
  unit Lwt.t
```
```ocaml
val keydowns : 
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  (Js_of_ocaml.Dom_html.keyboardEvent Js_of_ocaml.Js.t ->
    unit Lwt.t ->
    unit Lwt.t) ->
  unit Lwt.t
```
```ocaml
val keyups : 
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  (Js_of_ocaml.Dom_html.keyboardEvent Js_of_ocaml.Js.t ->
    unit Lwt.t ->
    unit Lwt.t) ->
  unit Lwt.t
```
```ocaml
val inputs : 
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  (Js_of_ocaml.Dom_html.event Js_of_ocaml.Js.t -> unit Lwt.t -> unit Lwt.t) ->
  unit Lwt.t
```
```ocaml
val timeupdates : 
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  (Js_of_ocaml.Dom_html.event Js_of_ocaml.Js.t -> unit Lwt.t -> unit Lwt.t) ->
  unit Lwt.t
```
```ocaml
val changes : 
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  (Js_of_ocaml.Dom_html.event Js_of_ocaml.Js.t -> unit Lwt.t -> unit Lwt.t) ->
  unit Lwt.t
```
```ocaml
val dragstarts : 
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  (Js_of_ocaml.Dom_html.dragEvent Js_of_ocaml.Js.t -> unit Lwt.t -> unit Lwt.t) ->
  unit Lwt.t
```
```ocaml
val dragends : 
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  (Js_of_ocaml.Dom_html.dragEvent Js_of_ocaml.Js.t -> unit Lwt.t -> unit Lwt.t) ->
  unit Lwt.t
```
```ocaml
val dragenters : 
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  (Js_of_ocaml.Dom_html.dragEvent Js_of_ocaml.Js.t -> unit Lwt.t -> unit Lwt.t) ->
  unit Lwt.t
```
```ocaml
val dragovers : 
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  (Js_of_ocaml.Dom_html.dragEvent Js_of_ocaml.Js.t -> unit Lwt.t -> unit Lwt.t) ->
  unit Lwt.t
```
```ocaml
val dragleaves : 
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  (Js_of_ocaml.Dom_html.dragEvent Js_of_ocaml.Js.t -> unit Lwt.t -> unit Lwt.t) ->
  unit Lwt.t
```
```ocaml
val drags : 
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  (Js_of_ocaml.Dom_html.dragEvent Js_of_ocaml.Js.t -> unit Lwt.t -> unit Lwt.t) ->
  unit Lwt.t
```
```ocaml
val drops : 
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  (Js_of_ocaml.Dom_html.dragEvent Js_of_ocaml.Js.t -> unit Lwt.t -> unit Lwt.t) ->
  unit Lwt.t
```
```ocaml
val mousewheels : 
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  ((Js_of_ocaml.Dom_html.mouseEvent Js_of_ocaml.Js.t * (int * int)) ->
    unit Lwt.t ->
    unit Lwt.t) ->
  unit Lwt.t
```
```ocaml
val wheels : 
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  (Js_of_ocaml.Dom_html.mousewheelEvent Js_of_ocaml.Js.t ->
    unit Lwt.t ->
    unit Lwt.t) ->
  unit Lwt.t
```
```ocaml
val touchstarts : 
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  (Js_of_ocaml.Dom_html.touchEvent Js_of_ocaml.Js.t ->
    unit Lwt.t ->
    unit Lwt.t) ->
  unit Lwt.t
```
```ocaml
val touchmoves : 
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  (Js_of_ocaml.Dom_html.touchEvent Js_of_ocaml.Js.t ->
    unit Lwt.t ->
    unit Lwt.t) ->
  unit Lwt.t
```
```ocaml
val touchends : 
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  (Js_of_ocaml.Dom_html.touchEvent Js_of_ocaml.Js.t ->
    unit Lwt.t ->
    unit Lwt.t) ->
  unit Lwt.t
```
```ocaml
val touchcancels : 
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  (Js_of_ocaml.Dom_html.touchEvent Js_of_ocaml.Js.t ->
    unit Lwt.t ->
    unit Lwt.t) ->
  unit Lwt.t
```
```ocaml
val focuses : 
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  (Js_of_ocaml.Dom_html.focusEvent Js_of_ocaml.Js.t ->
    unit Lwt.t ->
    unit Lwt.t) ->
  unit Lwt.t
```
```ocaml
val blurs : 
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  (Js_of_ocaml.Dom_html.focusEvent Js_of_ocaml.Js.t ->
    unit Lwt.t ->
    unit Lwt.t) ->
  unit Lwt.t
```
```ocaml
val scrolls : 
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  (Js_of_ocaml.Dom_html.event Js_of_ocaml.Js.t -> unit Lwt.t -> unit Lwt.t) ->
  unit Lwt.t
```
```ocaml
val submits : 
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  (Js_of_ocaml.Dom_html.submitEvent Js_of_ocaml.Js.t ->
    unit Lwt.t ->
    unit Lwt.t) ->
  unit Lwt.t
```
```ocaml
val selects : 
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  (Js_of_ocaml.Dom_html.event Js_of_ocaml.Js.t -> unit Lwt.t -> unit Lwt.t) ->
  unit Lwt.t
```
```ocaml
val loads : 
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.imageElement Js_of_ocaml.Js.t ->
  (Js_of_ocaml.Dom_html.event Js_of_ocaml.Js.t -> unit Lwt.t -> unit Lwt.t) ->
  unit Lwt.t
```
```ocaml
val errors : 
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.imageElement Js_of_ocaml.Js.t ->
  (Js_of_ocaml.Dom_html.event Js_of_ocaml.Js.t -> unit Lwt.t -> unit Lwt.t) ->
  unit Lwt.t
```
```ocaml
val aborts : 
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.imageElement Js_of_ocaml.Js.t ->
  (Js_of_ocaml.Dom_html.event Js_of_ocaml.Js.t -> unit Lwt.t -> unit Lwt.t) ->
  unit Lwt.t
```
```ocaml
val canplays : 
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  (Js_of_ocaml.Dom_html.event Js_of_ocaml.Js.t -> unit Lwt.t -> unit Lwt.t) ->
  unit Lwt.t
```
```ocaml
val canplaythroughs : 
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  (Js_of_ocaml.Dom_html.event Js_of_ocaml.Js.t -> unit Lwt.t -> unit Lwt.t) ->
  unit Lwt.t
```
```ocaml
val durationchanges : 
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  (Js_of_ocaml.Dom_html.event Js_of_ocaml.Js.t -> unit Lwt.t -> unit Lwt.t) ->
  unit Lwt.t
```
```ocaml
val emptieds : 
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  (Js_of_ocaml.Dom_html.event Js_of_ocaml.Js.t -> unit Lwt.t -> unit Lwt.t) ->
  unit Lwt.t
```
```ocaml
val endeds : 
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  (Js_of_ocaml.Dom_html.event Js_of_ocaml.Js.t -> unit Lwt.t -> unit Lwt.t) ->
  unit Lwt.t
```
```ocaml
val loadeddatas : 
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  (Js_of_ocaml.Dom_html.event Js_of_ocaml.Js.t -> unit Lwt.t -> unit Lwt.t) ->
  unit Lwt.t
```
```ocaml
val loadedmetadatas : 
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  (Js_of_ocaml.Dom_html.event Js_of_ocaml.Js.t -> unit Lwt.t -> unit Lwt.t) ->
  unit Lwt.t
```
```ocaml
val loadstarts : 
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  (Js_of_ocaml.Dom_html.event Js_of_ocaml.Js.t -> unit Lwt.t -> unit Lwt.t) ->
  unit Lwt.t
```
```ocaml
val pauses : 
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  (Js_of_ocaml.Dom_html.event Js_of_ocaml.Js.t -> unit Lwt.t -> unit Lwt.t) ->
  unit Lwt.t
```
```ocaml
val plays : 
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  (Js_of_ocaml.Dom_html.event Js_of_ocaml.Js.t -> unit Lwt.t -> unit Lwt.t) ->
  unit Lwt.t
```
```ocaml
val playings : 
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  (Js_of_ocaml.Dom_html.event Js_of_ocaml.Js.t -> unit Lwt.t -> unit Lwt.t) ->
  unit Lwt.t
```
```ocaml
val ratechanges : 
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  (Js_of_ocaml.Dom_html.event Js_of_ocaml.Js.t -> unit Lwt.t -> unit Lwt.t) ->
  unit Lwt.t
```
```ocaml
val seekeds : 
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  (Js_of_ocaml.Dom_html.event Js_of_ocaml.Js.t -> unit Lwt.t -> unit Lwt.t) ->
  unit Lwt.t
```
```ocaml
val seekings : 
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  (Js_of_ocaml.Dom_html.event Js_of_ocaml.Js.t -> unit Lwt.t -> unit Lwt.t) ->
  unit Lwt.t
```
```ocaml
val stalleds : 
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  (Js_of_ocaml.Dom_html.event Js_of_ocaml.Js.t -> unit Lwt.t -> unit Lwt.t) ->
  unit Lwt.t
```
```ocaml
val suspends : 
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  (Js_of_ocaml.Dom_html.event Js_of_ocaml.Js.t -> unit Lwt.t -> unit Lwt.t) ->
  unit Lwt.t
```
```ocaml
val volumechanges : 
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  (Js_of_ocaml.Dom_html.event Js_of_ocaml.Js.t -> unit Lwt.t -> unit Lwt.t) ->
  unit Lwt.t
```
```ocaml
val waitings : 
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  (Js_of_ocaml.Dom_html.event Js_of_ocaml.Js.t -> unit Lwt.t -> unit Lwt.t) ->
  unit Lwt.t
```
```ocaml
val lostpointercaptures : 
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  (Js_of_ocaml.Dom_html.pointerEvent Js_of_ocaml.Js.t ->
    unit Lwt.t ->
    unit Lwt.t) ->
  unit Lwt.t
```
```ocaml
val gotpointercaptures : 
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  (Js_of_ocaml.Dom_html.pointerEvent Js_of_ocaml.Js.t ->
    unit Lwt.t ->
    unit Lwt.t) ->
  unit Lwt.t
```
```ocaml
val pointerenters : 
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  (Js_of_ocaml.Dom_html.pointerEvent Js_of_ocaml.Js.t ->
    unit Lwt.t ->
    unit Lwt.t) ->
  unit Lwt.t
```
```ocaml
val pointercancels : 
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  (Js_of_ocaml.Dom_html.pointerEvent Js_of_ocaml.Js.t ->
    unit Lwt.t ->
    unit Lwt.t) ->
  unit Lwt.t
```
```ocaml
val pointerdowns : 
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  (Js_of_ocaml.Dom_html.pointerEvent Js_of_ocaml.Js.t ->
    unit Lwt.t ->
    unit Lwt.t) ->
  unit Lwt.t
```
```ocaml
val pointerleaves : 
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  (Js_of_ocaml.Dom_html.pointerEvent Js_of_ocaml.Js.t ->
    unit Lwt.t ->
    unit Lwt.t) ->
  unit Lwt.t
```
```ocaml
val pointermoves : 
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  (Js_of_ocaml.Dom_html.pointerEvent Js_of_ocaml.Js.t ->
    unit Lwt.t ->
    unit Lwt.t) ->
  unit Lwt.t
```
```ocaml
val pointerouts : 
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  (Js_of_ocaml.Dom_html.pointerEvent Js_of_ocaml.Js.t ->
    unit Lwt.t ->
    unit Lwt.t) ->
  unit Lwt.t
```
```ocaml
val pointerovers : 
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  (Js_of_ocaml.Dom_html.pointerEvent Js_of_ocaml.Js.t ->
    unit Lwt.t ->
    unit Lwt.t) ->
  unit Lwt.t
```
```ocaml
val pointerups : 
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  (Js_of_ocaml.Dom_html.pointerEvent Js_of_ocaml.Js.t ->
    unit Lwt.t ->
    unit Lwt.t) ->
  unit Lwt.t
```
```ocaml
val transitionends : 
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  (Js_of_ocaml.Dom_html.transitionEvent Js_of_ocaml.Js.t ->
    unit Lwt.t ->
    unit Lwt.t) ->
  unit Lwt.t
```
```ocaml
val transitionstarts : 
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  (Js_of_ocaml.Dom_html.transitionEvent Js_of_ocaml.Js.t ->
    unit Lwt.t ->
    unit Lwt.t) ->
  unit Lwt.t
```
```ocaml
val transitionruns : 
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  (Js_of_ocaml.Dom_html.transitionEvent Js_of_ocaml.Js.t ->
    unit Lwt.t ->
    unit Lwt.t) ->
  unit Lwt.t
```
```ocaml
val transitioncancels : 
  ?cancel_handler:bool ->
  ?use_capture:bool ->
  ?passive:bool ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t ->
  (Js_of_ocaml.Dom_html.transitionEvent Js_of_ocaml.Js.t ->
    unit Lwt.t ->
    unit Lwt.t) ->
  unit Lwt.t
```
```ocaml
val request_animation_frame : unit -> unit Lwt.t
```
Returns when a repaint of the window by the browser starts. (see JS method `window.requestAnimationFrame`)

```ocaml
val onload : unit -> Js_of_ocaml.Dom_html.event Js_of_ocaml.Js.t Lwt.t
```
Returns when the page is loaded

```ocaml
val domContentLoaded : unit -> unit Lwt.t
```
```ocaml
val onunload : unit -> Js_of_ocaml.Dom_html.event Js_of_ocaml.Js.t Lwt.t
```
```ocaml
val onbeforeunload : 
  unit ->
  Js_of_ocaml.Dom_html.beforeUnloadEvent Js_of_ocaml.Js.t Lwt.t
```
```ocaml
val onresize : unit -> Js_of_ocaml.Dom_html.event Js_of_ocaml.Js.t Lwt.t
```
```ocaml
val onorientationchange : 
  unit ->
  Js_of_ocaml.Dom_html.event Js_of_ocaml.Js.t Lwt.t
```
```ocaml
val onpopstate : 
  unit ->
  Js_of_ocaml.Dom_html.popStateEvent Js_of_ocaml.Js.t Lwt.t
```
```ocaml
val onhashchange : 
  unit ->
  Js_of_ocaml.Dom_html.hashChangeEvent Js_of_ocaml.Js.t Lwt.t
```
```ocaml
val onorientationchange_or_onresize : 
  unit ->
  Js_of_ocaml.Dom_html.event Js_of_ocaml.Js.t Lwt.t
```
```ocaml
val onresizes : 
  (Js_of_ocaml.Dom_html.event Js_of_ocaml.Js.t -> unit Lwt.t -> unit Lwt.t) ->
  unit Lwt.t
```
```ocaml
val onorientationchanges : 
  (Js_of_ocaml.Dom_html.event Js_of_ocaml.Js.t -> unit Lwt.t -> unit Lwt.t) ->
  unit Lwt.t
```
```ocaml
val onpopstates : 
  (Js_of_ocaml.Dom_html.popStateEvent Js_of_ocaml.Js.t ->
    unit Lwt.t ->
    unit Lwt.t) ->
  unit Lwt.t
```
```ocaml
val onhashchanges : 
  (Js_of_ocaml.Dom_html.hashChangeEvent Js_of_ocaml.Js.t ->
    unit Lwt.t ->
    unit Lwt.t) ->
  unit Lwt.t
```
```ocaml
val onorientationchanges_or_onresizes : 
  (Js_of_ocaml.Dom_html.event Js_of_ocaml.Js.t -> unit Lwt.t -> unit Lwt.t) ->
  unit Lwt.t
```
```ocaml
val limited_onresizes : 
  ?elapsed_time:float ->
  (Js_of_ocaml.Dom_html.event Js_of_ocaml.Js.t -> unit Lwt.t -> unit Lwt.t) ->
  unit Lwt.t
```
```ocaml
val limited_onorientationchanges : 
  ?elapsed_time:float ->
  (Js_of_ocaml.Dom_html.event Js_of_ocaml.Js.t -> unit Lwt.t -> unit Lwt.t) ->
  unit Lwt.t
```
```ocaml
val limited_onorientationchanges_or_onresizes : 
  ?elapsed_time:float ->
  (Js_of_ocaml.Dom_html.event Js_of_ocaml.Js.t -> unit Lwt.t -> unit Lwt.t) ->
  unit Lwt.t
```