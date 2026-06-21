
# Class type `Dom_html.animation`

```ocaml
method id : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t Js_of_ocaml__.Js.prop
```
```ocaml
method effect_ : animationEffect Js_of_ocaml__.Js.t Js_of_ocaml__.Js.opt
                   Js_of_ocaml__.Js.prop
```
```ocaml
method timeline : animationTimeline Js_of_ocaml__.Js.t Js_of_ocaml__.Js.opt
                    Js_of_ocaml__.Js.prop
```
```ocaml
method playState : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                     Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method replaceState : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                        Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method playbackRate : Js_of_ocaml__.Js.number_t Js_of_ocaml__.Js.prop
```
```ocaml
method currentTime : Js_of_ocaml__.Js.number_t Js_of_ocaml__.Js.opt
                       Js_of_ocaml__.Js.prop
```
```ocaml
method startTime : Js_of_ocaml__.Js.number_t Js_of_ocaml__.Js.opt
                     Js_of_ocaml__.Js.prop
```
```ocaml
method pending : bool Js_of_ocaml__.Js.t Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method play : unit Js_of_ocaml__.Js.meth
```
```ocaml
method pause : unit Js_of_ocaml__.Js.meth
```
```ocaml
method finish : unit Js_of_ocaml__.Js.meth
```
```ocaml
method cancel : unit Js_of_ocaml__.Js.meth
```
```ocaml
method reverse : unit Js_of_ocaml__.Js.meth
```
```ocaml
method persist : unit Js_of_ocaml__.Js.meth
```
```ocaml
method commitStyles : unit Js_of_ocaml__.Js.meth
```
```ocaml
method updatePlaybackRate : Js_of_ocaml__.Js.number_t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method finished : animation Js_of_ocaml__.Js.t Js_of_ocaml__.Promise.t
                    Js_of_ocaml__.Js.readonly_prop
```
Resolves when the animation reaches its end, or rejects when it is cancelled.

```ocaml
method ready : animation Js_of_ocaml__.Js.t Js_of_ocaml__.Promise.t
                 Js_of_ocaml__.Js.readonly_prop
```
Resolves when the animation is ready to play (i.e. the user agent has finished any pending changes to its state).

```ocaml
method oncancel : (animation Js_of_ocaml__.Js.t,
                    animationPlaybackEvent Js_of_ocaml__.Js.t)
                    event_listener
                    Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method onfinish : (animation Js_of_ocaml__.Js.t,
                    animationPlaybackEvent Js_of_ocaml__.Js.t)
                    event_listener
                    Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method onremove : (animation Js_of_ocaml__.Js.t, event Js_of_ocaml__.Js.t)
                    event_listener
                    Js_of_ocaml__.Js.writeonly_prop
```