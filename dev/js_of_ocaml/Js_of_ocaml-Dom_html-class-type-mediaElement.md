
# Class type `Dom_html.mediaElement`

```ocaml
inherit element
```
```ocaml
method canPlayType : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
```ocaml
method load : unit Js_of_ocaml__.Js.meth
```
```ocaml
method play : unit Js_of_ocaml__.Promise.t Js_of_ocaml__.Js.meth
```
```ocaml
method pause : unit Js_of_ocaml__.Js.meth
```
```ocaml
method autoplay : bool Js_of_ocaml__.Js.t Js_of_ocaml__.Js.prop
```
```ocaml
method buffered : timeRanges Js_of_ocaml__.Js.t Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method controls : bool Js_of_ocaml__.Js.t Js_of_ocaml__.Js.prop
```
```ocaml
method currentSrc : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                      Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method currentTime : Js_of_ocaml__.Js.number_t Js_of_ocaml__.Js.prop
```
```ocaml
method duration : Js_of_ocaml__.Js.number_t Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method ended : bool Js_of_ocaml__.Js.t Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method loop : bool Js_of_ocaml__.Js.t Js_of_ocaml__.Js.prop
```
```ocaml
method mediagroup : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                      Js_of_ocaml__.Js.prop
```
```ocaml
method muted : bool Js_of_ocaml__.Js.t Js_of_ocaml__.Js.prop
```
```ocaml
method networkState_int : int Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method networkState : networkState Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method paused : bool Js_of_ocaml__.Js.t Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method playbackRate : Js_of_ocaml__.Js.number_t Js_of_ocaml__.Js.prop
```
```ocaml
method played : timeRanges Js_of_ocaml__.Js.t Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method preload : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                   Js_of_ocaml__.Js.prop
```
```ocaml
method readyState_int : int Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method readyState : readyState Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method seekable : timeRanges Js_of_ocaml__.Js.t Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method seeking : bool Js_of_ocaml__.Js.t Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method src : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
               Js_of_ocaml__.Js.prop
```
```ocaml
method volume : Js_of_ocaml__.Js.number_t Js_of_ocaml__.Js.prop
```
```ocaml
method oncanplay : ('self Js_of_ocaml__.Js.t, mediaEvent Js_of_ocaml__.Js.t)
                     event_listener
                     Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method oncanplaythrough : ('self Js_of_ocaml__.Js.t,
                            mediaEvent Js_of_ocaml__.Js.t)
                            event_listener
                            Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method ondurationchange : ('self Js_of_ocaml__.Js.t,
                            mediaEvent Js_of_ocaml__.Js.t)
                            event_listener
                            Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method onemptied : ('self Js_of_ocaml__.Js.t, mediaEvent Js_of_ocaml__.Js.t)
                     event_listener
                     Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method onended : ('self Js_of_ocaml__.Js.t, mediaEvent Js_of_ocaml__.Js.t)
                   event_listener
                   Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method onloadeddata : ('self Js_of_ocaml__.Js.t, mediaEvent Js_of_ocaml__.Js.t)
                        event_listener
                        Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method onloadedmetadata : ('self Js_of_ocaml__.Js.t,
                            mediaEvent Js_of_ocaml__.Js.t)
                            event_listener
                            Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method onloadstart : ('self Js_of_ocaml__.Js.t, mediaEvent Js_of_ocaml__.Js.t)
                       event_listener
                       Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method onpause : ('self Js_of_ocaml__.Js.t, mediaEvent Js_of_ocaml__.Js.t)
                   event_listener
                   Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method onplay : ('self Js_of_ocaml__.Js.t, mediaEvent Js_of_ocaml__.Js.t)
                  event_listener
                  Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method onplaying : ('self Js_of_ocaml__.Js.t, mediaEvent Js_of_ocaml__.Js.t)
                     event_listener
                     Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method onratechange : ('self Js_of_ocaml__.Js.t, mediaEvent Js_of_ocaml__.Js.t)
                        event_listener
                        Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method onseeked : ('self Js_of_ocaml__.Js.t, mediaEvent Js_of_ocaml__.Js.t)
                    event_listener
                    Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method onseeking : ('self Js_of_ocaml__.Js.t, mediaEvent Js_of_ocaml__.Js.t)
                     event_listener
                     Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method onstalled : ('self Js_of_ocaml__.Js.t, mediaEvent Js_of_ocaml__.Js.t)
                     event_listener
                     Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method onsuspend : ('self Js_of_ocaml__.Js.t, mediaEvent Js_of_ocaml__.Js.t)
                     event_listener
                     Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method onvolumechange : ('self Js_of_ocaml__.Js.t,
                          mediaEvent Js_of_ocaml__.Js.t)
                          event_listener
                          Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method onwaiting : ('self Js_of_ocaml__.Js.t, mediaEvent Js_of_ocaml__.Js.t)
                     event_listener
                     Js_of_ocaml__.Js.writeonly_prop
```