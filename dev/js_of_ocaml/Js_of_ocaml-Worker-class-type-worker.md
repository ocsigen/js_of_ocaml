
# Class type `Worker.worker`

```ocaml
inherit Js_of_ocaml__.Dom_html.eventTarget
```
```ocaml
method onerror : ('self Js_of_ocaml__.Js.t, errorEvent Js_of_ocaml__.Js.t)
                   Js_of_ocaml__.Dom_html.event_listener
                   Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method onmessage : ('self Js_of_ocaml__.Js.t,
                     'b messageEvent Js_of_ocaml__.Js.t)
                     Js_of_ocaml__.Dom_html.event_listener
                     Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method postMessage : 'a -> unit Js_of_ocaml__.Js.meth
```
```ocaml
method terminate : unit Js_of_ocaml__.Js.meth
```