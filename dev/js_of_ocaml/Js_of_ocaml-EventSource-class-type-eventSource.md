
# Class type `EventSource.eventSource`

```ocaml
method url : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
               Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method withCredentials : bool Js_of_ocaml__.Js.t Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method readyState : state Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method close : unit Js_of_ocaml__.Js.meth
```
```ocaml
method onopen : ('self Js_of_ocaml__.Js.t,
                  'self Js_of_ocaml__.Dom.event Js_of_ocaml__.Js.t)
                  Js_of_ocaml__.Dom.event_listener
                  Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method onmessage : ('self Js_of_ocaml__.Js.t,
                     ('self, Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t)
                       Js_of_ocaml__.Dom_html.messageEvent
                       Js_of_ocaml__.Js.t)
                     Js_of_ocaml__.Dom.event_listener
                     Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method onerror : ('self Js_of_ocaml__.Js.t,
                   'self Js_of_ocaml__.Dom.event Js_of_ocaml__.Js.t)
                   Js_of_ocaml__.Dom.event_listener
                   Js_of_ocaml__.Js.writeonly_prop
```