
# Class type `WebSockets.webSocket`

```ocaml
inherit Js_of_ocaml__.Dom_html.eventTarget
```
```ocaml
method url : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
               Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method readyState : readyState Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method bufferedAmount : int Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method onopen : ('self Js_of_ocaml__.Js.t,
                  'self Js_of_ocaml__.Dom.event Js_of_ocaml__.Js.t)
                  Js_of_ocaml__.Dom.event_listener
                  Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method onclose : ('self Js_of_ocaml__.Js.t,
                   'self closeEvent Js_of_ocaml__.Js.t)
                   Js_of_ocaml__.Dom.event_listener
                   Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method onerror : ('self Js_of_ocaml__.Js.t,
                   'self Js_of_ocaml__.Dom.event Js_of_ocaml__.Js.t)
                   Js_of_ocaml__.Dom.event_listener
                   Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method extensions : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                      Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method protocol : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                    Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method close : unit Js_of_ocaml__.Js.meth
```
```ocaml
method close_withCode : int -> unit Js_of_ocaml__.Js.meth
```
```ocaml
method close_withCodeAndReason : int ->
  Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method onmessage : ('self Js_of_ocaml__.Js.t,
                     'self messageEvent Js_of_ocaml__.Js.t)
                     Js_of_ocaml__.Dom.event_listener
                     Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method binaryType : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                      Js_of_ocaml__.Js.prop
```
```ocaml
method send : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method send_buffer : Js_of_ocaml__.Typed_array.arrayBuffer Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method send_blob : Js_of_ocaml__.File.blob Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```