
# Class type `File.fileReader`

```ocaml
method readAsArrayBuffer : blob Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method readAsBinaryString : blob Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method readAsText : blob Js_of_ocaml__.Js.t -> unit Js_of_ocaml__.Js.meth
```
```ocaml
method readAsText_withEncoding : blob Js_of_ocaml__.Js.t ->
  Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method readAsDataURL : blob Js_of_ocaml__.Js.t -> unit Js_of_ocaml__.Js.meth
```
```ocaml
method abort : unit Js_of_ocaml__.Js.meth
```
```ocaml
method readyState : readyState Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method result : file_any Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method error : fileError Js_of_ocaml__.Js.t Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method onloadstart : ('self Js_of_ocaml__.Js.t,
                       'self progressEvent Js_of_ocaml__.Js.t)
                       Js_of_ocaml__.Dom.event_listener
                       Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method onprogress : ('self Js_of_ocaml__.Js.t,
                      'self progressEvent Js_of_ocaml__.Js.t)
                      Js_of_ocaml__.Dom.event_listener
                      Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method onload : ('self Js_of_ocaml__.Js.t,
                  'self progressEvent Js_of_ocaml__.Js.t)
                  Js_of_ocaml__.Dom.event_listener
                  Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method onabort : ('self Js_of_ocaml__.Js.t,
                   'self progressEvent Js_of_ocaml__.Js.t)
                   Js_of_ocaml__.Dom.event_listener
                   Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method onerror : ('self Js_of_ocaml__.Js.t,
                   'self progressEvent Js_of_ocaml__.Js.t)
                   Js_of_ocaml__.Dom.event_listener
                   Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method onloadend : ('self Js_of_ocaml__.Js.t,
                     'self progressEvent Js_of_ocaml__.Js.t)
                     Js_of_ocaml__.Dom.event_listener
                     Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
inherit progressEventTarget
```