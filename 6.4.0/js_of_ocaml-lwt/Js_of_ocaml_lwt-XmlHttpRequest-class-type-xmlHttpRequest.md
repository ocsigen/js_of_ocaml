
# Class type `XmlHttpRequest.xmlHttpRequest`

```ocaml
method onreadystatechange : (unit -> unit) Js_of_ocaml__.Js.callback
                              Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method readyState : readyState Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _open : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  bool Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method _open_full : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  bool Js_of_ocaml__.Js.t ->
  Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t Js_of_ocaml__.Js.opt ->
  Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t Js_of_ocaml__.Js.opt ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method setRequestHeader : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method overrideMimeType : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method send : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                Js_of_ocaml__.Js.opt ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method send_blob : Js_of_ocaml__.File.blob Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method send_document : Js_of_ocaml__.Dom.element Js_of_ocaml__.Dom.document
                         Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method send_formData : Js_of_ocaml__.Form.formData Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method abort : unit Js_of_ocaml__.Js.meth
```
```ocaml
method status : int Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method statusText : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                      Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method getResponseHeader : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t Js_of_ocaml__.Js.opt
    Js_of_ocaml__.Js.meth
```
```ocaml
method getAllResponseHeaders : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                                 Js_of_ocaml__.Js.meth
```
```ocaml
method response : Js_of_ocaml__.File.file_any Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method responseText : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                        Js_of_ocaml__.Js.opt
                        Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method responseXML : Js_of_ocaml__.Dom.element Js_of_ocaml__.Dom.document
                       Js_of_ocaml__.Js.t
                       Js_of_ocaml__.Js.opt
                       Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method responseType : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                        Js_of_ocaml__.Js.prop
```
```ocaml
method withCredentials : bool Js_of_ocaml__.Js.t
                           Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
inherit Js_of_ocaml__.File.progressEventTarget
```
```ocaml
method ontimeout : ('self Js_of_ocaml__.Js.t,
                     'self Js_of_ocaml__.File.progressEvent Js_of_ocaml__.Js.t)
                     Js_of_ocaml__.Dom.event_listener
                     Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method upload : xmlHttpRequestUpload Js_of_ocaml__.Js.t
                  Js_of_ocaml__.Js.readonly_prop
```