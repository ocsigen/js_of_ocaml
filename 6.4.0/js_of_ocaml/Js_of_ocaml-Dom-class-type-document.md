
# Class type `Dom.document`

Specification of `Document` objects.

```ocaml
inherit node
```
```ocaml
method documentElement : 'element Js_of_ocaml__.Js.t
                           Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method doctype : documentType Js_of_ocaml__.Js.t Js_of_ocaml__.Js.opt
                   Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _URL : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method documentURI : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                       Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method characterSet : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                        Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method contentType : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                       Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method compatMode : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                      Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method createDocumentFragment : documentFragment Js_of_ocaml__.Js.t
                                  Js_of_ocaml__.Js.meth
```
```ocaml
method createElement : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  'element Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
```ocaml
method createElementNS : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  'element Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
```ocaml
method createTextNode : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  text Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
```ocaml
method createAttribute : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  attr Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
```ocaml
method createComment : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  comment Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
```ocaml
method getElementById : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  'element Js_of_ocaml__.Js.t Js_of_ocaml__.Js.opt Js_of_ocaml__.Js.meth
```
```ocaml
method getElementsByTagName : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  'element collection Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
```ocaml
method getElementsByTagNameNS : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  'element collection Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
```ocaml
method getElementsByClassName : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  'element collection Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
```ocaml
method importNode : element Js_of_ocaml__.Js.t ->
  bool Js_of_ocaml__.Js.t ->
  'element Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
```ocaml
method adoptNode : element Js_of_ocaml__.Js.t ->
  'element Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```