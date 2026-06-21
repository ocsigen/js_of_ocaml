
# Class type `Dom.documentFragment`

Specification of `DocumentFragment` objects.

```ocaml
method nodeName : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                    Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method nodeValue : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                     Js_of_ocaml__.Js.opt
                     Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method nodeType : nodeType Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method parentNode : node Js_of_ocaml__.Js.t Js_of_ocaml__.Js.opt
                      Js_of_ocaml__.Js.prop
```
```ocaml
method parentElement : element Js_of_ocaml__.Js.t Js_of_ocaml__.Js.opt
                         Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method childNodes : node nodeList Js_of_ocaml__.Js.t Js_of_ocaml__.Js.prop
```
```ocaml
method firstChild : node Js_of_ocaml__.Js.t Js_of_ocaml__.Js.opt
                      Js_of_ocaml__.Js.prop
```
```ocaml
method lastChild : node Js_of_ocaml__.Js.t Js_of_ocaml__.Js.opt
                     Js_of_ocaml__.Js.prop
```
```ocaml
method previousSibling : node Js_of_ocaml__.Js.t Js_of_ocaml__.Js.opt
                           Js_of_ocaml__.Js.prop
```
```ocaml
method nextSibling : node Js_of_ocaml__.Js.t Js_of_ocaml__.Js.opt
                       Js_of_ocaml__.Js.prop
```
```ocaml
method namespaceURI : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                        Js_of_ocaml__.Js.opt
                        Js_of_ocaml__.Js.prop
```
```ocaml
method isConnected : bool Js_of_ocaml__.Js.t Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method insertBefore : node Js_of_ocaml__.Js.t ->
  node Js_of_ocaml__.Js.t Js_of_ocaml__.Js.opt ->
  node Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
```ocaml
method replaceChild : node Js_of_ocaml__.Js.t ->
  node Js_of_ocaml__.Js.t ->
  node Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
```ocaml
method removeChild : node Js_of_ocaml__.Js.t ->
  node Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
```ocaml
method appendChild : node Js_of_ocaml__.Js.t ->
  node Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
```ocaml
method hasChildNodes : bool Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
```ocaml
method cloneNode : bool Js_of_ocaml__.Js.t ->
  node Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
```ocaml
method compareDocumentPosition : node Js_of_ocaml__.Js.t ->
  DocumentPosition.t Js_of_ocaml__.Js.meth
```
```ocaml
method contains : node Js_of_ocaml__.Js.t ->
  bool Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
```ocaml
method getRootNode : node Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
```ocaml
method getRootNode_options : getRootNodeOptions Js_of_ocaml__.Js.t ->
  node Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
```ocaml
method isEqualNode : node Js_of_ocaml__.Js.t ->
  bool Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
```ocaml
method isSameNode : node Js_of_ocaml__.Js.t ->
  bool Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
```ocaml
method normalize : unit Js_of_ocaml__.Js.meth
```
```ocaml
method lookupNamespaceURI : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t Js_of_ocaml__.Js.opt
    Js_of_ocaml__.Js.meth
```
```ocaml
method lookupPrefix : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t Js_of_ocaml__.Js.opt
    Js_of_ocaml__.Js.meth
```