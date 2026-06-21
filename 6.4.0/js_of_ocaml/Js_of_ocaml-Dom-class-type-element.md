
# Class type `Dom.element`

Specification of `Element` objects.

```ocaml
inherit node
```
```ocaml
method tagName : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                   Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method localName : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                     Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method prefix : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                  Js_of_ocaml__.Js.opt
                  Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method getAttribute : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t Js_of_ocaml__.Js.opt
    Js_of_ocaml__.Js.meth
```
```ocaml
method setAttribute : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method removeAttribute : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method hasAttribute : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  bool Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
```ocaml
method hasAttributes : bool Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
```ocaml
method toggleAttribute : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  bool Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
```ocaml
method toggleAttribute_force : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  bool Js_of_ocaml__.Js.t ->
  bool Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
```ocaml
method getAttributeNames : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                             Js_of_ocaml__.Js.js_array
                             Js_of_ocaml__.Js.t
                             Js_of_ocaml__.Js.meth
```
```ocaml
method getAttributeNS : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t Js_of_ocaml__.Js.opt
    Js_of_ocaml__.Js.meth
```
```ocaml
method setAttributeNS : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method removeAttributeNS : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method hasAttributeNS : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  bool Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
```ocaml
method getAttributeNode : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  attr Js_of_ocaml__.Js.t Js_of_ocaml__.Js.opt Js_of_ocaml__.Js.meth
```
```ocaml
method setAttributeNode : attr Js_of_ocaml__.Js.t ->
  attr Js_of_ocaml__.Js.t Js_of_ocaml__.Js.opt Js_of_ocaml__.Js.meth
```
```ocaml
method removeAttributeNode : attr Js_of_ocaml__.Js.t ->
  attr Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
```ocaml
method getAttributeNodeNS : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  attr Js_of_ocaml__.Js.t Js_of_ocaml__.Js.opt Js_of_ocaml__.Js.meth
```
```ocaml
method setAttributeNodeNS : attr Js_of_ocaml__.Js.t ->
  attr Js_of_ocaml__.Js.t Js_of_ocaml__.Js.opt Js_of_ocaml__.Js.meth
```
```ocaml
method getElementsByTagName : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  element collection Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
```ocaml
method getElementsByClassName : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  element collection Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
```ocaml
method matches : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  bool Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
```ocaml
method attributes : attr namedNodeMap Js_of_ocaml__.Js.t
                      Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method children : element collection Js_of_ocaml__.Js.t
                    Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method firstElementChild : element Js_of_ocaml__.Js.t Js_of_ocaml__.Js.opt
                             Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method lastElementChild : element Js_of_ocaml__.Js.t Js_of_ocaml__.Js.opt
                            Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method childElementCount : int Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method previousElementSibling : element Js_of_ocaml__.Js.t Js_of_ocaml__.Js.opt
                                  Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method nextElementSibling : element Js_of_ocaml__.Js.t Js_of_ocaml__.Js.opt
                              Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method insertAdjacentHTML : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method insertAdjacentText : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method insertAdjacentElement : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  element Js_of_ocaml__.Js.t ->
  element Js_of_ocaml__.Js.t Js_of_ocaml__.Js.opt Js_of_ocaml__.Js.meth
```