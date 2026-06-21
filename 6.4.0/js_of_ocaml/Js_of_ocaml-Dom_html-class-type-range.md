
# Class type `Dom_html.range`

Object representing a range \*

```ocaml
method collapsed : bool Js_of_ocaml__.Js.t Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method startOffset : int Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method endOffset : int Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method startContainer : Js_of_ocaml__.Dom.node Js_of_ocaml__.Js.t
                          Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method endContainer : Js_of_ocaml__.Dom.node Js_of_ocaml__.Js.t
                        Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method commonAncestorContainer : Js_of_ocaml__.Dom.node Js_of_ocaml__.Js.t
                                   Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method setStart : Js_of_ocaml__.Dom.node Js_of_ocaml__.Js.t ->
  int ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method setEnd : Js_of_ocaml__.Dom.node Js_of_ocaml__.Js.t ->
  int ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method setStartBefore : Js_of_ocaml__.Dom.node Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method setEndBefore : Js_of_ocaml__.Dom.node Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method setStartAfter : Js_of_ocaml__.Dom.node Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method setEndAfter : Js_of_ocaml__.Dom.node Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method selectNode : Js_of_ocaml__.Dom.node Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method selectNodeContents : Js_of_ocaml__.Dom.node Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method collapse : bool Js_of_ocaml__.Js.t -> unit Js_of_ocaml__.Js.meth
```
```ocaml
method compareBoundaryPoints : boundary_points_comparison ->
  range Js_of_ocaml__.Js.t ->
  int Js_of_ocaml__.Js.meth
```
```ocaml
method comparePoint : Js_of_ocaml__.Dom.node Js_of_ocaml__.Js.t ->
  int ->
  int Js_of_ocaml__.Js.meth
```
```ocaml
method isPointInRange : Js_of_ocaml__.Dom.node Js_of_ocaml__.Js.t ->
  int ->
  bool Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
```ocaml
method intersectsNode : Js_of_ocaml__.Dom.node Js_of_ocaml__.Js.t ->
  bool Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
```ocaml
method getBoundingClientRect : clientRect Js_of_ocaml__.Js.t
                                 Js_of_ocaml__.Js.meth
```
```ocaml
method getClientRects : clientRectList Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
```ocaml
method cloneContents : Js_of_ocaml__.Dom.documentFragment Js_of_ocaml__.Js.t
                         Js_of_ocaml__.Js.meth
```
```ocaml
method extractContents : Js_of_ocaml__.Dom.documentFragment Js_of_ocaml__.Js.t
                           Js_of_ocaml__.Js.meth
```
```ocaml
method deleteContents : unit Js_of_ocaml__.Js.meth
```
```ocaml
method insertNode : Js_of_ocaml__.Dom.node Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method surroundContents : Js_of_ocaml__.Dom.node Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method createContextualFragment : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  Js_of_ocaml__.Dom.documentFragment Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
```ocaml
method cloneRange : range Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
```ocaml
method detach : unit Js_of_ocaml__.Js.meth
```
```ocaml
method toString : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                    Js_of_ocaml__.Js.meth
```