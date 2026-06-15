
# Class type `Dom_html.selection`

Information on current selection

```ocaml
method anchorNode : Js_of_ocaml__.Dom.node Js_of_ocaml__.Js.t
                      Js_of_ocaml__.Js.opt
                      Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method anchorOffset : int Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method focusNode : Js_of_ocaml__.Dom.node Js_of_ocaml__.Js.t
                     Js_of_ocaml__.Js.opt
                     Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method focusOffset : int Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method isCollapsed : bool Js_of_ocaml__.Js.t Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method rangeCount : int Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method _type : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                 Js_of_ocaml__.Js.readonly_prop
```
`"None"`, `"Caret"` or `"Range"`.

```ocaml
method getRangeAt : int -> range Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
```ocaml
method collapse : Js_of_ocaml__.Dom.node Js_of_ocaml__.Js.t
                    Js_of_ocaml__.Js.opt ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method collapse_offset : Js_of_ocaml__.Dom.node Js_of_ocaml__.Js.t
                           Js_of_ocaml__.Js.opt ->
  int ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method setPosition : Js_of_ocaml__.Dom.node Js_of_ocaml__.Js.t
                       Js_of_ocaml__.Js.opt ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method setPosition_offset : Js_of_ocaml__.Dom.node Js_of_ocaml__.Js.t
                              Js_of_ocaml__.Js.opt ->
  int ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method setBaseAndExtent : Js_of_ocaml__.Dom.node Js_of_ocaml__.Js.t ->
  int ->
  Js_of_ocaml__.Dom.node Js_of_ocaml__.Js.t ->
  int ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method extend : Js_of_ocaml__.Dom.node Js_of_ocaml__.Js.t ->
  int ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method modify : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method collapseToStart : unit Js_of_ocaml__.Js.meth
```
```ocaml
method collapseToEnd : unit Js_of_ocaml__.Js.meth
```
```ocaml
method selectAllChildren : Js_of_ocaml__.Dom.node Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method addRange : range Js_of_ocaml__.Js.t -> unit Js_of_ocaml__.Js.meth
```
```ocaml
method removeRange : range Js_of_ocaml__.Js.t -> unit Js_of_ocaml__.Js.meth
```
```ocaml
method removeAllRanges : unit Js_of_ocaml__.Js.meth
```
```ocaml
method deleteFromDocument : unit Js_of_ocaml__.Js.meth
```
```ocaml
method containsNode : Js_of_ocaml__.Dom.node Js_of_ocaml__.Js.t ->
  bool Js_of_ocaml__.Js.t ->
  bool Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
```ocaml
method toString : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                    Js_of_ocaml__.Js.meth
```