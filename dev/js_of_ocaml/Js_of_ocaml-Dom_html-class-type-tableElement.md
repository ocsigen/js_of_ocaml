
# Class type `Dom_html.tableElement`

```ocaml
inherit element
```
```ocaml
method caption : tableCaptionElement Js_of_ocaml__.Js.t Js_of_ocaml__.Js.prop
```
```ocaml
method tHead : tableSectionElement Js_of_ocaml__.Js.t Js_of_ocaml__.Js.prop
```
```ocaml
method tFoot : tableSectionElement Js_of_ocaml__.Js.t Js_of_ocaml__.Js.prop
```
```ocaml
method rows : tableRowElement collection Js_of_ocaml__.Js.t
                Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method tBodies : tableSectionElement collection Js_of_ocaml__.Js.t
                   Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method align : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                 Js_of_ocaml__.Js.prop
```
```ocaml
method border : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                  Js_of_ocaml__.Js.prop
```
```ocaml
method cellPadding : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                       Js_of_ocaml__.Js.prop
```
deprecated Use CSS instead.
```ocaml
method cellSpacing : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                       Js_of_ocaml__.Js.prop
```
deprecated Use CSS instead.
```ocaml
method frame : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                 Js_of_ocaml__.Js.prop
```
deprecated Use CSS instead.
```ocaml
method rules : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                 Js_of_ocaml__.Js.prop
```
deprecated Use CSS instead.
```ocaml
method summary : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                   Js_of_ocaml__.Js.prop
```
deprecated Use CSS instead.
```ocaml
method width : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                 Js_of_ocaml__.Js.prop
```
```ocaml
method createTHead : tableSectionElement Js_of_ocaml__.Js.t
                       Js_of_ocaml__.Js.meth
```
```ocaml
method deleteTHead : unit Js_of_ocaml__.Js.meth
```
```ocaml
method createTFoot : tableSectionElement Js_of_ocaml__.Js.t
                       Js_of_ocaml__.Js.meth
```
```ocaml
method deleteTFoot : unit Js_of_ocaml__.Js.meth
```
```ocaml
method createCaption : tableCaptionElement Js_of_ocaml__.Js.t
                         Js_of_ocaml__.Js.meth
```
```ocaml
method deleteCaption : unit Js_of_ocaml__.Js.meth
```
```ocaml
method insertRow : int ->
  tableRowElement Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
```ocaml
method deleteRow : int -> unit Js_of_ocaml__.Js.meth
```