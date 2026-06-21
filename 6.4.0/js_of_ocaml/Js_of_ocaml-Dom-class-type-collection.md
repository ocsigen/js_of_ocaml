
# Class type `Dom.collection`

Specification of `HTMLCollection` objects. Returned by `Element.children`, `getElementsByTagName`, `getElementsByClassName`, etc. Always live, contains only elements, and adds a `namedItem` lookup.

```ocaml
inherit 'node nodeList
```
```ocaml
method namedItem : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  'node Js_of_ocaml__.Js.t Js_of_ocaml__.Js.opt Js_of_ocaml__.Js.meth
```