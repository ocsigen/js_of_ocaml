
# Class type `IntersectionObserver.intersectionObserver`

```ocaml
method root : Js_of_ocaml__.Dom.node Js_of_ocaml__.Js.t Js_of_ocaml__.Js.opt
                Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method rootMargin : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                      Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method thresholds : Js_of_ocaml__.Js.number_t Js_of_ocaml__.Js.js_array
                      Js_of_ocaml__.Js.t
                      Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method observe : Js_of_ocaml__.Dom.node Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method unobserve : Js_of_ocaml__.Dom.node Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method disconnect : unit Js_of_ocaml__.Js.meth
```
```ocaml
method takeRecords : intersectionObserverEntry Js_of_ocaml__.Js.t
                       Js_of_ocaml__.Js.js_array
                       Js_of_ocaml__.Js.t
                       Js_of_ocaml__.Js.meth
```