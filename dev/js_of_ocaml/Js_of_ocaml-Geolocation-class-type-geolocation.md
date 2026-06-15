
# Class type `Geolocation.geolocation`

```ocaml
method getCurrentPosition : (position Js_of_ocaml__.Js.t ->
                              unit)
                              Js_of_ocaml__.Js.callback ->
  (positionError Js_of_ocaml__.Js.t -> unit) Js_of_ocaml__.Js.callback ->
  positionOptions Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method watchPosition : (position Js_of_ocaml__.Js.t ->
                         unit)
                         Js_of_ocaml__.Js.callback ->
  (positionError Js_of_ocaml__.Js.t -> unit) Js_of_ocaml__.Js.callback ->
  positionOptions Js_of_ocaml__.Js.t ->
  watchId Js_of_ocaml__.Js.meth
```
```ocaml
method clearWatch : watchId -> unit Js_of_ocaml__.Js.meth
```