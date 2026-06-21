
# Module `Js_of_ocaml.Geolocation`

Geolocation API

A code example:

```ocaml
if (Geolocation.is_supported()) then
  let geo = Geolocation.geolocation in
  let options = Geolocation.empty_position_options() in
  let () = options##.enableHighAccuracy := true in
  let f_success pos =
    let coords = pos##.coords in
    let latitude = coords##.latitude in
    Console.console##debug latitude ;
  in
  let f_error err =
    let code = err##.code in
    let msg = err##.message in
    if code = err##._TIMEOUT then Console.console##debug(msg)
  in
  geo##getCurrentPosition (Js.wrap_callback f_success) (Js.wrap_callback f_error) options
```
see [https://developer.mozilla.org/en-US/docs/Web/API/Geolocation](https://developer.mozilla.org/en-US/docs/Web/API/Geolocation) for API documentation.
see [http://www.w3.org/TR/geolocation-API/](http://www.w3.org/TR/geolocation-API/) for the W3C Recommendation.
```ocaml
type positionErrorCode
```
```ocaml
type watchId
```
```ocaml
class type  coordinates = object ... end
```
```ocaml
class type  position = object ... end
```
```ocaml
class type  positionOptions = object ... end
```
```ocaml
class type  positionError = object ... end
```
```ocaml
class type  geolocation = object ... end
```
```ocaml
val empty_position_options : unit -> positionOptions Js.t
```
```ocaml
val geolocation : geolocation Js.t
```
```ocaml
val is_supported : unit -> bool
```