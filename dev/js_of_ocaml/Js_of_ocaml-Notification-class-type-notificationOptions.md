
# Class type `Notification.notificationOptions`

Initializer for [`notification`](./Js_of_ocaml-Notification-class-type-notification.md). All fields are optional; create an empty record with [`empty_options`](./Js_of_ocaml-Notification.md#val-empty_options) and populate the ones you need.

```ocaml
method dir : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
               Js_of_ocaml__.Js.writeonly_prop
```
Text direction: `"auto"` (default), `"ltr"` or `"rtl"`.

```ocaml
method lang : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method body : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method navigate : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                    Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method tag : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
               Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method icon : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method image : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                 Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method badge : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                 Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method vibrate : int Js_of_ocaml__.Js.js_array Js_of_ocaml__.Js.t
                   Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method data : Js_of_ocaml__.Js.Unsafe.any Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method requireInteraction : bool Js_of_ocaml__.Js.t
                              Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method silent : bool Js_of_ocaml__.Js.t Js_of_ocaml__.Js.opt
                  Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method renotify : bool Js_of_ocaml__.Js.t Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method timestamp : Js_of_ocaml__.Js.number_t Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method actions : notificationAction Js_of_ocaml__.Js.t
                   Js_of_ocaml__.Js.js_array
                   Js_of_ocaml__.Js.t
                   Js_of_ocaml__.Js.writeonly_prop
```