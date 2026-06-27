
# Module `Js_of_ocaml.Notification`

Notifications API.

A code example:

```ocaml
  if Notification.is_supported ()
  then
    Promise.map
      (fun perm ->
        if Js.to_string perm = "granted"
        then
          let options = Notification.empty_options () in
          options##.body := Js.string "Hello from OCaml!";
          ignore (new%js Notification.notification_with_options
                    (Js.string "Title") options))
      (Notification.request_permission ())
```
see [https://developer.mozilla.org/en-US/docs/Web/API/Notifications\_API](https://developer.mozilla.org/en-US/docs/Web/API/Notifications_API) 
see [https://notifications.spec.whatwg.org/](https://notifications.spec.whatwg.org/) 
```ocaml
class type  notificationAction = object ... end
```
A single action shown alongside a (persistent) notification. Used both when building [`notificationOptions`](./Js_of_ocaml-Notification-class-type-notificationOptions.md) and when reading [`notification`](./#val-notification)'s `actions`; the members are therefore read-write.

```ocaml
class type  notificationOptions = object ... end
```
Initializer for [`notification`](./Js_of_ocaml-Notification-class-type-notification.md). All fields are optional; create an empty record with [`empty_options`](./#val-empty_options) and populate the ones you need.

```ocaml
val empty_options : unit -> notificationOptions Js.t
```
```ocaml
class type  notification = object ... end
```
```ocaml
val notification : (Js.js_string Js.t -> notification Js.t) Js.constr
```
```ocaml
val notification_with_options : 
  (Js.js_string Js.t ->
    notificationOptions Js.t ->
    notification Js.t)
    Js.constr
```
```ocaml
val permission : unit -> Js.js_string Js.t
```
The current permission to display notifications: `"granted"`, `"denied"` or `"default"`.

```ocaml
val max_actions : unit -> int
```
The maximum number of actions supported by notifications on this device.

```ocaml
val request_permission : unit -> Js.js_string Js.t Promise.t
```
Request permission from the user to display notifications. The promise resolves with the new permission value (see [`permission`](./#val-permission)).

```ocaml
val is_supported : unit -> bool
```
Whether the `Notification` global is available in the current environment.
