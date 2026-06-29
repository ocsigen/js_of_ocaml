
# Class type `ServiceWorker.navigationPreloadManager`

Manages navigation preloading for a [`serviceWorkerRegistration`](./Js_of_ocaml-ServiceWorker-class-type-serviceWorkerRegistration.md), exposed as its `navigationPreload`.

```ocaml
method enable : unit Js_of_ocaml__.Promise.t Js_of_ocaml__.Js.meth
```
```ocaml
method disable : unit Js_of_ocaml__.Promise.t Js_of_ocaml__.Js.meth
```
```ocaml
method setHeaderValue : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Promise.t Js_of_ocaml__.Js.meth
```
Sets the value sent in the `Service-Worker-Navigation-Preload` request header for preload requests.

```ocaml
method getState : navigationPreloadState Js_of_ocaml__.Js.t
                    Js_of_ocaml__.Promise.t
                    Js_of_ocaml__.Js.meth
```