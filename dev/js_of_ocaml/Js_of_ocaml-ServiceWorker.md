
# Module `Js_of_ocaml.ServiceWorker`

Service Workers.

This module covers both sides of the API: the page-facing registration API ([`container`](./#val-container), [`serviceWorkerContainer`](./Js_of_ocaml-ServiceWorker-class-type-serviceWorkerContainer.md), [`serviceWorkerRegistration`](./Js_of_ocaml-ServiceWorker-class-type-serviceWorkerRegistration.md), [`serviceWorker`](./Js_of_ocaml-ServiceWorker-class-type-serviceWorker.md)) and the worker-side global scope ([`global`](./#val-global), [`serviceWorkerGlobalScope`](./Js_of_ocaml-ServiceWorker-class-type-serviceWorkerGlobalScope.md) and its events). The Cache API used inside a worker lives in [`Cache`](./Js_of_ocaml-Cache.md).

Service Workers require a secure context. The Promise-typed members use [`Promise`](./Js_of_ocaml-Promise.md).

see [https://developer.mozilla.org/en-US/docs/Web/API/Service\_Worker\_API](https://developer.mozilla.org/en-US/docs/Web/API/Service_Worker_API) 
see [https://w3c.github.io/ServiceWorker/](https://w3c.github.io/ServiceWorker/) 
```ocaml
class type  serviceWorker = object ... end
```
A service worker, as seen from a controlled page. Inherits `EventTarget`.

```ocaml
class type  registrationOptions = object ... end
```
Options for [`serviceWorkerContainer`](./Js_of_ocaml-ServiceWorker-class-type-serviceWorkerContainer.md)`##register_withOptions`. Create an empty record with [`empty_registration_options`](./#val-empty_registration_options).

```ocaml
val empty_registration_options : unit -> registrationOptions Js.t
```
```ocaml
class type  navigationPreloadState = object ... end
```
The state of a registration's navigation preload, as resolved by [`navigationPreloadManager`](./Js_of_ocaml-ServiceWorker-class-type-navigationPreloadManager.md)`##getState`.

```ocaml
class type  navigationPreloadManager = object ... end
```
Manages navigation preloading for a [`serviceWorkerRegistration`](./Js_of_ocaml-ServiceWorker-class-type-serviceWorkerRegistration.md), exposed as its `navigationPreload`.

```ocaml
class type  serviceWorkerRegistration = object ... end
```
A registration of a service worker against a scope. Inherits `EventTarget`.

```ocaml
class type  serviceWorkerContainer = object ... end
```
The page-facing entry point, exposed as `navigator.serviceWorker` (see [`container`](./#val-container)). Inherits `EventTarget`.

```ocaml
val container : unit -> serviceWorkerContainer Js.t Js.optdef
```
`navigator.serviceWorker`, or `undefined` when service workers are not supported (including non-secure contexts).

```ocaml
val is_supported : unit -> bool
```
Whether `navigator.serviceWorker` is available in the current environment.


## Service worker global scope

The following bindings are meant to be used from *inside* a service worker (see [`global`](./#val-global)); they are not available on a controlled page.

```ocaml
class type  extendableEvent = object ... end
```
Base class of events that may extend the service worker's lifetime via `waitUntil`.

```ocaml
class type  fetchEvent = object ... end
```
The event passed to the worker's `fetch` handler.

```ocaml
class type 'a extendableMessageEvent = object ... end
```
The event delivered to the worker's `message` / `messageerror` handlers. Unlike a plain [`MessageChannel.messageEvent`](./Js_of_ocaml-MessageChannel-class-type-messageEvent.md) it extends [`extendableEvent`](./Js_of_ocaml-ServiceWorker-class-type-extendableEvent.md), so the handler can call `waitUntil` to keep the worker alive until asynchronous processing of the message finishes. The type parameter `'a` is the type of the `data` payload.

```ocaml
class type  client = object ... end
```
A client (a `Window`, worker or shared worker) controlled by the worker.

```ocaml
class type  windowClient = object ... end
```
A [`client`](./Js_of_ocaml-ServiceWorker-class-type-client.md) that is a top-level browsing context (a window/tab).

```ocaml
class type  clientsQueryOptions = object ... end
```
Options for `clients##matchAll_withOptions`. Create an empty record with [`empty_clients_query_options`](./#val-empty_clients_query_options).

```ocaml
val empty_clients_query_options : unit -> clientsQueryOptions Js.t
```
```ocaml
class type  clients = object ... end
```
The worker's `clients` object, used to enumerate and message controlled clients.

```ocaml
class type  serviceWorkerGlobalScope = object ... end
```
The global scope of a running service worker (its `self`). Inherits `EventTarget`.

```ocaml
val global : unit -> serviceWorkerGlobalScope Js.t
```
The service worker's global scope. Only meaningful when the current code is actually running as a service worker.
