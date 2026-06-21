
# Lwt support

The `js_of_ocaml-lwt` package provides integration between Lwt and browser APIs. It includes:

- `Lwt_js` — basic JavaScript bindings (sleep, yield)
- `Lwt_js_events` — DOM event handling with Lwt
- `Lwt_xmlHttpRequest` — XMLHttpRequest with Lwt
- `Lwt_jsonp` — JSONP requests

## Events

The `Js_of_ocaml_lwt.Lwt_js_events` module provides a concise way to handle DOM events using Lwt threads, as an alternative to [Dom\_html.addEventListener](./Js_of_ocaml-Dom_html.md#val-addEventListener).

This module defines functions you can call on a DOM element to create an Lwt thread that will return when the event occurs.

Example:

```ocaml
Lwt.ignore_result (Lwt_js_events.click target >>= handler)
```
The handler receives the JS event as parameter.

Each of these functions has a version (same name with an ending "s") that loops when the handler terminates:

```ocaml
Lwt.ignore_result (Lwt_js_events.clicks target handler)
```

## Cancellation

To remove an event handler, cancel the Lwt thread using `Lwt.cancel`. You can also use `Lwt.pick`. For example, the following code waits for a click on either `t1` or `t2`:

```ocaml
Lwt.pick [
  Lwt_js_events.click t1 >>= handler1;
  Lwt_js_events.click t2 >>= handler2
]
```
**Warning**: If you use `Lwt.pick` and your handlers take time, other event listeners will not be cancelled until the handler terminates. It is better to return immediately after launching long-running handlers.


## API reference

See `Js_of_ocaml_lwt.Lwt_js_events` for the full API.


## See also

- [JavaScript interop](./javascript-interop.md) — JavaScript binding fundamentals
- [`index`](./index.md) — Full Lwt support API