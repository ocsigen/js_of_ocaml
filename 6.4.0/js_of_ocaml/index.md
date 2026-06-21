
# Js\_of\_ocaml

Js\_of\_ocaml is a compiler from OCaml bytecode programs to JavaScript. It makes it possible to run pure OCaml programs in JavaScript environments like browsers and Node.js. A companion compiler, *wasm\_of\_ocaml*, targets WebAssembly.


## Manual

- [Overview](./overview.md) — what Js\_of\_ocaml is and how to use it
- [Installation](./install.md)
- [Quick Start](./quickstart.md)
- [JavaScript interoperability](./javascript-interop.md)
- [PPX syntax extension](./ppx.md) and [JSON derivation](./ppx-deriving.md)
- [Export OCaml code to JavaScript](./rev-bindings.md)
- [Error handling](./errors.md)
- [Lwt support](./lwt.md)
- [Command-line options](./options.md), [compilation modes](./compilation-modes.md), [JavaScript primitives](./linker.md), [tail-call optimization](./tailcall.md), [effect handlers](./effects.md), [runtime representation](./runtime-representation.md)
- [Targeting older browsers](./browser-compat.md)
- [wasm\_of\_ocaml overview](./wasm_overview.md) and [writing Wasm primitives](./wasm_runtime.md)
- [Build a toplevel](./build-toplevel.md), [debug a program](./debug.md), [performance](./performances.md), [contribute](./contribute.md)
- [Examples and projects](./examples.md)

## API reference

See the [API overview](./api.md) for the full list of modules, or jump to the main libraries: [`Js_of_ocaml.Js`](./Js_of_ocaml-Js.md), [`Js_of_ocaml.Dom_html`](./Js_of_ocaml-Dom_html.md), `Js_of_ocaml_lwt.Lwt_js_events`, `Js_of_ocaml_tyxml.Tyxml_js`.
