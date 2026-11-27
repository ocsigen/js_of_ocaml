# Module `Js_of_ocaml_devtools`

Browser developer tools integration (library `js_of_ocaml.devtools`).

Chrome DevTools (and other Chromium-based consoles) let a page install [custom formatters](https://firefox-source-docs.mozilla.org/devtools-user/custom_formatters/) in `globalThis.devtoolsFormatters` to control how objects are displayed in the console and in the debugger.

When the compiler is built with Introcaml, OCaml values carry a descriptor of their type at runtime and can be printed in OCaml syntax through `Introspect`. `register_formatters` installs a formatter that uses it, so that a value logged with `console.log` or inspected in the debugger shows as, e.g., `Circle ({x = 0; name = "o"}, 1)` instead of `Array(3)`, and expands to its fields (with their names for records and constructors) rather than to the array of its raw representation.

Custom formatters must be enabled in the DevTools settings (Settings \> Preferences \> Console \> Enable custom formatters).

```ocaml
val register_formatters : unit -> unit
```
Install the formatter in `globalThis.devtoolsFormatters`, creating the array if needed. Calling it more than once installs it once.

This is a no-op when the compiler has no introspection support or when the program does not run under Js\_of\_ocaml (native code, Wasm\_of\_ocaml). When the program was compiled with `--disable introspection`, the formatter is installed but no value carries a descriptor, so nothing is reformatted.
