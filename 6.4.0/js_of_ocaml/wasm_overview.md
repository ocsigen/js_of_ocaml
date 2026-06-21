
# Wasm\_of\_ocaml


## Overview

Wasm\_of\_ocaml is a compiler from OCaml bytecode programs to WebAssembly. It provides an alternative way to run pure OCaml programs in JavaScript environments like browsers and Node.js.

The compiler is provided by the `wasm_of_ocaml-compiler` package. The [Js\_of\_ocaml libraries](./overview.md) are compatible with this compiler.


## Installation

The easiest way to install wasm\_of\_ocaml is to use opam:

```
opam install wasm_of_ocaml-compiler js_of_ocaml js_of_ocaml-ppx js_of_ocaml-lwt
```
Binaryen version 119 or later is required. The opam command above will install it automatically if needed.


## Usage

Your program must first be compiled using the OCaml bytecode compiler `ocamlc`. JavaScript bindings are provided by the `js_of_ocaml` package and the syntax extension by the `js_of_ocaml-ppx` package:

```
ocamlfind ocamlc -package js_of_ocaml -package js_of_ocaml-ppx \
    -linkpkg -o cubes.byte cubes.ml
```
Then, run the `wasm_of_ocaml` compiler to produce Wasm code:

```
wasm_of_ocaml cubes.byte
```
This produces a JavaScript loading script `cubes.js` and a directory `cubes.assets` containing the Wasm code.


### With dune

Dune has native support for wasm\_of\_ocaml (starting with dune 3\.17.0). It supports both standard and separate compilation. See the [dune documentation](https://dune.readthedocs.io/en/latest/wasmoo.html).


## Supported features

Most of the OCaml standard library is supported. However:

- Most of the `Sys` module is not supported
Extra libraries distributed with OCaml (such as Thread) are not supported in general. However:

- `Bigarray` \- supported using Typed Arrays
- `Str` \- supported
- `Graphics` \- partially supported using canvas (see also `js_of_ocaml-lwt.graphics`)
- `Unix` \- time-related functions are supported

### Differences from js\_of\_ocaml

Compared to js\_of\_ocaml:

- The default effect handler mode is `jspi` (instead of `disabled`)
- The `--effects` flag accepts `jspi` instead of `double-translation`
- There is no `build-fs` subcommand; use the `--file` flag instead

### Effect handlers

OCaml 5\.x code using effect handlers can be compiled in three ways:

- **`--effects=cps`** \- Uses the CPS transformation from js\_of\_ocaml
- **`--effects=jspi`** (default) \- Uses the [JavaScript-Promise Integration extension](https://github.com/WebAssembly/js-promise-integration)
- **`--effects=native`** \- Uses the [WebAssembly Stack Switching proposal](https://github.com/WebAssembly/stack-switching)
The CPS transformation is not the default since the generated code is slower, larger, and less readable. However, the JSPI extension is currently only available in Chrome 137 and Node.js 25 (or higher), and performing effects is slower when using it. Use `--effects=cps` for other browsers.

The native implementation is based on the WebAssembly typed continuations proposal (stack switching). It provides the best performance but requires a runtime with support for the WasmFX extension (currently available, behind the `--experimental-wasm-wasmfx` flag, in Chrome 148 or higher, or in a recent Node.js canary release (V8 version 14\.7.100 or higher)).


## WASI support

You can produce a WASI binary by running `wasm_of_ocaml` with the `--enable wasi` flag. At the moment, `wasm_of_ocaml` supports WASI 0\.1. Features from the Sys and Unix modules are available whenever they're supported by the WASI API.

The binaries produced by `wasm_of_ocaml` require the GC and exception-handling proposals, which are supported by Node.js, Wasmtime (with the `-W=all-proposals=y` flag), and the Wizard engine. Wasmtime does not support the legacy Wasm exception-handling instructions, so when `--enable wasi` is used the compiler emits the new `exnref`\-based instructions instead.

When native effect handlers are used (`--effects native`), the binaries additionally require the stack-switching proposal. It is supported by Node.js and by the Wizard engine (with the `--ext:stack-switching` flag), but not by Wasmtime.

For now, the output remains the same as without the `--enable wasi` flag: a JavaScript file `foo.js` and a directory `foo.assets` containing the Wasm code `code.wasm`. The JavaScript file can be used to run the WASI binary with `node`, while the Wasm code can be run directly by other Wasm engines.


### Limitations

The WASI target currently has the following limitations:

- **No JavaScript interoperability** \- the JavaScript binding layer is not available, so the `js_of_ocaml` library (`Js`, `Js.Unsafe`, the DOM bindings, `XMLHttpRequest`, ...) cannot be used.
- **Weak references and ephemerons are not weak** \- values reachable through a `Weak` array or an `Ephemeron` are kept alive and never reclaimed by the garbage collector.
- **Marshalling is quadratic** \- sharing is detected with a linear scan, so `Marshal` and `output_value` run in time quadratic in the number of heap blocks serialised.
- **No dynamic linking** \- the `Dynlink` library is not supported.
- **Limited system interface** \- only the operations exposed by WASI 0\.1 are available. In particular, there is no networking or sockets, no process control (`fork`, `exec`, ...) and no signals; `Sys.command` and `Unix.rewinddir` are not implemented. File system access is restricted to the directories preopened by the host (the `node` launcher preopens `.` and `/tmp`).

## Binding with JavaScript libraries

Js\_of\_ocaml lets you bind code with JavaScript libraries by linking `.js` files. Similarly, wasm\_of\_ocaml allows linking Wasm modules (`.wasm` or `.wat` files). See [Writing Wasm primitives](./wasm_runtime.md).

If a js\_of\_ocaml project uses `external` primitives defined in companion `.js` files, you need to implement the same primitives in Wasm modules to build with wasm\_of\_ocaml.


## Toplevel

Wasm\_of\_ocaml can compile an OCaml toplevel (REPL) to WebAssembly. The toplevel dynamically compiles OCaml bytecode to Wasm and instantiates it in the browser.

See [Toplevel and Dynlink](./build-toplevel.md) for build instructions.


## Dynlink

OCaml's `Dynlink` module is supported. Plugin `.cmo` files are compiled on the fly to WebAssembly and instantiated at runtime.

See [Toplevel and Dynlink](./build-toplevel.md) for build instructions.


## Pseudo filesystem

The pseudo filesystem is supported. Use the `--file` flag to embed files, the same way as with js\_of\_ocaml:

```
wasm_of_ocaml --file data.txt cubes.byte
```

## See also

- [Toplevel and Dynlink](./build-toplevel.md) — Build a toplevel or use dynamic linking
- [Writing Wasm primitives](./wasm_runtime.md) — Implement custom Wasm primitives
- [Js\_of\_ocaml overview](./overview.md) — JavaScript compilation alternative