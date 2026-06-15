
# Toplevel and Dynlink

This page explains how to build an OCaml toplevel (REPL) that runs in the browser, and how to use dynamic linking to load bytecode at runtime.

Both js\_of\_ocaml and wasm\_of\_ocaml support these features.


## Building a toplevel

A toplevel is an interactive OCaml environment (like `ocaml` or `utop`). Js\_of\_ocaml and wasm\_of\_ocaml can compile a toplevel to JavaScript or WebAssembly, allowing it to run in the browser. See the [live demo](files/toplevel/index.html).


### With js\_of\_ocaml

1. Initialize the toplevel in your OCaml code using `Js_of_ocaml_toplevel.JsooTop.initialize`
1. Build your bytecode with debug info and linkall:
   
   ```
   ocamlfind ocamlc -g -linkall -package js_of_ocaml-toplevel \
    -linkpkg toplevel.ml -o toplevel.byte
   ```
1. Compile to JavaScript with the `--toplevel` flag:
   
   ```
   js_of_ocaml --toplevel toplevel.byte -o toplevel.js
   ```

### With wasm\_of\_ocaml

1. Initialize the toplevel in your OCaml code using `Js_of_ocaml_toplevel.JsooTop.initialize`
1. Build your bytecode with debug info and linkall, linking `wasm_of_ocaml-compiler.dynlink` and `js_of_ocaml-toplevel.common`:
   
   ```
   ocamlfind ocamlc -g -linkall -package wasm_of_ocaml-compiler.dynlink \
    -package js_of_ocaml-toplevel.common -package compiler-libs.toplevel \
    -linkpkg toplevel.ml -o toplevel.byte
   ```
1. Compile to WebAssembly with the `--toplevel` flag:
   
   ```
   wasm_of_ocaml --toplevel toplevel.byte -o toplevel.js
   ```

### Limiting available modules

By default, all linked modules are available in the toplevel. To limit this, use `--export FILE` where `FILE` lists compilation unit names (one per line).

The `jsoo_listunits` tool generates this list from findlib libraries:

```
jsoo_listunits -o units.txt stdlib str
js_of_ocaml --toplevel --export units.txt toplevel.byte
```
The `--export` flag works the same way with wasm\_of\_ocaml.


## Using the Dynlink library

OCaml's `Dynlink` module lets you load bytecode files at runtime. This works in both js\_of\_ocaml and wasm\_of\_ocaml with some setup.


### With js\_of\_ocaml

1. Link `js_of_ocaml-compiler.dynlink` in your program (initializes dynlink support)
1. Build your bytecode with debug info and linkall:
   
   ```
   ocamlfind ocamlc -g -linkall -package dynlink \
    -package js_of_ocaml-compiler.dynlink -linkpkg main.ml -o main.byte
   ```
1. Compile to JavaScript with the `--dynlink` flag:
   
   ```
   js_of_ocaml --dynlink main.byte -o main.js
   ```

### With wasm\_of\_ocaml

1. Link `wasm_of_ocaml-compiler.dynlink` in your program (initializes dynlink support)
1. Build your bytecode with debug info and linkall:
   
   ```
   ocamlfind ocamlc -g -linkall -package dynlink \
    -package wasm_of_ocaml-compiler.dynlink -linkpkg main.ml -o main.byte
   ```
1. Compile to WebAssembly with the `--dynlink` flag:
   
   ```
   wasm_of_ocaml --dynlink main.byte -o main.js
   ```
Plugin `.cmo` files are compiled on the fly to WebAssembly and instantiated at runtime.


### Example

```ocaml
(* main.ml *)
let () = Dynlink.loadfile "./plugin.cmo"
```
With js\_of\_ocaml:

```
# Compile main program
ocamlfind ocamlc -g -linkall -package dynlink \
    -package js_of_ocaml-compiler.dynlink -linkpkg main.ml -o main.byte
js_of_ocaml --dynlink main.byte -o main.js

# Compile plugin
ocamlfind ocamlc -c plugin.ml

# Run
node ./main.js
```
With wasm\_of\_ocaml:

```
# Compile main program
ocamlfind ocamlc -g -linkall -package dynlink \
    -package wasm_of_ocaml-compiler.dynlink -linkpkg main.ml -o main.byte
wasm_of_ocaml --dynlink main.byte -o main.js

# Compile plugin
ocamlfind ocamlc -c plugin.ml

# Run
node ./main.js
```

## Loading precompiled files

By default, `Dynlink.loadfile` compiles `.cmo` and `.cma` files on the fly. You can also precompile plugins ahead of time for faster loading.


### With js\_of\_ocaml

Use `js_of_ocaml` to compile a `.cmo` or `.cma` file to JavaScript:

```
js_of_ocaml plugin.cmo -o plugin.js
js_of_ocaml plugin2.cma -o plugin2.js
```
Then load the resulting JavaScript file at runtime. In Node.js, use `require`:

```ocaml
let require s =
  Js_of_ocaml.Js.Unsafe.fun_call
    (Js_of_ocaml.Js.Unsafe.js_expr "require")
    [| Js_of_ocaml.Js.Unsafe.inject (Js_of_ocaml.Js.string s) |]

let () = require "./plugin.js"
```
In a browser, use a `<script>` tag:

```html
<script src="plugin.js"></script>
```

### With wasm\_of\_ocaml

Use `wasm_of_ocaml compile` to compile a `.cmo` to a `.wasmo` file, or a `.cma` to a `.wasma` file:

```
wasm_of_ocaml compile plugin.cmo -o plugin.wasmo
wasm_of_ocaml compile plugin2.cma -o plugin2.wasma
```
Then load the precompiled file using `Wasm_of_ocaml_compiler_dynlink.loadfile`:

```ocaml
let () = Wasm_of_ocaml_compiler_dynlink.loadfile "./plugin.wasmo"
```
This skips the on-the-fly bytecode-to-Wasm compilation step, which can significantly reduce loading time for large plugins.


## See also

- [Command-line options](./options.md) — The `--toplevel` and `--dynlink` flags
- [Compilation modes](./compilation-modes.md) — Whole program vs separate compilation
- [`index`](./index.md) — Toplevel API