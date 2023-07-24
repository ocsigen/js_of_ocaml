# Wasm_of_ocaml

Wasm_of_ocaml is a fork of Js_of_ocaml which compiles OCaml bytecode to WebAssembly.

## Requirements

Wasm_of_ocaml relies on the Binaryen toolchain. At the moment, you need to install it [from the main branch on GitHub](https://github.com/WebAssembly/binaryen/).

## Supported engines

The generated code works with [Chrome beta](https://www.google.com/chrome/beta/) and [node V8 canary](https://nodejs.org/download/v8-canary/v21.0.0-v8-canary20230711fb76fe1ec2/). For Chrome, you need to enable WebAssembly Garbage Collection and WebAssembly Stringref from chrome://flags/. For node, you need to use the following flags:`--experimental-wasm-gc  --experimental-wasm-stringref`.

## Installation

The following commands will perform a minimal installation:
```
opam pin add -n --with-version 5.3.0 .
opam install wasm_of_ocaml-compiler
```
You may want to install additional packages. For instance:

```
opam install js_of_ocaml-ppx js_of_ocaml-lwt
```

## Usage

You can try compiling the program in `examples/cubes`. Your program must first be compiled using the OCaml bytecode compiler `ocamlc`. JavaScript bindings are provided by the `js_of_ocaml` package. The syntax extension is provided by `js_of_ocaml-ppx` package. Package `js_of_ocaml-lwt` provides Javascript specific Lwt functions.

```
ocamlfind ocamlc -package js_of_ocaml,js_of_ocaml-ppx,js_of_ocaml-lwt -linkpkg -o cubes.byte cubes.mli cubes.ml
```

Then, run the `wasm_of_ocaml` compiler to produce WebAssembly code:

```
wasm_of_ocaml cubes.byte
```

This outputs a file `cubes.js` which loads the WebAssembly code from file `cube.wasm`. For debugging, we currently also output the generated WebAssembly code in text file to `cube.wat`. Since Chrome does not allow loading from the filesystem, you need to serve the files using some Web server. For instance:
```
python3 -m http.server 8000 --directory .
```

As a larger example, you can try [CAMLBOY](https://github.com/linoscope/CAMLBOY). You need to install a forked version of [Brr](https://github.com/ocaml-wasm/brr/tree/wasm). Once the Js_of_ocaml UI is compiled (with `dune build`), you can generate WebAssembly code instead with the following command:
```
wasm_of_ocaml _build/default/bin/web/index.bc-for-jsoo
```

## Implementation status

A large part of the runtime is [implemented](https://github.com/ocaml-wasm/wasm_of_ocaml/issues/5). File-related functions, marshaling and dynamic linking are not supported yet.

Separate compilation is not implemented yet.

## Compatibility with Js_of_ocaml

Since the value representation is different, some adaptations are necessary.

The most notable change is that, except for integers, OCaml numbers are no longer mapped to JavaScript numbers. So, explicit conversions `Js.to_float` and `Js.float` are now necessary to convert between OCaml floats and JavaScript numbers. The typing of JavaScript Typed Arrays has also been changed to deal with this.

Additionally, OCaml physical equality will not work properly on JavaScript objects (it compares boxed values instead of values themselves). You should use `Js.string_equals` instead.

Some forked versions of [Brr](https://github.com/ocaml-wasm/brr/tree/wasm) and
[Gen_js_api](https://github.com/ocaml-wasm/gen_js_api/tree/wasm) are compatible with Wasm_of_ocaml.
