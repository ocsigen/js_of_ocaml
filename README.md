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
