# Wasm_of_ocaml

Wasm_of_ocaml is a fork of Js_of_ocaml which compiles OCaml bytecode to WebAssembly.

## Requirements

Wasm_of_ocaml relies on the Binaryen toolchain. At the moment, you need to install a [specific fork](https://github.com/vouillon/binaryen/tree/fixes): we rely on a number of unreleased fixes, and the main branch uses a new format for the `br_on_cast` instruction which is not yet supported by `node`.

## Supported engines

The generated code works with [Chrome beta](https://www.google.com/chrome/beta/) and [node nightly](https://nodejs.org/download/nightly/v21.0.0-nightly20230628900ae1bda7/). For Chrome, you need to enable WebAssembly Garbage Collection and WebAssembly Stringref from chrome://flags/. For node, you need to use the following flags:`--experimental-wasm-gc  --experimental-wasm-stringref`.

## Installation

```
opam pin add . wasm_of_ocaml js_of_ocaml js_of_ocaml-compiler js_of_ocaml-ppx
```

## Usage

Your program must first be compiled using the OCaml bytecode compiler `ocamlc`.
JavaScript bindings are provided by the `js_of_ocaml` package. The syntax
extension is provided by `js_of_ocaml-ppx` package.

```
ocamlfind ocamlc -package js_of_ocaml,js_of_ocaml-ppx,js_of_ocaml-lwt -linkpkg -o cubes.byte cubes.ml
```

Then, run the `wasm_of_ocaml` compiler to produce WebAssembly code:

```
wasm_of_ocaml cubes.byte
```

This outputs a file `cubes.js` which loads the WebAssembly code from file `cube.wasm`. For debugging, we currently also output the generated WebAssembly code in text file to `cube.wat`. Since Chrome does not allow loading from the filesystem, you need to serve the files using some Web server. For instance:
```
python3 -m http.server 8000 --directory .
```
