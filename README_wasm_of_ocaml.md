# Wasm_of_ocaml

Wasm_of_ocaml is an alternative compiler which compiles OCaml bytecode to WebAssembly.

## Supported engines

The generated code works with Chrome 119, Node.js 22, Firefox 122, and Safari 18.2 (or more recent versions of these applications).

In particular, the output code requires the following [Wasm extensions](https://webassembly.org/roadmap/) to run:
- [the GC extension](https://github.com/WebAssembly/gc), including functional references and 31-bit integers
- [the tail-call extension](https://github.com/WebAssembly/tail-call/blob/main/proposals/tail-call/Overview.md)
- [the exception handling extension](https://github.com/WebAssembly/exception-handling/blob/master/proposals/exception-handling/Exceptions.md)

OCaml 5.x code using effect handlers can be compiled in two different ways:
one can enable the CPS transformation from `js_of_ocaml` by passing the
`--effects=cps` flag. Without the flag `wasm_of_ocaml` will instead default to
`--effects=jspi` and emit code utilizing
- [the JavaScript-Promise Integration extension](https://github.com/WebAssembly/js-promise-integration/blob/main/proposals/js-promise-integration/Overview.md).


## Installation

The following commands will perform a minimal installation:
```
git clone https://github.com/ocsigen/js_of_ocaml
cd js_of_ocaml
opam pin add -n --with-version 6.0.0 .
opam install dune.3.17.0 wasm_of_ocaml-compiler
```
You may want to install additional packages. For instance:

```
opam install js_of_ocaml js_of_ocaml-ppx js_of_ocaml-lwt
```

## Running the test suite

The following commands can be used to set up an opam switch and run the test suite.
```
opam switch create wasm-tests 4.14.0
eval $(opam env --switch=wasm-tests)
opam pin add -n base.v0.16.1 git@github.com:ocaml-wasm/base#wasm
opam pin add -n time_now.v0.16.1 git@github.com:ocaml-wasm/time_now#wasm
opam pin add -n ppx_inline_test.v0.16.1 git@github.com:ocaml-wasm/ppx_inline_test#wasm
opam pin add -n ppx_expect.v0.16.1 git@github.com:ocaml-wasm/ppx_expect#wasm
opam pin add -y -n --with-version 6.0.0 .
opam install . --deps-only --with-test
WASM_OF_OCAML=true dune build @runtest-wasm
```

## Usage

You can try compiling the program in `examples/cubes`. Your program must first be compiled using the OCaml bytecode compiler `ocamlc`. JavaScript bindings are provided by the `js_of_ocaml` package. The syntax extension is provided by `js_of_ocaml-ppx` package. Package `js_of_ocaml-lwt` provides Javascript specific Lwt functions.

```
ocamlfind ocamlc -package js_of_ocaml,js_of_ocaml-ppx,js_of_ocaml-lwt -linkpkg -o cubes.byte cubes.mli cubes.ml
```

Then, run the `wasm_of_ocaml` compiler to produce WebAssembly code:

```
wasm_of_ocaml cubes.byte -o cubes.bc.js
```

This outputs a file `cubes.bc.js` which loads the WebAssembly code from file `cube.bc.wasm`. For debugging, we currently also output the generated WebAssembly code in text file to `cube.wat`. Since Chrome does not allow loading from the filesystem, you need to serve the files using some Web server. For instance:
```
python3 -m http.server 8000 --directory .
```

As a larger example, you can try [CAMLBOY](https://github.com/linoscope/CAMLBOY). You need to install a forked version of [Brr](https://github.com/ocaml-wasm/brr/tree/wasm). Once the Js_of_ocaml UI is compiled (with `dune build --profile release`), you can generate WebAssembly code instead with the following command:
```
wasm_of_ocaml _build/default/bin/web/index.bc-for-jsoo
```

## Implementation status

A large part of the runtime is [implemented](https://github.com/ocaml-wasm/wasm_of_ocaml/issues/5). File-related functions and dynamic linking are not supported yet.

## Compatibility with Js_of_ocaml

Since the value representation is different, some adaptations are necessary.

The most notable change is that, except for integers, OCaml numbers are no longer mapped to JavaScript numbers. So, explicit conversions `Js.to_float` and `Js.float` are now necessary to convert between OCaml floats and JavaScript numbers. The typing of JavaScript Typed Arrays has also been changed to deal with this.
