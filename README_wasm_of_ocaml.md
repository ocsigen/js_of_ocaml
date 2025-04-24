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

## Installation and usage

Installation and usage documentation can be found in [the js_of_ocaml manual](https://ocsigen.org/js_of_ocaml/dev/manual/wasm_overview).

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

## Compatibility with Js_of_ocaml

Since the value representation is different, some adaptations are necessary.

The most notable change is that, except for integers, OCaml numbers are no longer mapped to JavaScript numbers. So, explicit conversions `Js.to_float` and `Js.float` are now necessary to convert between OCaml floats and JavaScript numbers. The typing of JavaScript Typed Arrays has also been changed to deal with this.
