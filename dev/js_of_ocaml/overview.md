
# Js\_of\_ocaml


## Overview

Js\_of\_ocaml is a compiler from OCaml bytecode programs to JavaScript. It makes it possible to run pure OCaml programs in JavaScript environments like browsers and Node.js. It is easy to install as it works with an existing installation of OCaml, with no need to recompile any library. It comes with bindings for a large part of the browser APIs.

According to our benchmarks, the generated programs [run typically faster](./performances.md) than with the OCaml bytecode interpreter. We believe this compiler will prove much easier to maintain than a retargeted OCaml compiler, as the bytecode provides a very stable API.

Js\_of\_ocaml is composed of multiple packages:

- `js_of_ocaml-compiler` \- the compiler
- `js_of_ocaml-ppx` \- ppx syntax extension
- `js_of_ocaml` \- the base library
- `js_of_ocaml-ppx_deriving_json` \- JSON derivation
- `js_of_ocaml-lwt` \- Lwt support
- `js_of_ocaml-tyxml` \- TyXML support
- `js_of_ocaml-toplevel` \- tools to [build an OCaml toplevel](./build-toplevel.md)
There is also a compiler targeting WebAssembly provided by the `wasm_of_ocaml` package. See [wasm\_of\_ocaml](./wasm_overview.md).

**Note**: All code examples in this manual use Js\_of\_ocaml's ppx syntax. It is possible to use Js\_of\_ocaml purely as a compiler while using a different package (e.g., gen\_js\_api, brr) to provide bindings to the browser APIs.


## Installation

The easiest way to install js\_of\_ocaml is to use opam:

```
opam install js_of_ocaml js_of_ocaml-ppx js_of_ocaml-lwt
```
For alternatives, see [Installation](./install.md).


## Usage

Your program must first be compiled using the OCaml bytecode compiler `ocamlc`. JavaScript bindings are provided by the `js_of_ocaml` package and the syntax extension by the `js_of_ocaml-ppx` package:

```
ocamlfind ocamlc -package js_of_ocaml -package js_of_ocaml-ppx \
    -linkpkg -o cubes.byte cubes.ml
```
Then, run the `js_of_ocaml` compiler to produce JavaScript code:

```
js_of_ocaml cubes.byte
```
**Note**: Generated JavaScript files are UTF-8 encoded.


### With dune

Dune has native support for js\_of\_ocaml. It supports both standard and separate compilation of JavaScript files. See the [dune documentation](https://dune.readthedocs.io/en/latest/jsoo.html).


### With ocamlbuild (legacy)

Js\_of\_ocaml provides an ocamlbuild plugin. See [js\_of\_ocaml-ocamlbuild](https://github.com/ocsigen/js_of_ocaml-ocamlbuild).


### Toplevel

You can find an OCaml toplevel running in the browser [here](files/toplevel/index.html). See [Building a toplevel](./build-toplevel.md) to create your own.


## Supported features

Most of the OCaml standard library is supported. However:

- Most of the `Sys` module is not supported
Extra libraries distributed with OCaml (such as Thread) are not supported in general. However:

- `Bigarray` \- supported using Typed Arrays
- `Num` \- supported
- `Str` \- supported
- `Graphics` \- partially supported using canvas (see also `js_of_ocaml-lwt.graphics`)
- `Unix` \- time-related functions are supported

### Tail call optimization

Tail calls are not optimized in general. However, mutually recursive functions are optimized:

- Self-recursive functions (when the tail calls are the function itself) are compiled using a loop
- Trampolines are used otherwise
See [tailcall optimization](./tailcall.md) for more details.


### Effect handlers

Effect handlers are fully supported with the `--effects=..` flag. Effect support is disabled by default since effects are not widely used and the generated code can be slower, larger, and less readable. See [effect handlers](./effects.md) for details.


### Data representation

Data representation differs from the usual OCaml runtime:

- Integers are 32 bits (rather than 31 or 63 bits), their natural size in JavaScript
- Floats are not boxed
As a consequence, marshalling, polymorphic comparison, and hashing functions can yield different results:

- Marshalling floats might generate different output (do not unmarshal with the standard OCaml runtime)
- The polymorphic hash function will not give the same results on data structures containing floats
- These functions may be more prone to stack overflow
**Note**: Float rounding is slightly different between native and JavaScript. Both round to nearest but resolve ties differently: JavaScript resolves ties away from zero while libc resolves ties to even.

See [runtime representation](./runtime-representation.md) for details.
