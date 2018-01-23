[![Build Status](https://travis-ci.org/ocsigen/js_of_ocaml.png)](https://travis-ci.org/ocsigen/js_of_ocaml)

# Js_of_ocaml (jsoo)

Js_of_ocaml is a compiler from OCaml bytecode to JavaScript. It makes
it possible to run pure OCaml programs in JavaScript environment
like browsers and Node.js.
  * It is easy to install and use as it works with an existing
    installation of OCaml, with no need to recompile any library.
  * It comes with bindings for a large part of the browser APIs.
  * According to our benchmarks, the generated programs runs typically
    faster than with the OCaml bytecode interpreter.
  * We believe this compiler will prove much easier to maintain than a
    retargeted OCaml compiler, as the bytecode provides a very stable
    API.
    
## Requirements
  Findlib, cppo
  See [opam](https://github.com/ocsigen/js_of_ocaml/blob/master/js_of_ocaml-compiler.opam) file for version constraints.

### optional
  * [camlp4](https://github.com/ocaml/camlp4)
  * [lwt](https://github.com/ocsigen/lwt)
  * [deriving](https://github.com/ocsigen/deriving)
  * [ppx_deriving](https://github.com/whitequark/ppx_deriving)
  * [tyxml](https://github.com/ocsigen/tyxml)
  * [reactiveData](https://github.com/ocsigen/reactiveData)
  * [yojson](https://github.com/mjambon/yojson)

### Toplevel requirements
 * base64, tyxml, reactiveData
 * ocp-indent: needed to support indentation in the toplevel
 * higlo: needed to support Syntax highlighting in the toplevel
 * cohttp: needed to build the toplevel webserver

## Installation

### Opam
```
opam install js_of_ocaml js_of_ocaml-ppx
```

### Manual
  * edit `Makefile.conf` to change the default configuration
  * run `make all` to compile
  * run `make install` as root to install the compiler
    and its libraries
  * run `make uninstall` as root to uninstall them

You can run `make toplevel-examples` if you want to build a Web-based OCaml
toplevel as well. [Try the toplevel](http://ocsigen.github.io/js_of_ocaml/)

## Usage

Your program must first be compiled using the OCaml bytecode compiler
`ocamlc`. JavaScript bindings are provided by the `js_of_ocaml` package.
The syntax extension is provided by `js_of_ocaml.syntax` package.

```
ocamlfind ocamlc -package js_of_ocaml -package js_of_ocaml.ppx -linkpkg -o cubes.byte cubes.ml
```

Then, run the `js_of_ocaml` compiler to produce JavaScript code:

```
js_of_ocaml cubes.byte
```

## Features

Most of the OCaml standard library is supported. However,
  * Weak semantic cannot be implemented using JavaScript.
    A dummy implementation is provided.
  * Most of the Sys module is not supported.

Extra libraries distributed with Ocaml (such as Thread or Str) are not
supported in general. However,
  * Bigarray: bigarrays are supported using Typed Arrays
  * Num: supported using `+nat.js` option
  * Graphics: partially supported using canvas (see js_of_ocaml.graphics)
  * Unix: time related functions are supported

Tail call is not optimized in general. However, mutually recursive
functions are optimized:
  * self recursive functions (when the tail calls are the function itself) are
    compiled using a loop.
  * trampolines are used otherwise.
[More](http://ocsigen.org/js_of_ocaml/dev/manual/tailcall) about tail call optimization.

Data representation differs from the usual one.  Most notably,
integers are 32 bits (rather than 31 bits or 63 bits), which is their
natural size in JavaScript, and floats are not boxed.  As a
consequence, marshalling, polymorphic comparison, and hashing
functions can yield results different from usual:
  * marshalling of floats is not supported (unmarshalling works);
  * the polymorphic hash function will not give the same results on
    datastructures containing floats;
  * these functions may be more prone to stack overflow.

## Toplevel
  * [Ocaml 4.02.3](http://ocsigen.github.io/js_of_ocaml/#version=4.02.3)
  * [Ocaml 4.03.0](http://ocsigen.github.io/js_of_ocaml/#version=4.03.0)
  * [Ocaml 4.04.2](http://ocsigen.github.io/js_of_ocaml/#version=4.04.2) includes Base, Core_kernel, Async_kernel, Async_js
  * [Ocaml 4.04.0+BER](http://ocsigen.github.io/js_of_ocaml/#version=4.04.0+BER) see http://okmij.org/ftp/ML/MetaOCaml.html
  * [Ocaml 4.05.0](http://ocsigen.github.io/js_of_ocaml/#version=4.05.0) includes Base, Core_kernel, Async_kernel, Async_js
  * [Ocaml 4.06.0](http://ocsigen.github.io/js_of_ocaml/#version=4.06.0) includes Base, Core_kernel, Async_kernel, Async_js

## Contents of the distribution
| Filename    | Description                                  |
|-----------  |----------------------------------------------|
| LICENSE     | license and copyright notice                 |
| README      | this file                                    |
| compiler/   | compiler                                     |
| examples/   | small examples                               |
| lib/        | library for interfacing with JavaScript APIs |
| ppx/        | ppx syntax extensions                        |
| camlp4/     | camlp4 syntax extensions                     |
| runtime/    | runtime system                               |
| toplevel/   | web-based OCaml toplevel                     |
| ocamlbuild/ | ocamlbuild plugin for js_of_ocaml            |
