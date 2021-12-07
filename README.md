# Js_of_ocaml (jsoo)

[![Build Status](https://github.com/ocsigen/js_of_ocaml/workflows/build/badge.svg?branch=master)](https://github.com/ocsigen/js_of_ocaml/actions)

Js_of_ocaml is a compiler from OCaml bytecode to JavaScript. It makes it
possible to run pure OCaml programs in JavaScript environment like browsers and
Node.js.

- It is easy to install and use as it works with an existing installation of
  OCaml, with no need to recompile any library.
- It comes with bindings for a large part of the browser APIs.
- According to our benchmarks, the generated programs runs typically faster than
  with the OCaml bytecode interpreter.
- We believe this compiler will prove much easier to maintain than a retargeted
  OCaml compiler, as the bytecode provides a very stable API.

## Requirements

See
[opam](https://github.com/ocsigen/js_of_ocaml/blob/master/js_of_ocaml-compiler.opam)
file for version constraints.

### optional

- [lwt](https://github.com/ocsigen/lwt)
- [tyxml](https://github.com/ocsigen/tyxml)
- [reactiveData](https://github.com/ocsigen/reactiveData)
- [yojson](https://github.com/mjambon/yojson)

### Toplevel requirements

- tyxml, reactiveData
- ocp-indent: needed to support indentation in the toplevel
- higlo: needed to support Syntax highlighting in the toplevel
- cohttp: needed to build the toplevel webserver

## Installation

### Opam

```
opam install js_of_ocaml js_of_ocaml-compiler js_of_ocaml-ppx
```

## Usage

Your program must first be compiled using the OCaml bytecode compiler `ocamlc`.
JavaScript bindings are provided by the `js_of_ocaml` package. The syntax
extension is provided by `js_of_ocaml.syntax` package.

```
ocamlfind ocamlc -package js_of_ocaml -package js_of_ocaml-ppx -linkpkg -o cubes.byte cubes.ml
```

Then, run the `js_of_ocaml` compiler to produce JavaScript code:

```
js_of_ocaml cubes.byte
```

## Features

Most of the OCaml standard library is supported. However,

- Weak semantic cannot be implemented using JavaScript. A dummy implementation
  is provided.
- Most of the Sys module is not supported.

Extra libraries distributed with OCaml (such as Thread) are not supported in
general. However,

- Bigarray: bigarrays are supported using Typed Arrays
- Num: supported
- Str: supported
- Graphics: partially supported using canvas (see js_of_ocaml-lwt.graphics)
- Unix: time related functions are supported

Tail call is not optimized in general. However, mutually recursive functions are
optimized:

- self recursive functions (when the tail calls are the function itself) are
  compiled using a loop.
- trampolines are used otherwise.
  [More](http://ocsigen.org/js_of_ocaml/dev/manual/tailcall) about tail call
  optimization.

## Data representation

Data representation differs from the usual one. Most notably, integers are 32
bits (rather than 31 bits or 63 bits), which is their natural size in
JavaScript, and floats are not boxed. As a consequence, marshalling, polymorphic
comparison, and hashing functions can yield results different from usual:

- marshalling of floats is not supported (unmarshalling works);
- the polymorphic hash function will not give the same results on datastructures
  containing floats;
- these functions may be more prone to stack overflow.

| Ocaml | javascript |
| ------------- | ------------- |
| int   | number (32bit int)  |
| int32 | number (32bit int)  |
| nativeint | number (32bit int)  |
| int64 | Object (MlInt64) |
| float | number |
| string | string or object (MlBytes) |
| bytes | object (MlBytes) |
| "immediate" (e.g. true, false, None, ()) | number (32bit int) |
| "block" | array with tag as first element (e.g. `C(1,2) => [tag,1,2]`) |
| array | block with tag 0 (e.g. `[\|1;2\|] => [0,1,2]`) |
| tuple | block with tag 0 (e.g. `(1,2) => [0,1,2]`) |
| record | block (e.g. `{x=1;y=2} => [0,1,2]`) |
| contructor with arguments | block (e.g. `C (1, 2) => [tag,1,2]`) |
| module | block |
| exception and extensible variant | block or immediate |
| function | function |



## Toplevel

- [OCaml 4.04.2](http://ocsigen.github.io/js_of_ocaml/toplevel.html#version=4.04.2)
  includes Base, Core_kernel, Async_kernel, Async_js
- [OCaml 4.04.0+BER](http://ocsigen.github.io/js_of_ocaml/toplevel.html#version=4.04.0+BER)
  see http://okmij.org/ftp/ML/MetaOCaml.html
- [OCaml 4.05.0](http://ocsigen.github.io/js_of_ocaml/toplevel.html#version=4.05.0)
  includes Base, Core_kernel, Async_kernel, Async_js
- [OCaml 4.06.0](http://ocsigen.github.io/js_of_ocaml/toplevel.html#version=4.06.0)
  includes Base, Core_kernel, Async_kernel, Async_js

## Contents of the distribution

| Filename    | Description                                  |
| ----------- | -------------------------------------------- |
| LICENSE     | license and copyright notice                 |
| README      | this file                                    |
| compiler/   | compiler                                     |
| examples/   | small examples                               |
| lib/        | library for interfacing with JavaScript APIs |
| ppx/        | ppx syntax extensions                        |
| runtime/    | runtime system                               |
| toplevel/   | web-based OCaml toplevel                     |
| ocamlbuild/ | ocamlbuild plugin for js_of_ocaml            |
