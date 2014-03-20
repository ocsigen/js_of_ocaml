# Js_of_ocaml

Js_of_ocaml is a compiler from OCaml bytecode to Javascript. It makes OCaml programs run on Web browsers.

  * It is easy to install as it works with an existing installation of OCaml,
with no need to recompile any library.
  * It comes with bindings for a large part of the browser APIs.
  * According to our benchmarks, the generated programs runs typically faster than with the OCaml bytecode
interpreter.
  * We believe this compiler will prove much easier to maintain than a retargeted OCaml compiler,
as the bytecode provides a very stable API.

## Requirements

  * Findlib
  * Lwt: **version 2.3.0** at least
  * Menhir

###optionnal
  * [deriving](https://github.com/ocsigen/deriving): **version 0.6** at least
  * ocp-indent: needed to build the toplevel
  * cohttp: needed to build the toplevel webserver

## Installation

###Opam
```
opam install deriving js_of_ocaml
```

###Manual
  * edit `Makefile.conf` to change the default configuration
  * run `make` to compile
  * run `make install` as root to install the compiler
    and its libraries
  * run `make uninstall` as root to uninstall them

You can run `make toplevel` if you want to build a Web-based OCaml
toplevel as well. [Try the toplevel](http://ocsigen.github.io/js_of_ocaml/)

## Usage

Your program must first be compiled using the OCaml bytecode compiler
`ocamlc`.  Javascript bindings are provided by the `js_of_ocaml` package.
The syntax extension is provided by `js_of_ocaml.syntax` package.

```
ocamlfind ocamlc -package js_of_ocaml -syntax camlp4o -package js_of_ocaml.syntax -linkpkg -o cubes.byte cubes.ml
```

Then, run the `js_of_ocaml` compiler to produce Javascript code:

```
js_of_ocaml cubes.byte
```

## Features

Most of the OCaml standard library is supported. However,
  * Weak semantic cannot be implemented using javascript.
    A dummy implemtation is available (use `+weak.js` option)
  * Most of Sys module is not supported.

Extra libraries distributed with Ocaml (such as Thread or Str) are not
supported in general. However,
  * Bigarray: 1-dimensional bigarray are supported using Typed Arrays
  * Unix: time related functions are supported

Tail call is not optimized in general. However, mutually recursive
functions are optimized.
  * self recursive functions (when the tail calls are the function itself) are
    compiled using a loop.
  * trampolines are used otherwise.

Data representation differs from the usual one, for performance
reasons.  Most notably, integers are 32 bits (rather than 31 bits or
63 bits) and floats are not boxed.  As a consequence, marshaling,
polymorphic comparison, and hashing functions can yield results
different from usual:
  * the polymorphic hash function will not give the same results on
    datastructures containing floats;
  * these functions may be more prone to stack overflow, as the
    Javascript stack is small.

## Toplevel
  * [Ocaml 4.01.0](http://ocsigen.github.io/js_of_ocaml/#4.01.0)
  * [Ocaml 4.01.0+open-types](http://ocsigen.github.io/js_of_ocaml/#4.01.0+open-types)
  * [Ocaml 4.01.0+BER](http://ocsigen.github.io/js_of_ocaml/#4.01.0+open-types) : metaocaml
  * [Ocaml 4.02.0dev+trunk](http://ocsigen.github.io/js_of_ocaml/#4.02.0dev+trunk)

## Contents of the distribution
| Filename  | Description                                  |
|-----------|----------------------------------------------|
| LICENSE   | license and copyright notice                 |
| README    | this file                                    |
| compiler/ | compiler                                     |
| examples/ | small examples                               |
| lib/      | library for interfacing with Javascript APIs |
| runtime/  | runtime system                               |
| toplevel/ | web-based OCaml toplevel                     |
