# ECMAScript and Node requirements

The goal of the document is to list features we rely on for the runtime and the generated code.
Features are grouped by ECMAScript version.

## ECMAScript 2015 - ES6
### rest parameters
- added in Nodejs 6
### spread in function call
- added in Nodejs 5
### Object.assign
- added In Nodejs 4
### ArrowFunction
- for wasm_of_ocaml
### TypedArray
- to implement bigarray

## ECMAScript 2016
### Async/Await
- for wasm_of_ocaml

## ECMAScript 2020
### globalThis
- added in Nodejs 12
- polyfill-ed in the repo

## ECMAScript 2021
### WeakRef to implement weak and ephemeron
- added in Nodejs 14.6
- optional

## Node
### Namespaced require `require("node:fs")`
- added in Nodejs 16