# ECMAScript and Node requirements

The goal of the document is to list features we rely on for the runtime and the generated code.
Features are grouped by ECMAScript version.

## ECMAScript 2015

### Arrow function expressions

- [Compatibility](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Functions/Arrow_functions#browser_compatibility)
- For Wasm_of_ocaml

### Classes

- [Compatibility](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Classes#browser_compatibility)

### const

- [Compatibility](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/const#browser_compatibility)

### for...of

- [Compatibility](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/for...of#browser_compatibility)

### let

- [Compatibility](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/let#browser_compatibility)

### Map

- [Compatibility](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Map#browser_compatibility)
- Used in runtime for data structures and object tracking

### Object.assign

- [Compatibility](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Object/assign#browser_compatibility)

### Rest parameters

- [Compatibility](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Functions/rest_parameters#browser_compatibility)

### Set

- [Compatibility](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Set#browser_compatibility)

### Spread syntax (in function call)

- [Compatibility](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Spread_syntax#browser_compatibility)

### TypedArray

- [Compatibility](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray#browser_compatibility)
- To implement bigarray

### WeakMap

- [Compatibility](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/WeakMap#browser_compatibility)

### DataView

- [Compatibility](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/DataView#browser_compatibility)

## ECMAScript 2016

### async function

- [Compatibility](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/async_function#browser_compatibility)
- For Wasm_of_ocaml

### await

- [Compatibility](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/await#browser_compatibility)
- For Wasm_of_ocaml

## ECMAScript 2020

### BigInt

- [Compatibility](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/BigInt#browser_compatibility)
- For Wasm_of_ocaml

### globalThis

- [Compatibility](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/globalThis#browser_compatibility)
- Polyfilled in the repository

### Optional chaining (?.)

- [Compatibility](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Optional_chaining#browser_compatibility)

## ECMAScript 2021

### FinalizationRegistry

- [Compatibility](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/FinalizationRegistry#browser_compatibility)
- Optional for js_of_ocaml

### WeakRef

- [Compatibility](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/WeakRef#browser_compatibility)
- To implement weak and ephemeron
- Optional

## Web APIs

### Canvas API

- [Compatibility](https://developer.mozilla.org/en-US/docs/Web/API/Canvas_API#browser_compatibility)

### Crypto API

- [Compatibility](https://developer.mozilla.org/en-US/docs/Web/API/Crypto#browser_compatibility)

### Document API

- [Compatibility](https://developer.mozilla.org/en-US/docs/Web/API/Document#browser_compatibility)

### Image API

- [Compatibility](https://developer.mozilla.org/en-US/docs/Web/API/HTMLImageElement#browser_compatibility)

### Performance API

- [Compatibility](https://developer.mozilla.org/en-US/docs/Web/API/Performance#browser_compatibility)

### XMLHttpRequest

- [Compatibility](https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest#browser_compatibility)

## Node.js

### Built-in modules with `node:` prefix

- [Compatibility](https://nodejs.org/docs/latest/api/modules.html#built-in-modules)

### Child process

- [Compatibility](https://nodejs.org/docs/latest/api/child_process.html)

### File system

- [Compatibility](https://nodejs.org/docs/latest/api/fs.html)

### Process

- [Compatibility](https://nodejs.org/docs/latest/api/process.html)

### TTY

- [Compatibility](https://nodejs.org/docs/latest/api/tty.html)

### Util

- [Compatibility](https://nodejs.org/docs/latest/api/util.html)
