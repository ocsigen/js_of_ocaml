# ECMAScript and Node requirements

The goal of the document is to list features we rely on for the runtime and the generated code.
Features are grouped by ECMAScript version.

## ECMAScript 2015

### Classes

- [Compatibility](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Classes#browser_compatibility)

### Rest parameters

- [Compatibility](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Functions/rest_parameters#browser_compatibility)

### Spread syntax (in function call)

- [Compatibility](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Spread_syntax#browser_compatibility)

### Object.assign

- [Compatibility](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Object/assign#browser_compatibility)

### Arrow function expressions

- [Compatibility](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Functions/Arrow_functions#browser_compatibility)
- For Wasm_of_ocaml

### TypedArray

- [Compatibility](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray#browser_compatibility)
- To implement bigarray

### Map

- [Compatibility](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Map#browser_compatibility)

## ECMAScript 2016

### async function

- [Compatibility](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/async_function#browser_compatibility)
- For Wasm_of_ocaml

### await

- [Compatibility](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/await#browser_compatibility)
- For Wasm_of_ocaml

## ECMAScript 2020

### globalThis

- [Compatibility](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/globalThis#browser_compatibility)
- Polyfilled in the repository

### BigInt

- [Compatibility](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/BigInt#browser_compatibility)
- For Wasm_of_ocaml

## ECMAScript 2021

### WeakRef

- [Compatibility](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/WeakRef#browser_compatibility)
- To implement weak and ephemeron
- Optional

## Web APIs

### XMLHttpRequest

- [Compatibility](https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest#browser_compatibility)

## Node.js

### Built-in modules with `node:` prefix

- [Compatibility](https://nodejs.org/docs/latest/api/modules.html#built-in-modules)
