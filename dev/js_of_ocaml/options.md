
# Command-line options

This page documents the command-line options for the `js_of_ocaml` compiler. Most of these options also apply to `wasm_of_ocaml`. Differences are noted where applicable.


## General options

| --- | --- |
| Option | Description |
| `--version` | Display the compiler version |
| `-o <file>` | Set output filename |
| `--opt {1,2,3}` | Set optimization profile (default: 1\). See [Optimizations](#optimizations) |
| `--enable <opt>` | Enable option. See [Enable/Disable options](#enable-disable) |
| `--disable <opt>` | Disable option. See [Enable/Disable options](#enable-disable) |
| `-I <dir>` | Add directory to the list of include directories |

## Debugging

| --- | --- |
| Option | Description |
| `--pretty` | Pretty print JavaScript output, preserve OCaml variable names |
| `--no-inline` | Disable code inlining |
| `--debug-info` | Output debug information (source locations) |
| `--source-map` | Generate source map. See [Source maps](#source-maps) |

## Target environment

| --- | --- |
| Option | Description |
| `--target-env <env>` | Build for target environment (default: `isomorphic`) |
Possible values:

- **`isomorphic`** \- Runs in both browser and Node.js (default)
- **`browser`** \- Browser-specific, smaller output
- **`nodejs`** \- Node.js-specific, smaller output

## Effect handlers

| --- | --- |
| Option | Description |
| `--effects <mode>` | Select effect handler implementation |
For **js\_of\_ocaml** (default: `disabled`):

- **`disabled`** \- No effect handler support (default)
- **`cps`** \- CPS transformation for effect support
- **`double-translation`** \- Keep both direct and CPS versions; choose at runtime
For **wasm\_of\_ocaml** (default: `jspi`):

- **`jspi`** \- JavaScript-Promise Integration (default). Available in Chrome 137, Node.js 25, and higher. Use `cps` for other browsers. Performing effects is slower than with `cps`.
- **`cps`** \- CPS transformation for effect support
- **`native`** \- WebAssembly stack switching (wasm\_of\_ocaml only). Available, behind the `--experimental-wasm-wasmfx` flag, in Chrome 148 or higher, or in a recent Node.js canary release (V8 version 14\.7.100 or higher).
- **`disabled`** \- No effect handler support
See [Effect handlers](./effects.md) for details.


## Source maps

| --- | --- |
| Option | Description |
| `--source-map` | Generate source map (separate file) |
| `--no-source-map` | Don't generate source map |
| `--source-map-inline` | Inline source map in the JavaScript output |
| `--source-map-no-source` | Don't embed source code in source map |
| `--source-map-root <dir>` | Set root directory for source map paths |

## Pseudo filesystem

Js\_of\_ocaml provides a pseudo filesystem for programs that read files at runtime. The `--file` flag is also supported by wasm\_of\_ocaml.

| --- | --- |
| Option | Description |
| `--file <file>[:<target>]` | Register file to pseudo filesystem (default target: `/static/`) |
| `--ofs <file>` | Write filesystem data to separate file (js\_of\_ocaml only) |
| `--extern-fs` | Allow registering files from outside at runtime (default, js\_of\_ocaml only) |
| `--no-extern-fs` | Only allow files embedded at compile time (js\_of\_ocaml only) |

## Environment variables

| --- | --- |
| Option | Description |
| `--setenv <VAR>=<value>` | Set environment variable at compile time |

### Resolution order

When OCaml code calls `Sys.getenv`, js\_of\_ocaml resolves variables in this order:

1. If the variable was set at compile time with `--setenv VAR=value`, return it
2. If running in Node.js, check `process.env`
3. Check `globalThis.jsoo_env` (useful for setting variables in browsers)
4. Raise `Not_found`

### Browser example

```javascript
// Set environment variables before loading the OCaml program
globalThis.jsoo_env = {
  DEBUG: "true",
  API_URL: "https://api.example.com"
};
```

## Toplevel and dynamic linking

| --- | --- |
| Option | Description |
| `--toplevel` | Compile an OCaml toplevel (embeds necessary .cmi files) |
| `--no-cmis` | Don't include .cmi files in toplevel |
| `--dynlink` | Enable dynamic linking of bytecode |
| `--export <file>` | File listing units to export for toplevel/dynlink |
| `--linkall` | Link all compilation units and primitives |
See [Building a toplevel](./build-toplevel.md) for details.


## Compilation modes

| --- | --- |
| Option | Description |
| `--noruntime` | Don't include the standard runtime |
| `--include-runtime` | Include partial runtime when compiling .cmo/.cma |
| `--keep-unit-names` | Preserve unit names (for separate compilation) |
| `--wrap-with-fun[=<name>]` | Wrap output in a function (default: IIFE) |
| `--custom-header <text>` | Add custom header (e.g., `#!/usr/bin/env node`) |
See [Compilation modes](./compilation-modes.md) for details.


## Optimizations


### Optimization profiles

Use `--opt <level>` to select a profile:

- **`--opt 1`** \- Basic optimizations (default)
- **`--opt 2`** \- Standard optimizations
- **`--opt 3`** \- Maximum optimizations (iterates until fix-point)

### Recommended settings

For debugging:

```
js_of_ocaml --pretty --no-inline program.byte
```
For production:

```
js_of_ocaml --opt 3 program.byte
```

## Enable/Disable options

Use `--enable <opt>` or `--disable <opt>` to control these flags:


### Output formatting

| --- | --- | --- |
| Option | Default | Description |
| `pretty` | false | Pretty print JavaScript output |
| `shortvar` | true | Use short variable names |
| `strict` | true | Generate "use strict" |
| `header` | true | Include compiler version header |
| `es6` | false | Generate ES6 syntax |

### Optimizations

| --- | --- | --- |
| Option | Default | Description |
| `deadcode` | true | Dead code elimination |
| `globaldeadcode` | true | Global dead code elimination |
| `inline` | true | Function inlining |
| `staticeval` | true | Static evaluation of constants |
| `share` | true | Share string and number constants |
| `optcall` | true | JavaScript call optimizations |
| `constant-sinking` | true | Move constant declarations closer to usage sites |
| `var-coalescing` | true | Coalesce JavaScript variables with disjoint lifespans |

### Runtime behavior

| --- | --- | --- |
| Option | Default | Description |
| `effects` | false | Enable effect handlers |
| `excwrap` | true | Wrap JavaScript exceptions in OCaml exceptions |
| `with-js-error` | false | Attach JavaScript stack traces to OCaml exceptions |
| `debugger` | true | Keep `debugger` statements (stripped if false) |
| `genprim` | true | Generate dummy primitives when missing |
| `use-js-string` | true | Use JavaScript strings internally |

### Internal debugging

| --- | --- | --- |
| Option | Default | Description |
| `debuginfo` | false | Include source location comments |
| `stable_var` | false | Use stable (predictable) variable names |
| `auto-link` | true | link required compilation units only |

## Compiler parameters

Use `--set <param>=<value>` to tune compiler behavior:

| --- | --- | --- |
| Parameter | Default | Description |
| `switch_size` | 60 | Max cases before switch becomes if-else chain |
| `inlining-limit` | 150 | Size threshold for function inlining |
| `tc_depth` | 50 | Max tail-call depth before trampoline |
| `tc` | trampoline | Tail-call mode: `trampoline` or `none` |

## Warnings

| --- | --- |
| Option | Description |
| `--quiet` / `-q` | Suppress non-error messages |
| `--Werror` | Treat warnings as errors |
| `-w <warning>` | Enable warning |
| `-w no-<warning>` | Disable warning |
Available warnings:

| --- | --- | --- |
| Warning | Default | Description |
| `missing-primitive` | on | Missing JavaScript primitive implementation |
| `missing-cmi` | on | Missing .cmi file |
| `missing-deps` | on | Missing dependencies |
| `missing-effects-backend` | on | Using effects, but not backend was selected |
| `integer-overflow` | on | Integer overflow in constant |
| `overriding-primitive` | on | Overriding an existing primitive |
| `deprecated-primitive` | on | Using deprecated primitive |
| `free-variables` | on | Free variables in primitive code |

## Debug output

Use `--debug <section>` to enable debug output for compiler internals:

`times`, `stats`, `deadcode`, `globaldeadcode`, `inlining`, `constant-sinking`, `gen`, `linker`, `sourcemap`, `flow`, `parser`, `var-coalescing`


## Subcommands

The `js_of_ocaml` command has several subcommands:

| --- | --- |
| Command | Description |
| `js_of_ocaml compile` | Compile bytecode to JavaScript (default) |
| `js_of_ocaml link` | Link JavaScript files together |
| `js_of_ocaml build-runtime` | Build standalone runtime |
| `js_of_ocaml build-fs` | Build pseudo filesystem |
The `wasm_of_ocaml` command has similar subcommands:

| --- | --- |
| Command | Description |
| `wasm_of_ocaml compile` | Compile bytecode to WebAssembly (default) |
| `wasm_of_ocaml link` | Link files together |
| `wasm_of_ocaml build-runtime` | Build standalone runtime |

### Link command

```
js_of_ocaml link -o output.js runtime.js lib1.js lib2.js main.js
```
Additional options:

| --- | --- |
| Option | Description |
| `-a` | Build a library (.cma.js) |
| `--resolve-sourcemap-url` | Resolve source map URLs |

### Build-runtime command

```
js_of_ocaml build-runtime -o runtime.js
```
Accepts the same `--enable`/`--disable` options as the main command.


### Build-fs command

```
js_of_ocaml build-fs -o fs.js file1.txt file2.dat
```
Builds a JavaScript file containing filesystem data.
