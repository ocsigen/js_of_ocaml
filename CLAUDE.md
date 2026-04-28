# CLAUDE.md

This file provides guidance for Claude Code when working with the js_of_ocaml codebase.

## Project Overview

**Js_of_ocaml (jsoo)** is a compiler from OCaml bytecode to JavaScript, allowing pure OCaml programs to run in browsers and Node.js. It also includes **wasm_of_ocaml**, a compiler targeting WebAssembly.

- **Repository:** https://github.com/ocsigen/js_of_ocaml
- **Maintainers:** Ocsigen team
- **License:** GPL-2.0-or-later AND LGPL-2.1-or-later WITH OCaml-LGPL-linking-exception

## Build Commands

```bash
# Build everything
dune build @all

# Run all tests
make tests

# Run WebAssembly tests
make tests-wasm

# Format OCaml code
make fmt

# Format JavaScript code
make fmt-js

# Lint JavaScript
make lint-js

# Generate documentation
make doc

# Clean build artifacts
make clean

# Run benchmarks
make bench
```

## Project Structure

- **`/compiler/`** - Compiler implementation
  - `bin-js_of_ocaml/` - js_of_ocaml command entry point
  - `bin-wasm_of_ocaml/` - wasm_of_ocaml entry point
  - `bin-jsoo_minify/` - JavaScript minifier tool
  - `lib/` - Core compiler library
  - `lib-wasm/` - WebAssembly backend
  - `lib-cmdline/` - Command-line handling
  - `tests-*/` - Test suites (jsoo, wasm, compiler, effects, ocaml, full)

- **`/lib/`** - Library packages
  - `js_of_ocaml/` - Main JavaScript binding library
  - `lwt/` - Lwt async support
  - `tyxml/` - TyXML integration
  - `runtime/` - OCaml runtime for JavaScript

- **`/ppx/`** - PPX syntax extensions
  - `ppx_js/` - JavaScript PPX extension
  - `ppx_deriving_json/` - JSON derivation PPX

- **`/runtime/`** - JavaScript/WebAssembly runtime
  - `js/` - JavaScript runtime modules (`.js` files with `//Provides:` / `//Requires:` / `//If:` / `//Alias:` headers; `//Provides:` accepts flags `const`, `mutable`, `pure`, `shallow` that affect optimization)
  - `wasm/` - WebAssembly runtime modules (`.wat`); JS and Wasm runtimes are parallel — primitives in one usually need a counterpart in the other

- **`/examples/`** - Example projects
- **`/toplevel/`** - Web-based OCaml toplevel
- **`/benchmarks/`** - Performance benchmarks

## Development Setup

```bash
opam install --deps-only -t js_of_ocaml js_of_ocaml-lwt \
  js_of_ocaml-compiler js_of_ocaml-toplevel js_of_ocaml-ppx \
  js_of_ocaml-ppx_deriving_json js_of_ocaml-tyxml
opam install odoc lwt_log yojson ocp-indent graphics higlo
```

**Requirements:**
- OCaml 4.13 to 5.5
- Dune 3.19+
- For wasm_of_ocaml: Binaryen 119+

## Code Style

### OCaml
- Formatter: **ocamlformat 0.28.1** (config in `.ocamlformat`)
- Line margin: 90 characters
- All compiler files use `open! Stdlib` explicitly
- Comprehensive `.mli` module signatures

### JavaScript
- Formatter/Linter: **Biome** (config in `biome.json`)

### Conventions
- GPL-LGPL license header on all source files
- Heavy use of polymorphic variants and GADTs
- Tests use `ppx_expect` for inline snapshot testing and Cram tests (`.t` files)

## Testing

```bash
# Full test suite
make tests

# WebAssembly tests (requires WASM_OF_OCAML=true)
make tests-wasm

# Run specific test
dune runtest compiler/tests-jsoo

# Accept test output changes
dune promote
```

`make tests` requires a recent Node.js to be available on `PATH`.

Test directories:
- `compiler/tests-jsoo/` - JavaScript output tests
- `compiler/tests-wasm_of_ocaml/` - WebAssembly tests
- `compiler/tests-compiler/` - Compiler unit tests
- `compiler/tests-ocaml/` - OCaml compatibility tests
- `compiler/tests-full/` - Full integration tests

## Key Compiler Flags

- `--effects={disabled,cps,double-translation}` - Effect handler support (JS); wasm accepts `{disabled,cps,jspi}`
- `--target-env={isomorphic,browser,nodejs}` - Runtime target (default `isomorphic`)
- `--source-map` / `--debug-info` / `--pretty` - Debug-friendly output
- `--opt {1,2,3}` - Optimization profile (default 1; 3 iterates to fix-point)
- `--enable=OPT` / `--disable=OPT` - Toggle individual options (names in `compiler/lib/config.ml`), e.g. `--enable es6` to emit ES6 syntax in generated code
- `--toplevel` - Compile a toplevel (link against `js_of_ocaml-toplevel`)

### Compilation modes

The `js_of_ocaml` CLI has subcommands `compile` (default), `build-runtime`, and `link`. Whole-program compilation runs `compile` end-to-end; separate compilation builds the runtime, compiles each `.cmo`/`.cma` independently, then links the pieces. Dune picks whole-program for `--profile release` and separate for `dev`. See `manual/compilation-modes.wiki`.

## Architecture Notes

The compiler works by:
1. Reading OCaml bytecode (`.cmo`/`.cma` files)
2. Converting to an intermediate representation
3. Optimizing and transforming
4. Generating JavaScript or WebAssembly output

Key modules in `compiler/lib/`:
- `parse_bytecode.ml` - Bytecode parsing
- `code.ml` - Internal code representation
- `generate.ml` - JavaScript code generation
- `driver.ml` - Compilation driver

Wasm backend lives in `compiler/lib-wasm/` (e.g. `generate.ml`, `code_generation.ml`, `link.ml`).

## Contributing

1. Create branch from `master`
2. Write tests for new code
3. Run `make tests` before submitting
4. Add a `CHANGES.md` entry under the `# dev` heading, in an appropriate subsection (`Features/Changes`, `Bug fixes`, etc.) with a `(#NNNN)` PR reference
5. Discuss significant changes via issues first
