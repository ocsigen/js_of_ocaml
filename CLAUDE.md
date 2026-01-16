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
  - `js/` - JavaScript runtime modules
  - `wasm/` - WebAssembly runtime modules

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
- OCaml 4.13 to 5.4
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

Test directories:
- `compiler/tests-jsoo/` - JavaScript output tests
- `compiler/tests-wasm_of_ocaml/` - WebAssembly tests
- `compiler/tests-compiler/` - Compiler unit tests
- `compiler/tests-ocaml/` - OCaml compatibility tests
- `compiler/tests-full/` - Full integration tests

## Key Compiler Flags

- `--effects={cps,double-translation}` - Effect handler support
- `--enable es6` - ES6 output mode
- `--compilation_mode separate` - Separate compilation

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

## Contributing

1. Create branch from `master`
2. Write tests for new code
3. Run `make tests` before submitting
4. Update `CHANGES.md` with entry
5. Discuss significant changes via issues first
