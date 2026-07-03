
# Contributing to Js\_of\_ocaml


## Reporting issues

If you encounter a problem or have questions, please open a [GitHub issue](https://github.com/ocsigen/js_of_ocaml/issues/).

1. Check if your issue has already been [reported](https://github.com/ocsigen/js_of_ocaml/issues/)
2. Include version information (`ocamlc -version`, `js_of_ocaml --version`)
3. Describe the expected and actual behavior
4. Provide a minimal reproducible example if possible

## Development setup


### Install dependencies

```
opam install --deps-only -t js_of_ocaml js_of_ocaml-lwt \
  js_of_ocaml-compiler js_of_ocaml-toplevel js_of_ocaml-ppx \
  js_of_ocaml-ppx_deriving_json js_of_ocaml-tyxml
```

### Build

```
dune build @all
```

## Pull requests

1. Discuss significant changes in an issue first
2. Fork the repository and create your branch from `master`
3. Write tests for new code
4. Ensure tests pass (`make tests`)
5. Update `CHANGES.md` with an entry describing your change

## Code style


### OCaml

Code is formatted with **ocamlformat 0\.29.0**. Run before committing:

```
make fmt
```

### JavaScript

JavaScript runtime files are formatted and linted with **Biome**:

```
make fmt-js
make lint-js
```

## Documentation

Code examples in the `.mli` doc-comments and the `manual/` pages are checked by the test suite: OCaml code blocks are type-checked against the real API, so examples cannot silently drift out of sync. An illustrative snippet that is not meant to compile can be marked to be only syntax-checked, or skipped entirely. See `manual/examples-check/` for the markers.


## Testing

```
# Run all tests
make tests

# Run WebAssembly tests
make tests-wasm

# Run a specific test directory
dune build @compiler/tests-jsoo/runtest
dune build @compiler/tests-ocaml/runtest-js
WASM_OF_OCAML=true dune build @compiler/tests-ocaml/runtest-wasm

# Run javascript parser tests
cd compiler/tests-js-parser
git clone https://github.com/tc39/test262.git test262
dune build @runtest-parser

# Update expected test output after intentional changes
dune promote
```
Test directories:

- `compiler/tests-jsoo/` \- JavaScript and WebAssembly tests
- `compiler/tests-wasm_of_ocaml/` \- JavaScript and WebAssembly tests
- `compiler/tests-compiler/` \- Compiler unit tests
- `compiler/tests-ocaml/` \- Tests from the OCaml compiler codebase
- `compiler/tests-full/` \- Full integration tests

## Project structure

- `compiler/` \- Compiler implementation
  
  - `lib/` \- Core compiler library
  - `lib-wasm/` \- WebAssembly backend
  - `bin-js_of_ocaml/` \- js\_of\_ocaml entry point
  - `bin-wasm_of_ocaml/` \- wasm\_of\_ocaml entry point
- `lib/` \- Library packages (js\_of\_ocaml, lwt, tyxml bindings)
- `ppx/` \- PPX syntax extensions
- `runtime/` \- JavaScript and WebAssembly runtime files
- `toplevel/` \- Web-based OCaml toplevel
- `examples/` \- Example projects

## See also

- [GitHub repository](https://github.com/ocsigen/js_of_ocaml)
- [Issue tracker](https://github.com/ocsigen/js_of_ocaml/issues)
- [Pull requests](https://github.com/ocsigen/js_of_ocaml/pulls)