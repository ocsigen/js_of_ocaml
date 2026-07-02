# Contributing to Js_of_ocaml

## Reporting Issues

If you encounter a problem when using js_of_ocaml or if you have any questions, please open a [GitHub issue](https://github.com/ocsigen/js_of_ocaml/issues/).

1. Check first if your issue has already been [reported](https://github.com/ocsigen/js_of_ocaml/issues/).
2. Include the version of OCaml and js_of_ocaml you are using (`ocamlc -version`, `js_of_ocaml --version`).
3. Describe the expected and actual behavior.
4. Do not unsubscribe from the issue until it is closed, the maintainers may ask for your feedback.

## Pull Requests

We actively welcome pull requests.

1. Prior to investing a large amount of time into significant or invasive changes, it is likely more efficient to first open an issue for discussion and planning.
2. Install all dependencies (see [Install dependencies](#install-dependencies))
3. Fork the repository and create your branch from `master`.
4. If you have added code that should be tested, add tests.
5. Ensure tests still pass (see [Running the tests](#running-the-tests)).
6. Add an entry in the changelog `CHANGES.md`

### Install dependencies
```
opam install --deps-only -t js_of_ocaml js_of_ocaml-lwt js_of_ocaml-compiler js_of_ocaml-toplevel js_of_ocaml-ppx js_of_ocaml-ppx_deriving_json js_of_ocaml-tyxml
opam install odoc yojson ocp-indent graphics higlo
```

### Running the tests

Run `make tests`.

### Documentation examples

Code examples in `.mli` doc-comments and the `manual/` pages are checked by the
test suite: an `{@ocaml[ ... ]}` block is type-checked against the real API, so
examples cannot silently drift out of sync. Mark an illustrative snippet that is
not meant to compile with `{@ocaml parse[ ... ]}` (syntax-checked only) or
`{@ocaml skip[ ... ]}` (ignored). See `manual/examples-check/` for details.

## Library binding conventions

When adding new bindings under `lib/js_of_ocaml/`, follow these rules so
that the role of an underscore in a method name is unambiguous.

### Underscores in method names

- **Leading `_`** — pure name mangling. Use it when the JavaScript
  identifier is an OCaml keyword (`type`, `method`, `open`, `match`,
  `end`, `as`, `effect`, `class`, `for`, `assert`, …) or starts with an
  uppercase letter (which OCaml method names cannot). The `ppx_js_internal`
  preprocessor strips the leading underscore in the generated JS, so
  `method _type` reads `.type` on the JS side. Examples: `_method`,
  `_type`, `_open`, `_URL`, `_PI`, `_HORIZONTAL_AXIS`.

- **Trailing `_`** — *not* mangling. Reserved for an argument-overload
  variant: same JS method, different OCaml signature (e.g. extra
  optional flag, different result type). Existing examples: `getInt16_`
  / `setInt16_` (DataView, take a little-endian boolean), `bindBuffer_`
  / `bindFramebuffer_` / `bindTexture_` (WebGL, accept an `opt`
  binding).

Older code mixes these roles (`open_`, `type_`, `method_`, `assert_`,
`effect_` use trailing `_` for keyword escape rather than leading);
those are grandfathered. New code should follow the split above.

### Multi-underscore names

The mangler strips at most one leading `_` and then drops the last `_`
and everything after it. So a name with more than one non-leading
underscore resolves to something shorter than you might expect:

| OCaml method            | Calls JS identifier |
| ----------------------- | ------------------- |
| `abort_with_reason`     | `abort_with`        |
| `_BYTES_PER_ELEMENT`    | `BYTES_PER`         |
| `toDataURL_type_quality`| `toDataURL_type`    |

If the underlying JS identifier itself contains underscores (e.g. a
WebGL constant like `MAX_RENDERBUFFER_SIZE`), terminate the OCaml name
with a trailing `_` to anchor the rindex on the very end:
`_MAX_RENDERBUFFER_SIZE_`. If you want a single overload disambiguator
on a method like `toDataURL`, give the OCaml name exactly one trailing
`_<suffix>` (e.g. `toDataURL_quality`, not `toDataURL_type_quality`).
