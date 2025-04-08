# Contributing to Js_of_ocaml

## Reporting Issues

If you encounter a problem when using js_of_ocaml or if you have any questions, please open a [GitHub issue](https://github.com/ocsigen/js_of_ocaml/issues/).

1. Check first if your issue has already been [reported](https://github.com/ocsigen/js_of_ocaml/issues/).
2. Include the version of ocaml and js_of_ocaml you are using (`ocamlc -version`, `js_of_ocaml --version`).
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
opam install odoc lwt_log yojson ocp-indent graphics higlo
```

### Running the tests

Run `make tests`.
