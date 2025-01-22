# Testsuite from the OCaml compiler distribution

This directory contains a subset of the tests available in
https://github.com/ocaml/ocaml under the testsuite/tests/
directory.

The tests have been updated to accommodate wasoo and jsoo but we should
aim at keeping diffs as small as possible to simplify (re-)synchronisation.

- `*.reference` files in the ocaml repo are renamed to `*.expected` here.
- expect test are not suppot yet.

## Tooling
`tools/sync_testsuite.exe` is a small tool that helps visualizing the diff between the ocaml testsuite and this directory.

`dune exe tools/sync_testsuite.exe -- <PATH_TO_OCAML_REPO>/testsuite/tests/ compiler/tests-ocaml/`