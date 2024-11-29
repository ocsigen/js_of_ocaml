# Example projects

Build all projects:
```
$> dune build @examples/default
```

Or a single one:
```
$> dune build @examples/<path-to-example-project>/default
```

Add the `--profile wasm` option to compile to Wasm:
```
$> dune build @examples/boulderdash/default --profile wasm
```

Compilation artifacts can be found in `${REPO_ROOT}/_build/default/examples/`.

When generating JavaScript code, you can directly open the
`index.html` files in a browser. When generating Wasm code, you need
to serve the files, for instance with the following command:
```
python -m http.server -d _build/default/examples/boulderdash/
```
and then open `http://localhost:8000` in a browser.
