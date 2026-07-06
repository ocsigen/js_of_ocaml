# Example projects

Build all projects:
```
$> dune build @examples/default
```

Or a single one:
```
$> dune build @examples/<path-to-example-project>/default
```

Set the environment variable WASM_OF_OCAML=true to compile to Wasm:
```
$> WASM_OF_OCAML=true dune build @examples/boulderdash/default
```

Compilation artifacts can be found in `${REPO_ROOT}/_build/default/examples/`.

When generating JavaScript code, you can usually directly open the
`index.html` files in a browser. When generating Wasm code, or for
examples fetching resources at runtime (e.g. `webgl` loading its
model), you need to serve the files, for instance with the following
command:
```
python -m http.server -d _build/default/examples/boulderdash/
```
and then open `http://localhost:8000/index.html` in a browser
(`http://localhost:8000/index.html?wasm` for the Wasm version).
