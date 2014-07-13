opam pin add --no-action js_of_ocaml .
opam install deriving tyxml reactiveData
opam install --deps-only js_of_ocaml
opam install --verbose js_of_ocaml
opam remove --verbose js_of_ocaml
