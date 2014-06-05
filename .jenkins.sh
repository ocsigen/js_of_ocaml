opam pin js_of_ocaml .
opam pin tyxml https://github.com/ocsigen/tyxml.git
opam install tyxml react deriving
opam install --deps-only js_of_ocaml
opam install --verbose js_of_ocaml
opam remove --verbose js_of_ocaml
