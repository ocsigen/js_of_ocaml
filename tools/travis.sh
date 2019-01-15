set -x

# Install system packages.
wget https://raw.githubusercontent.com/ocaml/ocaml-travisci-skeleton/master/.travis-ocaml.sh
bash -ex .travis-ocaml.sh

eval $(opam env)

ACTUAL_COMPILER=`ocamlc -version`
if [ "$ACTUAL_COMPILER" != "$OCAML_VERSION" ]
then
    echo Expected OCaml $OCAML_VERSION, but $ACTUAL_COMPILER is installed
fi


# Pin Js_of_ocaml, install dependencies, and then install Js_of_ocaml.
# Js_of_ocaml is installed
# separately because we want to keep the build directory for running the tests.
opam pin add --no-action -y js_of_ocaml.dev .
opam pin add --no-action -y js_of_ocaml-compiler.dev .
opam pin add --no-action -y js_of_ocaml-ocamlbuild.dev .
opam pin add --no-action -y js_of_ocaml-ppx.dev .
opam pin add --no-action -y js_of_ocaml-ppx_deriving_json.dev .
opam pin add --no-action -y js_of_ocaml-lwt.dev .
opam pin add --no-action -y js_of_ocaml-tyxml.dev .
opam pin add --no-action -y js_of_ocaml-toplevel.dev .

opam install -y lwt reactiveData tyxml || true
opam install -y ppx_tools ppx_deriving || true

opam install -y --deps-only \
     js_of_ocaml \
     js_of_ocaml-lwt \
     js_of_ocaml-compiler \
     js_of_ocaml-ocamlbuild \
     js_of_ocaml-toplevel \
     js_of_ocaml-ppx \
     js_of_ocaml-ppx_deriving_json \
     js_of_ocaml-tyxml

opam install --keep-build-dir --verbose js_of_ocaml-compiler
opam install --keep-build-dir --verbose js_of_ocaml-ocamlbuild
opam install --keep-build-dir --verbose js_of_ocaml
opam install --keep-build-dir --verbose js_of_ocaml-ppx
opam install --keep-build-dir --verbose js_of_ocaml-ppx_deriving_json
opam install --keep-build-dir --verbose js_of_ocaml-lwt
opam install --keep-build-dir --verbose js_of_ocaml-tyxml
opam install --keep-build-dir --verbose js_of_ocaml-toplevel

dune build @install -j 8 --ignore-promoted-rules

opam install -y base64 cohttp-lwt-unix menhir

dune build @runtest @default -j 8
