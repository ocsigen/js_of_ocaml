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

opam install -y \
     js_of_ocaml \
     js_of_ocaml-lwt \
     js_of_ocaml-compiler \
     js_of_ocaml-ocamlbuild \
     js_of_ocaml-toplevel \
     js_of_ocaml-ppx \
     js_of_ocaml-ppx_deriving_json \
     js_of_ocaml-tyxml

opam install -y menhir

dune build @install -j 8 --ignore-promoted-rules

if [ "$ALL_TARGETS" == "true" ]
then
opam install -y base64 cohttp-lwt-unix
dune build @runtest @default -j 8
fi
