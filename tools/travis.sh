set -x

# Install system packages.
wget https://raw.githubusercontent.com/ocaml/ocaml-ci-scripts/master/.travis-ocaml.sh
bash -ex .travis-ocaml.sh

eval $(opam env)
opam update
opam upgrade --yes || opam upgrade --yes --fixup

ACTUAL_COMPILER=`ocamlc -version`
if [ "$ACTUAL_COMPILER" != "$OCAML_VERSION" ]
then
    echo Expected OCaml $OCAML_VERSION, but $ACTUAL_COMPILER is installed
fi

PACKAGES="js_of_ocaml \
        js_of_ocaml-lwt \
        js_of_ocaml-compiler \
        js_of_ocaml-ocamlbuild \
        js_of_ocaml-toplevel \
        js_of_ocaml-ppx \
        js_of_ocaml-ppx_deriving_json \
        js_of_ocaml-tyxml"

case $MODE in
    opam)
        # Pin Js_of_ocaml, install dependencies, and then install Js_of_ocaml.
        opam pin add --no-action -y js_of_ocaml.dev -k path .
        opam pin add --no-action -y js_of_ocaml-compiler.dev -k path .
        opam pin add --no-action -y js_of_ocaml-ocamlbuild.dev -k path .
        opam pin add --no-action -y js_of_ocaml-ppx.dev -k path .
        opam pin add --no-action -y js_of_ocaml-ppx_deriving_json.dev -k path .
        opam pin add --no-action -y js_of_ocaml-lwt.dev -k path .
        opam pin add --no-action -y js_of_ocaml-tyxml.dev -k path .
        opam pin add --no-action -y js_of_ocaml-toplevel.dev -k path .

        opam install -y lwt reactiveData tyxml graphics || true
        opam install -y ppxlib || true

        opam install -y --best-effort $PACKAGES
        opam upgrade --yes
        ;;
    build)
        # Pin Js_of_ocaml, install dependencies.
        opam pin add --no-action -y js_of_ocaml.dev -k path .
        opam pin add --no-action -y js_of_ocaml-compiler.dev -k path .
        opam pin add --no-action -y js_of_ocaml-ocamlbuild.dev -k path .
        opam pin add --no-action -y js_of_ocaml-ppx.dev -k path .
        opam pin add --no-action -y js_of_ocaml-ppx_deriving_json.dev -k path .
        opam pin add --no-action -y js_of_ocaml-lwt.dev -k path .
        opam pin add --no-action -y js_of_ocaml-tyxml.dev -k path .
        opam pin add --no-action -y js_of_ocaml-toplevel.dev -k path .

        opam pin add --no-action -y num https://github.com/ocaml/num.git#master

        opam install -y --best-effort --deps-only $PACKAGES || true
        opam install -y cohttp-lwt-unix menhir ppx_expect yojson sexplib graphics odoc
        opam upgrade --yes
        dune build @runtest @default @ocsigen-doc -j 8
        opam install -y ocamlformat.0.13.0
        make fmt
        ;;
esac
