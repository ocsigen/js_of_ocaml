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


        opam install -y --best-effort $PACKAGES
        ;;
    build)
        opam install -y --deps-only $PACKAGES
        opam install -y cohttp-lwt-unix menhir
        dune build @runtest @default @ocsigen-doc -j 8
        ;;
esac
