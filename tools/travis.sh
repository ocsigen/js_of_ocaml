set -x



# Install system packages.
packages_apt () {
    case $COMPILER in
        4.02) PPA=avsm/ocaml42+opam12;;
        4.03) PPA=avsm/ocaml42+opam12; DO_SWITCH=yes;;
        4.04) PPA=avsm/ocaml42+opam12; DO_SWITCH=yes;;
        4.05) PPA=avsm/ocaml42+opam12; DO_SWITCH=yes;;
        4.06) PPA=avsm/ocaml42+opam12; DO_SWITCH=yes;;
           *) echo Unsupported compiler $COMPILER; exit 1;;
    esac

    sudo add-apt-repository -y ppa:$PPA
    sudo apt-get update -qq

    if [ -z "$DO_SWITCH" ]
    then
        sudo apt-get install -qq ocaml-nox
    fi

    sudo apt-get install -qq opam

}

packages_homebrew () {
    brew update > /dev/null

    if [ "$COMPILER" = system ]
    then
        brew install ocaml
        # The system compiler on Homebrew is now 4.06 or higher, and there is no
        # system Camlp4 package compatible with that (at least not yet). See:
        #   https://github.com/ocaml/opam-repository/pull/10455
        HAVE_CAMLP4=no
    else
        DO_SWITCH=yes
    fi

    brew install opam

}

packages_osx () {
    case $PACKAGE_MANAGER in
               *) packages_homebrew;;
    esac
}

packages () {
    case $TRAVIS_OS_NAME in
        linux) packages_apt;;
          osx) packages_osx;;
            *) echo Unsupported system $TRAVIS_OS_NAME; exit 1;;
    esac
}

packages



# Initialize OPAM and switch to the right compiler, if necessary.
case $COMPILER in
    4.02) OCAML_VERSION=4.02.3;;
    4.03) OCAML_VERSION=4.03.0;;
    4.04) OCAML_VERSION=4.04.2;;
    4.05) OCAML_VERSION=4.05.0;;
    4.06) OCAML_VERSION=4.06.0;;
    system) OCAML_VERSION=`ocamlc -version`;;
       *) echo Unsupported compiler $COMPILER; exit 1;;
esac

if [ "$FLAMBDA" = yes ]
then
    SWITCH="$OCAML_VERSION+flambda"
else
    SWITCH="$OCAML_VERSION"
fi

if [ -n "$DO_SWITCH" ]
then
    opam init --compiler=$SWITCH -y -a
else
    opam init -y -a
fi

eval `opam config env`

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
opam pin add --no-action -y js_of_ocaml-camlp4.dev .
opam pin add --no-action -y js_of_ocaml-ppx.dev .
opam pin add --no-action -y js_of_ocaml-ppx_deriving_json.dev .
opam pin add --no-action -y js_of_ocaml-lwt.dev .
opam pin add --no-action -y js_of_ocaml-tyxml.dev .
opam pin add --no-action -y js_of_ocaml-toplevel.dev .

opam install -y lwt reactiveData tyxml || true
opam install -y camlp4 deriving || true
opam install -y ppx_tools ppx_deriving || true

opam install -y --deps-only \
     js_of_ocaml \
     js_of_ocaml-lwt \
     js_of_ocaml-compiler \
     js_of_ocaml-ocamlbuild \
     js_of_ocaml-camlp4 \
     js_of_ocaml-toplevel \
     js_of_ocaml-ppx \
     js_of_ocaml-ppx_deriving_json \
     js_of_ocaml-tyxml

opam install --keep-build-dir --verbose js_of_ocaml-compiler
opam install --keep-build-dir --verbose js_of_ocaml-ocamlbuild
opam install --keep-build-dir --verbose js_of_ocaml-camlp4
opam install --keep-build-dir --verbose js_of_ocaml
opam install --keep-build-dir --verbose js_of_ocaml-ppx
opam install --keep-build-dir --verbose js_of_ocaml-ppx_deriving_json
opam install --keep-build-dir --verbose js_of_ocaml-lwt
opam install --keep-build-dir --verbose js_of_ocaml-tyxml
opam install --keep-build-dir --verbose js_of_ocaml-toplevel

# don't test pa_deriving_json and pa_js
rm -Rf camlp4/pa_js/tests
rm -Rf camlp4/pa_deriving_json/tests

make
make test
