opam update

opam pin add --no-action js_of_ocaml .
opam pin add --no-action js_of_ocaml-core .
opam pin add --no-action js_of_ocaml-compiler .
opam pin add --no-action js_of_ocaml-ocamlbuild .
opam pin add --no-action js_of_ocaml-camlp4 .
opam pin add --no-action js_of_ocaml-ppx .
opam pin add --no-action js_of_ocaml-lwt .
opam pin add --no-action js_of_ocaml-tyxml .
opam pin add --no-action js_of_ocaml-toplevel .

opam install lwt reactiveData tyxml || true
opam install camlp4 deriving || true
opam install ppx_tools ppx_deriving || true

opam pin add lwt https://github.com/ocsigen/lwt.git#pull/322/head

opam install --deps-only \
     js_of_ocaml \
     js_of_ocaml-lwt \
     js_of_ocaml-compiler \
     js_of_ocaml-ocamlbuild \
     js_of_ocaml-camlp4 \
     js_of_ocaml-toplevel \
     js_of_ocaml-ppx \
     js_of_ocaml-tyxml

opam install --verbose js_of_ocaml-compiler
opam install --verbose js_of_ocaml-ocamlbuild
opam install --verbose js_of_ocaml-camlp4
opam install --verbose js_of_ocaml
opam install --verbose js_of_ocaml-core
opam install --verbose js_of_ocaml-ppx
opam install --verbose js_of_ocaml-lwt
opam install --verbose js_of_ocaml-tyxml
opam install --verbose js_of_ocaml-toplevel

do_build_doc () {
  opam install ocp-indent higlo base64
  make clean
  make all
  make -C doc doc
  make -C doc wikidoc
  cp -Rf doc/manual/files/* ${MANUAL_FILES_DIR}/
  cp -Rf doc/manual/src/* ${MANUAL_SRC_DIR}/
  cp -Rf doc/api/wiki/*.wiki ${API_DIR}/
}

do_remove () {
    opam remove --verbose \
         js_of_ocaml \
         js_of_ocaml-core \
         js_of_ocaml-compiler \
         js_of_ocaml-ocamlbuild \
         js_of_ocaml-camlp4 \
         js_of_ocaml-ppx \
         js_of_ocaml-toplevel \
         js_of_ocaml-lwt \
         js_of_ocaml-tyxml
}
