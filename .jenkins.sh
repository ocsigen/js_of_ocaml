
opam pin add --no-action js_of_ocaml .
opam pin add --no-action tyxml 'https://github.com/ocsigen/tyxml.git#master'
opam install deriving tyxml reactiveData
opam install --deps-only js_of_ocaml
opam install --verbose js_of_ocaml

do_build_doc () {
  opam install ocp-indent.1.4.1 optcomp
  make -C doc clean
  make -C doc doc
  make -C doc wikidoc
  cp -Rf doc/manual/files/* ${MANUAL_FILES_DIR}/
  cp -Rf doc/manual/src/* ${MANUAL_SRC_DIR}/
  cp -Rf doc/api/wiki/*.wiki ${API_DIR}/
}

do_remove () {
  opam remove --verbose js_of_ocaml
}
