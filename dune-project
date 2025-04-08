(lang dune 3.17)
(using menhir 3.0)
(name js_of_ocaml)
(generate_opam_files true)
(subst disabled)
(executables_implicit_empty_intf true)

(authors "Ocsigen team <dev@ocsigen.org>")
(maintainers "Ocsigen team <dev@ocsigen.org>")
(source (github ocsigen/js_of_ocaml))
(homepage "https://ocsigen.org/js_of_ocaml/latest/manual/overview")
(documentation "https://ocsigen.org/js_of_ocaml/latest/manual/overview")
(license "GPL-2.0-or-later" "LGPL-2.1-or-later WITH OCaml-LGPL-linking-exception")

(package
 (name js_of_ocaml-compiler)
 (synopsis "Compiler from OCaml bytecode to JavaScript")
 (description
  "Js_of_ocaml is a compiler from OCaml bytecode to JavaScript. It makes it possible to run pure OCaml programs in JavaScript environment like browsers and Node.js")
 (depends
  (ocaml (and (>= 4.13) (< 5.4)))
  (num :with-test)
  (ppx_expect (and (>= v0.16.1) :with-test))
  (ppxlib (>= 0.15))
  (re :with-test)
  (cmdliner (>= 1.1.0))
  (sedlex (>= 3.3))
  (qcheck :with-test)
  menhir
  menhirLib
  menhirSdk
  (yojson (>= 2.1)))
 (depopts
  ocamlfind)
 (conflicts
  (ocamlfind (< 1.5.1))
  (js_of_ocaml (< 3.0))
))

(package
 (name js_of_ocaml-lwt)
 (synopsis "Compiler from OCaml bytecode to JavaScript")
 (description
  "Js_of_ocaml is a compiler from OCaml bytecode to JavaScript. It makes it possible to run pure OCaml programs in JavaScript environment like browsers and Node.js")
 (depends
  (ocaml (>= 4.13))
  (js_of_ocaml (= :version))
  (js_of_ocaml-ppx (= :version))
  (lwt (>= 2.4.4))
  (num :with-test)
  (ppx_expect (and (>= v0.14.2) :with-test))
  (ppxlib (and (>= 0.22.0) :with-test))
  (re (and (>= 1.9.0) :with-test)))
 (depopts
  graphics
  lwt_log
))

(package
 (name js_of_ocaml-ppx_deriving_json)
 (synopsis "Compiler from OCaml bytecode to JavaScript")
 (description
  "Js_of_ocaml is a compiler from OCaml bytecode to JavaScript. It makes it possible to run pure OCaml programs in JavaScript environment like browsers and Node.js")
 (depends
  (ocaml (>= 4.13))
  (js_of_ocaml (= :version))
  (ppxlib (>= 0.15))
  (num :with-test)
  (ppx_expect (and (>= v0.14.2) :with-test))
  (re (and (>= 1.9.0) :with-test))
))

(package
 (name js_of_ocaml-ppx)
 (synopsis "Compiler from OCaml bytecode to JavaScript")
 (description
  "Js_of_ocaml is a compiler from OCaml bytecode to JavaScript. It makes it possible to run pure OCaml programs in JavaScript environment like browsers and Node.js")
 (depends
  (ocaml (>= 4.13))
  (js_of_ocaml (= :version))
  (ppxlib (>= 0.15))
  (ppxlib (and (< 0.36) :with-test))
  (num :with-test)
  (ppx_expect (and (>= v0.14.2) :with-test))
  (re (and (>= 1.9.0) :with-test))
))

(package
 (name js_of_ocaml-toplevel)
 (synopsis "Compiler from OCaml bytecode to JavaScript")
 (description
  "Js_of_ocaml is a compiler from OCaml bytecode to JavaScript. It makes it possible to run pure OCaml programs in JavaScript environment like browsers and Node.js")
 (depends
  (ocaml (>= 4.13))
  (js_of_ocaml-compiler (= :version))
  (ocamlfind (>= 1.5.1))
;;(cohttp-lwt-unix (and (>= 6.0.0) :with-test))
  (graphics :with-test)
  (num :with-test)
  (ppx_expect (and (>= v0.14.2) :with-test))
  (ppxlib (>= 0.15))
  (re (and (>= 1.9.0) :with-test))
))

(package
 (name js_of_ocaml-tyxml)
 (synopsis "Compiler from OCaml bytecode to JavaScript")
 (description
  "Js_of_ocaml is a compiler from OCaml bytecode to JavaScript. It makes it possible to run pure OCaml programs in JavaScript environment like browsers and Node.js")
 (depends
  (ocaml (>= 4.13))
  (js_of_ocaml (= :version))
  (js_of_ocaml-ppx (= :version))
  (react (>= 1.2.2))
  (reactiveData (>= 0.2))
  (tyxml (>= 4.6))
  (num :with-test)
  (ppx_expect (and (>= v0.14.2) :with-test))
  (ppxlib (and (>= 0.22.0) :with-test))
  (re (and (>= 1.9.0) :with-test))
))

(package
 (name js_of_ocaml)
 (synopsis "Compiler from OCaml bytecode to JavaScript")
 (description
  "Js_of_ocaml is a compiler from OCaml bytecode to JavaScript. It makes it possible to run pure OCaml programs in JavaScript environment like browsers and Node.js")
 (depends
  (ocaml (>= 4.13))
  (js_of_ocaml-compiler (= :version))
  (ppxlib (>= 0.15))
  (num :with-test)
  (ppx_expect (and (>= v0.14.2) :with-test))
  (re (and (>= 1.9.0) :with-test))
))

(package
 (name wasm_of_ocaml-compiler)
 (synopsis "Compiler from OCaml bytecode to WebAssembly")
 (description
  "Wasm_of_ocaml is a compiler from OCaml bytecode to WebAssembly. It makes it possible to run pure OCaml programs in JavaScript environment like browsers and Node.js")
 (depends
  (ocaml (>= 4.14))
  (js_of_ocaml (= :version))
  (num :with-test)
  (ppx_expect (and (>= v0.14.2) :with-test))
  (ppxlib (>= 0.15))
  (re :with-test)
  (cmdliner (>= 1.1.0))
  (opam-format :with-test)
  (sedlex (>= 2.3))
  menhir
  menhirLib
  menhirSdk
  (yojson (>= 2.1))
  binaryen-bin)
 (depopts
  ocamlfind)
 (conflicts
  (ocamlfind (< 1.5.1))
  (js_of_ocaml (< 3.0))
))
