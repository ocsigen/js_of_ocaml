(lang dune 3.20)
(using menhir 3.0)
(using directory-targets 0.1)
(using oxcaml 0.1)
(name js_of_ocaml)
(generate_opam_files true)
(executables_implicit_empty_intf true)

(authors "Ocsigen team <dev@ocsigen.org>")
(maintainers "Ocsigen team <dev@ocsigen.org>")
(source (github ocsigen/js_of_ocaml))
(homepage "https://ocsigen.org/js_of_ocaml/latest/js_of_ocaml/overview.html")
(documentation "https://ocsigen.org/js_of_ocaml/latest/js_of_ocaml/overview.html")
(license "GPL-2.0-or-later" "LGPL-2.1-or-later WITH OCaml-LGPL-linking-exception")

(package
 (name ppx_devtools-optcomp)
 (synopsis "Lightweight ppx for conditional compilation")
 (description
  "ppx_devtools-optcomp gates code at compile time with [@if] attributes, depending on the OCaml version, the operating system or the compiler flavour.")
 (license "LGPL-2.1-or-later WITH OCaml-LGPL-linking-exception")
 (depends
  (ocaml (>= 4.13))
  (ppxlib (>= 0.33))
  (num :with-test)
  (re (and (>= 1.9.0) :with-test))))

(package
 (name ppx_devtools-expect)
 (synopsis "Lightweight inline expect tests, with conditional snapshots")
 (description
  "ppx_devtools-expect is an inline expect-test ppx in the style of ppx_expect, with snapshots conditional on runtime axes such as the backend or the OCaml version. Tests run natively, with js_of_ocaml and with wasm_of_ocaml.")
 (license "LGPL-2.1-or-later WITH OCaml-LGPL-linking-exception")
 (depends
  (ocaml (>= 4.13))
  (ppxlib (>= 0.33))
  (ppx_devtools-optcomp (= :version))
  out-channel-redirect
  (num :with-test)
  (re (and (>= 1.9.0) :with-test))))

(package
 (name js_of_ocaml-compiler)
 (synopsis "Compiler from OCaml bytecode to JavaScript")
 (description
  "Js_of_ocaml is a compiler from OCaml bytecode to JavaScript. It makes it possible to run pure OCaml programs in JavaScript environment like browsers and Node.js")
 (depends
  (ocaml (and (>= 4.13) (< 5.7)))
  (num :with-test)
  ppx_devtools-optcomp
  (ppx_devtools-expect :with-test)
  (ppxlib (>= 0.33))
  (ocaml-compiler-libs (>= v0.12.4))
  (re :with-test)
  (cmdliner (>= 2.0))
  (sedlex (>= 3.8.1))
  (qcheck :with-test)
  (menhir (>= 20180523))
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
  (lwt (and (>= 2.4.4) (<> 5.9.2)))
  (num :with-test)
  (re (and (>= 1.9.0) :with-test)))
 (depopts
  graphics))

(package
 (name js_of_ocaml-ppx_deriving_json)
 (synopsis "Compiler from OCaml bytecode to JavaScript")
 (description
  "Js_of_ocaml is a compiler from OCaml bytecode to JavaScript. It makes it possible to run pure OCaml programs in JavaScript environment like browsers and Node.js")
 (depends
  (ocaml (>= 4.13))
  (js_of_ocaml (= :version))
  ppx_devtools-optcomp
  (ppx_devtools-expect :with-test)
  (ppxlib (>= 0.33))
  (num :with-test)
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
  ppx_devtools-optcomp
  (ppxlib (>= 0.33))
  (num :with-test)
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
  (js_of_ocaml (= :version))
  (ocamlfind (>= 1.5.1))
;;(cohttp-lwt-unix (and (>= 6.0.0) :with-test))
  (graphics :with-test)
  (num :with-test)
  ppx_devtools-optcomp
  (ppxlib (>= 0.33))
  (re (and (>= 1.9.0) :with-test)))
 (depopts
  lwt))

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
  (num :with-test)
  ppx_devtools-optcomp
  (ppx_devtools-expect :with-test)
  (ppxlib (>= 0.33))
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
  ppx_devtools-optcomp
  (ppx_devtools-expect :with-test)
  (ppxlib (>= 0.33))
  (re :with-test)
  (cmdliner (>= 2.0))
  (sedlex (>= 2.3))
  (menhir (>= 20180523))
  menhirLib
  menhirSdk
  (yojson (>= 2.1))
  conf-binaryen)
 (depopts
  ocamlfind
  opam-format)
 (conflicts
  (ocamlfind (< 1.5.1))
  (js_of_ocaml (< 3.0))
))
