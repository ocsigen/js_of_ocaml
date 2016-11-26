

all: test test_deriving

expect: main.ml
	ocamlfind ocamlc -verbose -linkpkg -linkall -predicates create_toploop,ppx_driver -package toplevel_expect_test,js_of_ocaml.ppx.driver main.ml -o expect

test: expect ppx.mlt
	./expect ppx.mlt

expect_deriving: main_deriving.ml
	ocamlfind ocamlc -verbose -linkpkg -linkall -predicates create_toploop,ppx_driver -package toplevel_expect_test,ppx_type_conv,js_of_ocaml.deriving.ppx,js_of_ocaml.deriving  main_deriving.ml -o expect_deriving

test_deriving: expect_deriving ppx_deriving.mlt
	./expect_deriving ppx_deriving.mlt
