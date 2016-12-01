include ../../Makefile.conf
include ../../Makefile.filelist

OCAMLC=ocamlfind ocamlc     -w +A-4-7-9-37-38-41-44-45
OCAMLOPT=ocamlfind ocamlopt -w +A-4-7-9-37-38-41-44-45

all: $(PPX_DERIVING_INSTALL)

lib/ppx_deriving_json.cmo: lib/ppx_deriving_json.ml
	$(OCAMLC) -w -40-42-48-27 -package ppx_deriving,ppx_tools,ppx_tools.metaquot -c $<

lib/ppx_deriving_json.cma: lib/ppx_deriving_json.cmo
	$(OCAMLC) -a -o $@ $^

lib/ppx_deriving_json.cmx: lib/ppx_deriving_json.ml
	$(OCAMLOPT) -w -40-42-48-27 -package ppx_deriving,ppx_tools.metaquot -c $<

lib/ppx_deriving_json.a: lib/ppx_deriving_json.cmx

lib/ppx_deriving_json.cmxa: lib/ppx_deriving_json.cmx
	$(OCAMLOPT) -a -o $@ $^

lib/ppx_deriving_json.cmxs: lib/ppx_deriving_json.cmx
	$(OCAMLOPT) -shared -o $@ $^

tests/expect: tests/main.ml
	ocamlfind ocamlc -verbose -linkpkg -linkall -predicates create_toploop,ppx_driver \
	-package toplevel_expect_test,ppx_type_conv,js_of_ocaml.deriving.ppx,js_of_ocaml.deriving  \
	tests/main.ml -o $@

tests: tests/expect tests/ppx.mlt
	./tests/expect tests/ppx.mlt
