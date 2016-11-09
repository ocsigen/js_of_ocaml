OCAMLC=ocamlfind ocamlc     -w +A-4-7-9-37-38-41-44-45
OCAMLOPT=ocamlfind ocamlopt -w +A-4-7-9-37-38-41-44-45

VERSION := $(shell head -n 1 ../../VERSION)

include ../../Makefile.conf
-include ../../Makefile.local

ifeq "${WITH_TOPLEVEL}" "YES"
TOPLEVELLIB= jsooTop.cmo jsooTopError.cmo
TOPLEVELLIB+= jsooTopPpx.cmo
ifeq ($(shell ocamlc -version | grep -q -E "4.0[^01]"; echo $$?),0)
TOPLEVELLIB+= jsooTopCamlp4.cmo
endif
endif

all: $(TOPLEVELLIB)

../../lib/ppx/ppx_js.byte:
	$(MAKE) -C ../../lib/ ppx/ppx_js.byte

../../compiler/lib/js_of_ocaml_compiler.cma:
	$(MAKE) -C ../../compiler/lib/ js_of_ocaml_compiler.cma

jsooTop.cmo: jsooTop.ml ../../lib/ppx/ppx_js.byte ../../compiler/lib/js_of_ocaml_compiler.cma
	$(OCAMLC) \
		$(SAFESTRING) -package lwt,lwt.syntax \
		-ppx ../../lib/ppx/ppx_js.byte \
		-I ../../compiler/lib -I ../../lib -I +compiler-libs \
		-c $< -o $@

%.cmo: %.ml
	$(OCAMLC) $(SAFESTRING) \
		-I +compiler-libs -package camlp4 \
		-c $< -o $@

%.cmi: %.mli
	$(OCAMLC) $(SAFESTRING) \
	-I +compiler-libs $< -o $@

clean:
	rm -f *.cm[xioa] *.[ao] *.so *.cmx[sa]
	rm -f jsooTopError.ml

depend: jsooTopError.ml
	ocamldep -ppx ../../lib/ppx/ppx_js.byte jsooTop.ml jsooTopCamlp4.ml *.mli > .depend
	ocamldep jsooTopError.ml jsooTopPpx.ml >> .depend

install:
	ocamlfind install -patch-version ${VERSION} js_of_ocaml-toplevel META $(TOPLEVELLIB)

include .depend
