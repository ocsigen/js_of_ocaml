include ../Makefile.conf
include ../Makefile.filelist

OCAMLC=ocamlfind ocamlc     -w +A-3-4-7-9-37-38-41-44-45
OCAMLOPT=ocamlfind ocamlopt -w +A-3-4-7-9-37-38-41-44-45-58
VERSION := $(shell head -n 1 ../VERSION)

all: $(CAMLP4_INSTALL)

pa_js.cmo pa_js.cmi: pa_js.ml
	$(OCAMLC) -w -42-58 $(SAFESTRING) -package camlp4.extend,camlp4.quotations -syntax camlp4o -c $<

pa_js.cmx: pa_js.ml
	$(OCAMLOPT) -w -42-58 $(SAFESTRING) -package camlp4.extend,camlp4.quotations -syntax camlp4o -c $<

pa_js.cmxs: pa_js.cmx
	$(OCAMLOPT) -shared -linkall -o $@ $<

pa_deriving_Json.cmo: pa_deriving_Json.ml pa_deriving_Json.cmi
	$(OCAMLC) -w -27 $(SAFESTRING) -package deriving.syntax.common,camlp4.quotations.o -syntax camlp4o -c $<

pa_deriving_Json.cmi: pa_deriving_Json.mli
	$(OCAMLC) $(SAFESTRING) -package deriving.syntax.common,camlp4.quotations.o -syntax camlp4o -c $<

pa_deriving_Json.cmx: pa_deriving_Json.ml
	$(OCAMLOPT) -w -27 $(SAFESTRING) -package deriving.syntax.common,camlp4.quotations.o -syntax camlp4o -c $<

pa_deriving_Json.cmxs: pa_deriving_Json.cmx
	$(OCAMLOPT) -shared -linkall -o $@ $<

clean:
	rm -f *.cm[xioa] *.[ao] *.so *.cmx[sa]
