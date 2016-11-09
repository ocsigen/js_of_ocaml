OCAMLC=ocamlfind ocamlc     -w +A-4-7-9-37-38-41-44-45
OCAMLOPT=ocamlfind ocamlopt -w +A-4-7-9-37-38-41-44-45

include ../Makefile.conf
-include ../Makefile.local

ifeq "${WITH_CAMLP4}" "YES"
ifeq "${WITH_DERIVING}" "YES"
PA_DERIVING=pa_deriving_Json.cmo pa_deriving_Json.cmi
ifeq "$(BEST)" "opt"
ifeq "${NATDYNLINK}" "YES"
PA_DERIVING_NDL= pa_deriving_Json.cmx pa_deriving_Json.cmxs
endif
endif
endif
endif


PA_JS=
ifeq "${WITH_CAMLP4}" "YES"
PA_JS= pa_js.cmo
ifeq "${BEST}" "opt"
ifeq "${NATDYNLINK}" "YES"
PA_JS_NDL= pa_js.cmx pa_js.cmxs
endif
endif
endif


all: 	${PA_JS} ${PA_JS_NDL} ${PA_DERIVING} ${PA_DERIVING_NDL}


pa_js.cmo: pa_js.ml
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

VERSION := $(shell head -n 1 ../VERSION)

install:
	ocamlfind install -patch-version ${VERSION} js_of_ocaml-camlp4 META ${PA_JS} ${PA_JS_NDL} ${PA_DERIVING} ${PA_DERIVING_NDL}

clean:
	rm -f *.cm[xioa] *.[ao] *.so *.cmx[sa]
