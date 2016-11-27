include ../../Makefile.conf
include ../../Makefile.filelist

OCAMLC=ocamlfind ocamlc     -w +A-3-4-7-9-37-38-41-44-45
OCAMLOPT=ocamlfind ocamlopt -w +A-3-4-7-9-37-38-41-44-45-58
VERSION := $(shell head -n 1 ../../VERSION)


all: $(CAMLP4_INSTALL)

lib/pa_js.cmo lib/pa_js.cmi: lib/pa_js.ml
	$(OCAMLC) -w -42-58 $(SAFESTRING) -package camlp4.extend,camlp4.quotations -syntax camlp4o -c $<

lib/pa_js.cmx: lib/pa_js.ml
	$(OCAMLOPT) -w -42-58 $(SAFESTRING) -package camlp4.extend,camlp4.quotations -syntax camlp4o -c $<

lib/pa_js.cmxs: lib/pa_js.cmx
	$(OCAMLOPT) -shared -linkall -o $@ $<

clean:
	rm -f *.cm[xioa] *.[ao] *.so *.cmx[sa]

tests/%.result: tests/%.ml tests/init.ml
	(TERM=dumb ocaml -init tests/init.ml < $< 2>&1) | grep -v version > $@
	patdiff tests/$*.expected $@ || true
	rm $@

tests: tests/meth.result tests/prop.result tests/write_prop.result tests/object_litteral.result
