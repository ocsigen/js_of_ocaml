include ../../Makefile.conf
include ../../Makefile.filelist

OCAMLC=ocamlfind ocamlc     -w +A-3-4-7-9-37-38-41-44-45
OCAMLOPT=ocamlfind ocamlopt -w +A-3-4-7-9-37-38-41-44-45-58
VERSION := $(shell head -n 1 ../../VERSION)

lib/pa_deriving_Json.cmo: lib/pa_deriving_Json.ml lib/pa_deriving_Json.cmi
	$(OCAMLC) -w -27 $(SAFESTRING) -I lib/ -package deriving.syntax.common,camlp4.quotations.o -syntax camlp4o -c $<

lib/pa_deriving_Json.cmi: lib/pa_deriving_Json.mli
	$(OCAMLC) $(SAFESTRING) -package deriving.syntax.common,camlp4.quotations.o -syntax camlp4o -c $<

lib/pa_deriving_Json.cmx: lib/pa_deriving_Json.ml lib/pa_deriving_Json.cmi
	$(OCAMLOPT) -w -27 $(SAFESTRING) -I lib/ -package deriving.syntax.common,camlp4.quotations.o -syntax camlp4o -c $<

lib/pa_deriving_Json.cmxs: lib/pa_deriving_Json.cmx
	$(OCAMLOPT) -shared -linkall -o $@ $<

clean:
	rm -f *.cm[xioa] *.[ao] *.so *.cmx[sa]

tests/%.result: tests/%.ml tests/init.ml
	(TERM=dumb ocaml -init tests/init.ml < $< 2>&1) | grep -v version > $@
	patdiff tests/$*.expected $@ || true
	rm $@

tests: tests/deriving.result
