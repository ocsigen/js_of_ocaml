
include ../../Makefile.conf

PACKAGES=

ifeq "${WITH_FINDLIB}" "YES"
PACKAGES+= findlib
FINDLIB_SUPPORT=jsoo_findlib_support.cma \
                jsoo_findlib_support.cmx \
                jsoo_findlib_support.cmxa \
                jsoo_findlib_support.cmxs
endif

all: $(FINDLIB_SUPPORT)

../lib/js_of_ocaml_compiler.cmo:
	$(MAKE) -C ../lib/ js_of_ocaml_compiler.cmo

../lib/js_of_ocaml_compiler.cmx:
	$(MAKE) -C ../lib/ js_of_ocaml_compiler.cmx

OCAMLC=ocamlfind ocamlc -g  -w +A-4-7-9-37-38-41-44-45-58
OCAMLOPT=ocamlfind ocamlopt -g -w +A-4-7-9-37-38-41-44-45-58

jsoo_findlib_support.cmo: ../lib/js_of_ocaml_compiler.cmo
jsoo_findlib_support.cmx: ../lib/js_of_ocaml_compiler.cmx

jsoo_findlib_support.cma: jsoo_findlib_support.cmo
	$(OCAMLC) -I ../ -linkall -a -o $@ $^

jsoo_findlib_support.cmxa: jsoo_findlib_support.cmx
	$(OCAMLOPT) -linkall -a -o $@ $^

jsoo_findlib_support.cmxs: jsoo_findlib_support.cmxa
	$(OCAMLOPT) -linkall -shared -o $@ $^

%.cmx: %.ml
	$(OCAMLOPT) -I ../lib $(SAFESTRING) ${addprefix -package , $(PACKAGES)} -c $<

%.cmo: %.ml
	$(OCAMLC) -I ../lib $(SAFESTRING) ${addprefix -package , $(PACKAGES)} -c $<

%.cmi: %.mli
	$(OCAMLC) -I ../lib $(SAFESTRING) ${addprefix -package , $(PACKAGES)} -c $<

clean:
	rm -f *.cm[aiox] *.cmxa *.cmxs *.o *.a *.conflicts
	rm -f lib/*.cm[aiox] lib/*.cmxa lib/*.cmxs lib/*.o lib/*.a

.PHONY: depend
depend: jsoo_findlib_support.ml
	echo $^ | xargs -n1 | sort -u | xargs ocamldep > .depend

include .depend
