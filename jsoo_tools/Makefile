include ../Makefile.conf
-include ../Makefile.local

ifeq "${WITH_TOPLEVEL}${WITH_FINDLIB}" "YESYES"
ALL= $(MKTOP) $(MKCMIS)
endif

all: $(ALL)

$(MKTOP): jsoo_mktop.$(BEST)
	cp $< $@
$(MKCMIS): jsoo_mkcmis.$(BEST)
	cp $< $@

byte: jsoo_mkcmis.byte jsoo_mktop.byte
opt: jsoo_mkcmis.opt jsoo_mktop.opt

%.cmo: %.ml
%.cmx: %.ml
jsoo_mktop.cmo jsoo_mkcmis.cmo: jsoo_common.cmo
jsoo_mktop.cmx jsoo_mkcmis.cmx: jsoo_common.cmx
jsoo_common.cmo jsoo_common.cmx: ../compiler/compiler.cmi

%.byte: ../compiler/compiler.cma jsoo_common.cmo %.cmo
	ocamlfind ocamlc -linkpkg -g $(SAFESTRING) \
	-package compiler-libs.common,findlib \
	$^ -o $*.byte

%.opt: ../compiler/compiler.cmxa jsoo_common.cmx %.cmx
	ocamlfind ocamlopt -linkpkg -g $(SAFESTRING) \
	-package compiler-libs.common,findlib \
	$^ -o $*.opt

%.cmo: %.ml
	ocamlfind ocamlc -c -g $(SAFESTRING) \
	-package compiler-libs.common,findlib \
	-I ../compiler/ $<

%.cmx: %.ml
	ocamlfind ocamlopt -c -g $(SAFESTRING) \
	-package compiler-libs.common,findlib \
	-I ../compiler/ $<

clean:
	rm -f *.cm[xioa] *.[ao] *.so *.cmx[sa] *.byte *.opt $(MKTOP) $(MKCMIS)
