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
jsoo_common.cmo jsoo_common.cmx: ../compiler/lib/compiler.cmi

OCAMLC=ocamlfind ocamlc     -w +A-4-7-9-37-38-41-44-45
OCAMLOPT=ocamlfind ocamlopt -w +A-4-7-9-37-38-41-44-45

%.byte: ../compiler/lib/compiler.cma jsoo_common.cmo %.cmo
	$(OCAMLC) -linkpkg -g $(SAFESTRING) \
	-package base64,compiler-libs.common,findlib \
	$^ -o $*.byte

%.opt: ../compiler/lib/compiler.cmxa jsoo_common.cmx %.cmx
	$(OCAMLOPT) -linkpkg -g $(SAFESTRING) \
	-package base64,compiler-libs.common,findlib \
	$^ -o $*.opt

%.cmo: %.ml
	$(OCAMLC) -c -g $(SAFESTRING) \
	-package compiler-libs.common,findlib \
	-I ../compiler/lib/ $<

%.cmx: %.ml
	$(OCAMLOPT) -c -g $(SAFESTRING) \
	-package compiler-libs.common,findlib \
	-I ../compiler/lib/ $<

clean:
	rm -f *.cm[xioa] *.[ao] *.so *.cmx[sa] *.byte *.opt $(MKTOP) $(MKCMIS)
