include ../Makefile.conf
-include ../Makefile.local

ifeq "${WITH_TOPLEVEL}" "YES"
ALL= $(MKTOP) $(MKCMIS)
endif

all: $(ALL)

$(MKTOP): jsoo_mktop.$(BEST)
	mv -f $< $@
$(MKCMIS): jsoo_mkcmis.$(BEST)
	mv -f $< $@

byte: jsoo_mkcmis.byte jsoo_mktop.byte
opt: jsoo_mkcmis.opt jsoo_mktop.opt

%.cmo: %.ml
%.cmx: %.ml
jsoo_mk%.cmo: jsoo_common.cmo
jsoo_mk%.cmx: jsoo_common.cmx

%.byte: ../compiler/local/compiler.cma jsoo_common.cmo%.cmo
	ocamlfind ocamlc -linkpkg -g \
	-package compiler-libs.common,findlib \
	$^ -o $*.byte

%.opt: ../compiler/local/compiler.cmxa jsoo_common.cmx %.cmx
	ocamlfind ocamlopt -linkpkg -g \
	-package compiler-libs.common,findlib \
	$^ -o $*.opt

%.cmo: %.ml
	ocamlfind ocamlc -c -g \
	-package compiler-libs.common,findlib \
	-I ../compiler/local/ $<

%.cmx: %.ml
	ocamlfind ocamlopt -c -g \
	-package compiler-libs.common,findlib \
	-I ../compiler/local/ $<

../compiler/local/compiler.cma:
	$(MAKE) -C ../compiler local/compiler.cma
../compiler/local/compiler.cmxa: ../compiler/local/compiler.cma

clean:
	rm -f *.cm[xioa] *.[ao] *.so *.cmx[sa] *.{byte,opt}
