include ../Makefile.conf
-include ../Makefile.local

ifeq "${WITH_TOPLEVEL}${WITH_FINDLIB}" "YESYES"
ALL= $(MKTOP) $(MKCMIS)
endif

PACKAGES=base64 compiler-libs.common findlib yojson

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
jsoo_common.cmo jsoo_common.cmx: ../compiler/lib/js_of_ocaml_compiler.cmi

OCAMLC=ocamlfind ocamlc     -w +A-4-7-9-37-38-41-44-45-58
OCAMLOPT=ocamlfind ocamlopt -w +A-4-7-9-37-38-41-44-45-58

%.byte: ../compiler/lib/js_of_ocaml_compiler.cma ../compiler/findlib_support/jsoo_findlib_support.cma jsoo_common.cmo %.cmo
	$(OCAMLC) -linkpkg -linkall -g $(SAFESTRING) \
	${addprefix -package , $(PACKAGES)} \
	$^ -o $@

%.opt: ../compiler/lib/js_of_ocaml_compiler.cmxa ../compiler/findlib_support/jsoo_findlib_support.cmxa jsoo_common.cmx %.cmx
	$(OCAMLOPT) -linkpkg -linkall -g $(SAFESTRING) \
	${addprefix -package , $(PACKAGES)} \
	$^ -o $@

%.cmo: %.ml
	$(OCAMLC) -c -g $(SAFESTRING) \
	${addprefix -package , $(PACKAGES)} \
	-I ../compiler/lib/ $< -o $@

%.cmx: %.ml
	$(OCAMLOPT) -c -g $(SAFESTRING) \
	${addprefix -package , $(PACKAGES)} \
	-I ../compiler/lib/ $< -o $@

clean:
	rm -f *.cm[xioa] *.[ao] *.so *.cmx[sa] *.byte *.opt $(MKTOP) $(MKCMIS)
