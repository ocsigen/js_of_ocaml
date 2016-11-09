include ../Makefile.conf
include ../Makefile.filelist

PACKAGES=compiler-libs.common cmdliner base64 yojson

ifeq "${WITH_FINDLIB}" "YES"
PACKAGES+= findlib
FINDLIB_SUPPORT=$(addprefix findlib_support/, $(FINDLIB_INSTALL))
FINDLIB_SUPPORT_CMA=findlib_support/jsoo_findlib_support.cma
FINDLIB_SUPPORT_CMXA=findlib_support/jsoo_findlib_support.cmxa
endif

COMPILER_LIB=$(addprefix lib/, $(COMPILER_INSTALL))

VERSION := $(shell head -n 1 ../VERSION)

COMPOBJS=commonArg.cmx compileArg.cmx jsoo_compile.cmx
MINIOBJS=commonArg.cmx minifyArg.cmx minify.cmx
LINKEROBJS=commonArg.cmx linkerArg.cmx jsoo_link.cmx
OCAMLC=ocamlfind ocamlc -g  -w +A-4-7-9-37-38-41-44-45-58
OCAMLOPT=ocamlfind ocamlopt -g -w +A-4-7-9-37-38-41-44-45-58
BYTE_COMP=lib/js_of_ocaml_compiler.cma
NAT_COMP=lib/js_of_ocaml_compiler.cmxa

all: $(COMPILER) $(MINIFIER) $(LINKER) $(COMPILER_LIB) $(FINDLIB_SUPPORT) man

lib/%: lib
	make -C lib $*

findlib_support/%: findlib_support lib
	make -C findlib_support $*

$(COMPILER): jsoo_compile.$(BEST)
	mv -f $< $@

$(MINIFIER): minify.$(BEST)
	mv -f $< $@

$(LINKER): jsoo_link.$(BEST)
	mv -f $< $@

man: man/$(MINIFIER).1 man/$(COMPILER).1 man/$(LINKER).1
man/$(MINIFIER).1: $(MINIFIER)
	mkdir -p man
	./$(MINIFIER) --help=groff >$@
man/$(COMPILER).1: $(COMPILER)
	mkdir -p man
	./$(COMPILER) --help=groff >$@
man/$(LINKER).1: $(LINKER)
	mkdir -p man
	./$(LINKER) --help=groff >$@

.INTERMEDIATE: jsoo_compile.byte jsoo_compile.opt minify.byte minify.opt jsoo_link.byte jsoo_link.opt

<<<<<<< HEAD
jsoo_compile.byte: $(BYTE_COMP) findlib_support/jsoo_findlib_support.cma $(COMPOBJS:cmx=cmo)
	$(OCAMLC) $(SAFESTRING) ${addprefix -package , $(PACKAGES)} -linkpkg -o $@ $^
jsoo_compile.opt: $(NAT_COMP) findlib_support/jsoo_findlib_support.cmxa $(COMPOBJS)
	$(OCAMLOPT) $(SAFESTRING) ${addprefix -package , $(PACKAGES)} -linkpkg -o $@ $^

minify.byte: $(BYTE_COMP) $(MINIOBJS:cmx=cmo)
	$(OCAMLC) $(SAFESTRING) ${addprefix -package , $(PACKAGES)} -linkpkg -o $@ $^
minify.opt: $(NAT_COMP) $(MINIOBJS:cmx=cmx)
	$(OCAMLOPT) $(SAFESTRING) ${addprefix -package , $(PACKAGES)} -linkpkg -o $@ $^

jsoo_link.byte: $(BYTE_COMP) $(LINKEROBJS:cmx=cmo)
	$(OCAMLC) $(SAFESTRING) ${addprefix -package , $(PACKAGES)} -linkpkg -o $@ $^
jsoo_link.opt: $(NAT_COMP) $(LINKEROBJS:cmx=cmx)
	$(OCAMLOPT) $(SAFESTRING) ${addprefix -package , $(PACKAGES)} -linkpkg -o $@ $^

%.cmx: %.ml lib/ findlib_support/
=======
jsoo_compile.byte: $(FINDLIB_SUPPORT_CMA)  $(COMPOBJS:cmx=cmo)
jsoo_compile.opt:  $(FINDLIB_SUPPORT_CMXA) $(COMPOBJS)

minify.byte: $(MINIOBJS:cmx=cmo)
minify.opt: $(MINIOBJS)

jsoo_link.byte: $(LINKEROBJS:cmx=cmo)
jsoo_link.opt: $(LINKEROBJS)

%.byte: $(BYTE_COMP)
	$(OCAMLC) $(SAFESTRING) ${addprefix -package , $(PACKAGES)} -linkpkg -o $@ $^
%.opt: $(NAT_COMP)
	$(OCAMLOPT) $(SAFESTRING) ${addprefix -package , $(PACKAGES)} -linkpkg -o $@ $^

%.cmx: %.ml lib $(NAT_COMP)
>>>>>>> Misc: fix
	$(OCAMLOPT) -I lib $(SAFESTRING) ${addprefix -package , $(PACKAGES)} -c $<

%.cmo: %.ml $(BYTE_COMP)
	$(OCAMLC) -I lib $(SAFESTRING) ${addprefix -package , $(PACKAGES)} -c $<

<<<<<<< HEAD
%.cmi: %.mli lib/ findlib_support/
=======
%.cmi: %.mli $(BYTE_COMP)
>>>>>>> Misc: fix
	$(OCAMLC) -I lib $(SAFESTRING) ${addprefix -package , $(PACKAGES)} -c $<

clean:
	rm -f *.cm[aiox] *.cmxa *.cmxs *.o *.a *.opt *.byte
	rm -f $(MINIFIER) $(COMPILER) $(LINKER)
	rm -Rf man
	$(MAKE) -C lib clean
	$(MAKE) -C findlib_support clean

depend: $(COMPOBJS:cmx=ml) $(MINIOBJS:cmx=ml) $(LINKEROBJS:cmx=ml) compileArg.mli minifyArg.mli
	echo $^ | xargs -n1 | sort -u | xargs ocamldep > .depend

.PHONY: depend clean

include .depend
