
include ../Makefile.conf

PACKAGES=cmdliner base64


ifeq "${WITH_FINDLIB}" "YES"
PACKAGES+= findlib
FINDLIB_SUPPORT_OBJ=jsoo_findlib_support_internal.cmx
FINDLIB_SUPPORT=jsoo_findlib_support.cma \
                jsoo_findlib_support.cmx \
                jsoo_findlib_support.cmxa \
                jsoo_findlib_support.cmxs
endif

all: $(COMPILER) $(MINIFIER) $(FINDLIB_SUPPORT) man

compilerlib: lib/compiler.cma lib/compiler.cmxa lib/compiler.cmxs lib/compiler.cmx
	cp compiler.cmi compiler.a lib/

lib/%: %
	cp $< $@

OBJS=compiler_version.cmx \
     util.cmx pretty_print.cmx option.cmx reserved.cmx varPrinter.cmx \
     dgraph.cmx code.cmx javascript.cmx json.cmx vlq64.cmx source_map.cmx \
     js_output.cmx js_simpl.cmx parse_info.cmx js_token.cmx js_parser.cmx \
     js_lexer.cmx parse_js.cmx jsoo_primitive.cmx annot_parser.cmx annot_lexer.cmx \
     instr.cmx jsoo_subst.cmx pure_fun.cmx jsoo_deadcode.cmx \
     flow.cmx specialize.cmx specialize_js.cmx eval.cmx inline.cmx \
     tailcall.cmx freevars.cmx phisimpl.cmx \
     js_traverse.cmx js_assign.cmx \
     linker.cmx parse_bytecode.cmx jsoo_strongly_connected_components.cmx \
     generate_closure.cmx generate.cmx driver.cmx \
     pseudoFs.cmx

COMPOBJS=$(OBJS) $(FINDLIB_SUPPORT_OBJ) commonArg.cmx compileArg.cmx jsoo_compile.cmx
MINIOBJS=$(OBJS) commonArg.cmx minifyArg.cmx minify.cmx
OCAMLC=ocamlfind ocamlc -g  -w +A-4-7-9-37-38-41-44-45-58
OCAMLOPT=ocamlfind ocamlopt -g -w +A-4-7-9-37-38-41-44-45-58
$(COMPILER): jsoo_compile.$(BEST)
	mv -f $< $@

$(MINIFIER): minify.$(BEST)
	mv -f $< $@


man: man/$(MINIFIER).1 man/$(COMPILER).1
man/$(MINIFIER).1: $(MINIFIER)
	mkdir -p man
	./$(MINIFIER) --help=groff >$@
man/$(COMPILER).1: $(COMPILER)
	mkdir -p man
	./$(COMPILER) --help=groff >$@

.INTERMEDIATE: jsoo_compile.byte jsoo_compile.opt minify.byte minify.opt

jsoo_compile.byte: $(COMPOBJS:cmx=cmo)
	$(OCAMLC) $(SAFESTRING) ${addprefix -package , $(PACKAGES)} -linkpkg -o $@ $^
jsoo_compile.opt: $(COMPOBJS)
	$(OCAMLOPT) $(SAFESTRING) ${addprefix -package , $(PACKAGES)} -linkpkg -g -o $@ $^

minify.byte: $(MINIOBJS:cmx=cmo)
	$(OCAMLC) $(SAFESTRING) ${addprefix -package , $(PACKAGES)} -linkpkg -o $@ $^
minify.opt: $(MINIOBJS:cmx=cmx)
	$(OCAMLOPT) $(SAFESTRING) ${addprefix -package , $(PACKAGES)} -linkpkg -g -o $@ $^


compiler.cmo: $(OBJS:cmx=cmo)
	$(OCAMLC) -pack -o $@ $^

compiler.cmi: compiler.cma

compiler.cma: compiler.cmo
	$(OCAMLC) -a -o $@ $^

compiler.cmx: $(OBJS)
	$(OCAMLOPT) -pack -g -o $@ $^

compiler.cmxa: compiler.cmx
	$(OCAMLOPT) -a -o $@ $^

compiler.cmxs: compiler.cmxa
	$(OCAMLOPT) -shared -g -o $@ $^

jsoo_findlib_support.cmo: compiler.cmo
jsoo_findlib_support.cmx: compiler.cmx

jsoo_findlib_support.cma: jsoo_findlib_support.cmo
	$(OCAMLC) -a -o $@ $^

jsoo_findlib_support.cmxa: jsoo_findlib_support.cmx
	$(OCAMLOPT) -a -o $@ $^

jsoo_findlib_support.cmxs: jsoo_findlib_support.cmxa
	$(OCAMLOPT) -shared -g -o $@ $^

VERSION := $(shell head -n 1 ../VERSION)

compiler_version.ml: compiler_version.ml.tmp
	if cmp -s $^ $@; then rm $^; else mv $^ $@; fi

.PHONY: compiler_version.ml.tmp
compiler_version.ml.tmp:
	echo "let s = \"${VERSION}\"" > $@
	echo "let git_version = \"${VERSION_GIT}\"" >> $@

%.cmx: %.ml
	$(OCAMLOPT) -I +compiler-libs $(SAFESTRING) ${addprefix -package , $(PACKAGES)} -for-pack Compiler -g -c $<

%.cmo: %.ml
	$(OCAMLC) -I +compiler-libs $(SAFESTRING) ${addprefix -package , $(PACKAGES)} -c $<

%.cmi: %.mli
	$(OCAMLC) -I +compiler-libs $(SAFESTRING) ${addprefix -package , $(PACKAGES)} -c $<

annot_parser.ml: annot_parser.mli
annot_parser.mli: annot_parser.mly jsoo_primitive.cmi
	menhir --infer --explain $<

js_parser.ml: js_parser.mli
js_parser.mli: js_parser.mly javascript.cmi js_token.cmi
	menhir --infer --external-tokens Js_token --explain $<

%.ml: %.mll
	ocamllex -q $<
clean:
	rm -f *.cm[aiox] *.cmxa *.cmxs *.o *.a *.conflicts
	rm -f lib/*.cm[aiox] lib/*.cmxa lib/*.cmxs lib/*.o lib/*.a
	rm -f *.opt *.byte
	rm -f $(MINIFIER) $(COMPILER)
	rm -f compiler_version.ml
	rm -f annot_lexer.ml annot_parser.ml annot_parser.mli
	rm -f js_lexer.ml js_parser.ml js_parser.mli
	rm -f util.ml
	rm -Rf man

.PHONY: depend
depend: $(OBJS:cmx=mli) $(COMPOBJS:cmx=ml) $(MINIOBJS:cmx=ml) compileArg.mli minifyArg.mli
	echo $^ | xargs -n1 | sort -u | xargs ocamldep > .depend

include .depend
