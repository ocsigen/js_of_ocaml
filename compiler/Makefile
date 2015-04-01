
include ../Makefile.conf

PACKAGES=cmdliner base64

ifeq "${WITH_FINDLIB}" "YES"
PACKAGES+=findlib
endif

all: $(COMPILER) $(MINIFIER) man

compilerlib: lib/compiler.cma lib/compiler.cmxa lib/compiler.cmxs
	cp compiler.cmi compiler.a lib/

lib/%: %
	cp $< $@

OBJS=compiler_version.cmx myfindlib.cmx \
     util.cmx pretty_print.cmx option.cmx reserved.cmx varPrinter.cmx \
     dgraph.cmx code.cmx javascript.cmx json.cmx vlq64.cmx source_map.cmx \
     js_output.cmx js_simpl.cmx parse_info.cmx js_token.cmx js_parser.cmx \
     js_lexer.cmx parse_js.cmx primitive.cmx annot_parser.cmx annot_lexer.cmx \
     instr.cmx subst.cmx pure_fun.cmx deadcode.cmx \
     flow.cmx specialize.cmx specialize_js.cmx eval.cmx inline.cmx \
     tailcall.cmx freevars.cmx phisimpl.cmx \
     js_traverse.cmx js_assign.cmx js_tailcall.cmx \
     linker.cmx parse_bytecode.cmx generate.cmx driver.cmx \
     pseudoFs.cmx

COMPOBJS=$(OBJS) commonArg.cmx compileArg.cmx compile.cmx
OCAMLC=ocamlfind ocamlc     -w +A-4-7-9-37-38-41-44-45
OCAMLOPT=ocamlfind ocamlopt -w +A-4-7-9-37-38-41-44-45
$(COMPILER): compile.$(BEST)
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

.INTERMEDIATE: compile.byte compile.opt minify.byte minify.opt

compile.byte: $(COMPOBJS:cmx=cmo)
	$(OCAMLC) $(SAFESTRING) ${addprefix -package , $(PACKAGES)} -linkpkg -o $@ $^
compile.opt: $(COMPOBJS)
	$(OCAMLOPT) $(SAFESTRING) ${addprefix -package , $(PACKAGES)} -linkpkg -g -o $@ $^

minify.byte: $(OBJS:cmx=cmo) commonArg.cmo minifyArg.cmo minify.cmo
	$(OCAMLC) $(SAFESTRING) ${addprefix -package , $(PACKAGES)} -linkpkg -o $@ $^
minify.opt: $(OBJS:cmx=cmx) commonArg.cmx minifyArg.cmx minify.cmx
	$(OCAMLOPT) $(SAFESTRING) ${addprefix -package , $(PACKAGES)} -linkpkg -g -o $@ $^


compiler.cmo: $(OBJS:cmx=cmo)
	$(OCAMLC) -pack -o $@ $^

compiler.cma: compiler.cmo
	$(OCAMLC) -a -o $@ $^

compiler.cmx: $(OBJS)
	$(OCAMLOPT) -pack -g -o $@ $^

compiler.cmxa: compiler.cmx
	$(OCAMLOPT) -a -o $@ $^

compiler.cmxs: compiler.cmxa
	$(OCAMLOPT) -shared -g -o $@ $^

VERSION := $(shell head -n 1 ../VERSION)

compiler_version.ml: compiler_version.ml.tmp
	if cmp -s $^ $@; then rm $^; else mv $^ $@; fi

.PHONY: compiler_version.ml.tmp
compiler_version.ml.tmp:
	echo "let s = \"${VERSION}\"" > $@
	echo "let git_version = \"${VERSION_GIT}\"" >> $@

%.cmx: %.ml
	$(OCAMLOPT) $(SAFESTRING) ${addprefix -package , $(PACKAGES)} -for-pack Compiler -g -c $<

%.cmo: %.ml
	$(OCAMLC) $(SAFESTRING) ${addprefix -package , $(PACKAGES)} -c $<

%.cmi: %.mli
	$(OCAMLC) $(SAFESTRING) ${addprefix -package , $(PACKAGES)} -c $<

annot_parser.ml: annot_parser.mli
annot_parser.mli: annot_parser.mly primitive.cmi
	menhir --infer --explain $<

js_parser.ml: js_parser.mli
js_parser.mli: js_parser.mly javascript.cmi js_token.cmi
	menhir --infer --external-tokens Js_token --explain $<

%.ml: %.mll
	ocamllex -q $<
clean:
	rm -f *.cm[aiox] *.cmxa *.cmxs *.o *.a *.conflicts
	rm -f lib/*.cm[aiox] lib/*.cmxa lib/*.cmxs lib/*.o lib/*.a
	rm -f compile.opt compile.byte minify.opt minify.byte
	rm -f $(MINIFIER) $(COMPILER)
	rm -f compiler_version.ml
	rm -f annot_lexer.ml annot_parser.ml annot_parser.mli
	rm -f js_lexer.ml js_parser.ml js_parser.mli
	rm -f myfindlib.ml
	rm -Rf man

.PHONY: depend
depend: compiler_version.ml annot_lexer.ml js_lexer.ml js_parser.ml annot_parser.ml myfindlib.ml
	ocamldep *.ml *.mli > .depend

include .depend
