
include ../Makefile.conf

all: $(COMPILER) $(MINIFIER)

lib: compiler.cma compiler.cmxa compiler.cmxs

PACKAGES=findlib,str,unix

OBJS=compiler_version.cmx \
     pretty_print.cmx util.cmx reserved.cmx varPrinter.cmx option.cmx \
     dgraph.cmx code.cmx javascript.cmx vlq64.cmx source_map.cmx \
     js_output.cmx js_simpl.cmx parse_info.cmx js_token.cmx js_parser.cmx \
     js_lexer.cmx parse_js.cmx annot_parser.cmx annot_lexer.cmx \
     instr.cmx primitive.cmx subst.cmx pure_fun.cmx deadcode.cmx \
     flow.cmx specialize.cmx specialize_js.cmx eval.cmx inline.cmx \
     tailcall.cmx freevars.cmx phisimpl.cmx \
     js_traverse.cmx js_assign.cmx js_tailcall.cmx \
     linker.cmx generate.cmx parse_bytecode.cmx driver.cmx

COMPOBJS=$(OBJS) compile.cmx

$(COMPILER): compile.$(BEST)
	mv -f $< $@

$(MINIFIER): minify.$(BEST)
	mv -f $< $@

.INTERMEDIATE: compile.byte compile.opt minify.byte minify.opt

compile.byte: $(COMPOBJS:cmx=cmo)
	ocamlfind ocamlc -package $(PACKAGES) -linkpkg -o $@ $^
compile.opt: $(COMPOBJS)
	ocamlfind ocamlopt -package $(PACKAGES) -linkpkg -o $@ $^

minify.byte: $(OBJS:cmx=cmo) minify.cmo
	ocamlfind ocamlc -package $(PACKAGES) -linkpkg -o $@ $^
minify.opt: $(OBJS:cmx=cmx) minify.cmx
	ocamlfind ocamlopt -package $(PACKAGES) -linkpkg -o $@ $^


compiler.cma: $(OBJS:cmx=cmo)
	ocamlfind ocamlc -pack -o $@ $^

local/compiler.cma: compiler.cma
	mkdir -p local
	cp compiler.cma compiler.cmi local/

compiler.cmxa: $(OBJS)
	ocamlfind ocamlopt -pack -o $@ $^

compiler.cmxs: $(OBJS)
	ocamlfind ocamlopt -shared -o $@ $^

VERSION := $(shell head -n 1 ../VERSION)
compiler_version.ml: ../VERSION
	echo "let s = \"${VERSION}\"\n" > $@


%.cmx: %.ml
	ocamlfind ocamlopt -package findlib,str -for-pack Compiler -c $<

%.cmo: %.ml
	ocamlfind ocamlc -package findlib,str -c $<

%.cmi: %.mli
	ocamlfind ocamlc -package findlib,str -c $<

%.ml %.mli: %.mly
	menhir --explain $<

js_parser.ml js_parser.mli: js_parser.mly
	menhir --external-tokens Js_token --explain $<

%.ml: %.mll
	ocamllex $<
clean:
	rm -f *.cm[aiox] *.cmxa *.cmxs *.o *.a *.conflicts
	rm -f compile.opt compile.byte minify.opt minify.byte
	rm -f js_of_ocaml joo_minify
	rm -f compiler_version.ml
	rm -f annot_lexer.ml annot_parser.ml annot_parser.mli
	rm -f js_lexer.ml js_parser.ml js_parser.mli

.PHONY: depend
depend: compiler_version.ml annot_lexer.ml js_parser.ml annot_parser.ml
	ocamldep *.ml *.mli > .depend

include .depend
