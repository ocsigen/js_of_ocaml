
include ../Makefile.conf

all: $(COMPILER)

lib: compiler.cma compiler.cmxa compiler.cmxs

PACKAGES=findlib,str,unix

OBJS=pretty_print.cmx util.cmx reserved.cmx varPrinter.cmx option.cmx dgraph.cmx \
     code.cmx javascript.cmx js_output.cmx js_simpl.cmx \
	   parse_info.cmx js_parser.cmx js_lexer.cmx parse_js.cmx\
		 annot_parser.cmx annot_lexer.cmx \
     instr.cmx primitive.cmx subst.cmx pure_fun.cmx deadcode.cmx \
     flow.cmx specialize.cmx specialize_js.cmx eval.cmx inline.cmx \
     tailcall.cmx freevars.cmx phisimpl.cmx \
	   js_traverse.cmx js_assign.cmx \
     linker.cmx generate.cmx parse_bytecode.cmx driver.cmx

COMPOBJS=$(OBJS) compile.cmx

$(COMPILER): compile.$(BEST)
	mv -f $< $@

.INTERMEDIATE: compile.byte compile.opt
compile.byte: $(COMPOBJS:cmx=cmo)
	ocamlfind ocamlc -package $(PACKAGES) -linkpkg -o $@ $^

compile.opt: $(COMPOBJS)
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

%.cmx: %.ml
	ocamlfind ocamlopt -package findlib,str -for-pack Compiler -c $<

%.cmo: %.ml
	ocamlfind ocamlc -package findlib,str -c $<

%.cmi: %.mli
	ocamlfind ocamlc -package findlib,str -c $<


%.ml: %.mly
	menhir $<

%.ml: %.mll
	ocamllex $<
clean:
	rm -f *.cm[aiox] *.cmxa *.cmxs *.o *.a
	rm -f js_of_ocaml compile.opt compile.byte

depend:
	${MAKE} --no-print-directory js_lexer.ml
	${MAKE} --no-print-directory annot_lexer.ml
	ocamldep *.ml *.mli > .depend
	menhir --depend js_parser.mly >> .depend
	menhir --depend annot_parser.mly >> .depend



include .depend
