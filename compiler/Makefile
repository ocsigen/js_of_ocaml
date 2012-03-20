
include ../Makefile.conf

all: $(COMPILER)

PACKAGES=findlib,str,unix

OBJS=pretty_print.cmx util.cmx dgraph.cmx \
     javascript.cmx js_output.cmx js_simpl.cmx \
     instr.cmx code.cmx primitive.cmx subst.cmx pure_fun.cmx deadcode.cmx \
     flow.cmx inline.cmx \
     tailcall.cmx freevars.cmx phisimpl.cmx \
     linker.cmx generate.cmx parse.cmx driver.cmx

COMPOBJS=$(OBJS) main.cmx

$(COMPILER): compile.$(BEST)
	mv -f $< $@

.INTERMEDIATE: compile.byte compile.opt
compile.byte: $(COMPOBJS:cmx=cmo)
	ocamlfind ocamlc -package $(PACKAGES) -linkpkg -o $@ $^

compile.opt: $(COMPOBJS)
	ocamlfind ocamlopt -package $(PACKAGES) -linkpkg -o $@ $^

compiler.cma: $(OBJS:cmx=cmo)
	ocamlfind ocamlc -a -o $@ $^

%.cmx: %.ml
	ocamlfind ocamlopt -package findlib,str -c $<

%.cmo: %.ml
	ocamlfind ocamlc -package findlib,str -c $<

%.cmi: %.mli
	ocamlfind ocamlc -package findlib,str -c $<

clean:
	rm -f *.cm[aiox] *.o
	rm -f js_of_ocaml compile.opt compile.byte

depend:
	ocamldep *.ml *.mli > .depend

include .depend
