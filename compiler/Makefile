
include ../Makefile.conf

all: $(COMPILER)

OBJS=util.cmx dgraph.cmx \
     javascript.cmx js_output.cmx js_simpl.cmx \
     instr.cmx code.cmx primitive.cmx subst.cmx pure_fun.cmx deadcode.cmx \
     flow.cmx inline.cmx \
     tailcall.cmx freevars.cmx phisimpl.cmx \
     linker.cmx generate.cmx parse.cmx driver.cmx

COMPOBJS=$(OBJS) main.cmx

compile: $(COMPOBJS:cmx=cmo)
	ocamlfind ocamlc -package findlib,str -linkpkg -o $@ $^

$(COMPILER): $(COMPOBJS)
	ocamlfind ocamlopt -package findlib,str -linkpkg -o $@ $^

%.cmx: %.ml
	ocamlfind ocamlopt -package findlib,str -c $<

%.cmo: %.ml
	ocamlfind ocamlc -package findlib,str -g -c $<

%.cmi: %.mli
	ocamlfind ocamlc -package findlib,str -c $<

clean:
	rm -f *.cm[iox] *.o
	rm -f js_of_ocaml compile

depend:
	ocamldep *.ml *.mli > .depend

include .depend
