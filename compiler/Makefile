
include ../Makefile.conf

all: $(COMPILER)

lib: compiler.cma compiler.cmxa compiler.cmxs

PACKAGES=findlib,str,unix

OBJS=pretty_print.cmx util.cmx dgraph.cmx \
     code.cmx javascript.cmx js_output.cmx js_simpl.cmx \
     instr.cmx primitive.cmx subst.cmx pure_fun.cmx deadcode.cmx \
     flow.cmx inline.cmx \
     tailcall.cmx freevars.cmx phisimpl.cmx \
     linker.cmx generate.cmx parse_bytecode.cmx driver.cmx

COMPOBJS=$(OBJS) main.cmx

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

clean:
	rm -f *.cm[aiox] *.cmxa *.cmxs *.o *.a
	rm -f js_of_ocaml compile.opt compile.byte

depend:
	ocamldep *.ml *.mli > .depend

include .depend
