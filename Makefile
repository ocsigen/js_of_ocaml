NAME=ocaml2js

all: compiler library runtime examples

compiler: $(NAME)

library:
	make -C lib

.PHONY: runtime
runtime:
	make -C runtime

obrowser: library
	make -C obrowser

examples: compiler library
	make -C examples

OBJS=util.cmx dgraph.cmx \
     javascript.cmx js_output.cmx js_simpl.cmx \
     instr.cmx code.cmx primitive.cmx subst.cmx pure_fun.cmx deadcode.cmx \
     flow.cmx inline.cmx \
     tailcall.cmx freevars.cmx phisimpl.cmx \
     linker.cmx generate.cmx parse.cmx main.cmx

tst: util.cmo linker.cmo
	ocamlc str.cma -o $@ $^

compile: $(OBJS:cmx=cmo)
	ocamlc -g str.cma -o $@ $^

$(NAME): $(OBJS)
	ocamlopt str.cmxa -o $@ $^

%.cmx: %.ml
	ocamlopt -c $<

%.cmo: %.ml
	ocamlc -g -c $<

%.cmi: %.mli
	ocamlc -c $<

clean:
	rm -f *.cm[iox] *.o
	make -C lib clean
	make -C runtime clean
	make -C obrowser clean
	make -C examples clean

depend:
	find . -maxdepth 1 -name private -prune -o -regex ".*\\.mli?" | \
	xargs ocamldep > .depend

include .depend
