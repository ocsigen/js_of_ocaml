NAME=ocaml2js

all: ${NAME}

OBJS=util.cmx dgraph.cmx \
     javascript.cmx js_output.cmx js_simpl.cmx js_rename.cmx \
     instr.cmx code.cmx subst.cmx deadcode.cmx flow.cmx control.cmx \
     tailcall.cmx freevars.cmx phisimpl.cmx \
     generate.cmx parse.cmx main.cmx

compile: $(OBJS:cmx=cmo)
	ocamlc -g -o $@ $^

${NAME}: $(OBJS)
	ocamlopt -o $@ $^

%.cmx: %.ml
	ocamlopt -c $<

%.cmo: %.ml
	ocamlc -g -c $<

%.cmi: %.mli
	ocamlc -g -c $<

clean:
	rm -f *.cm[ix] *.o

depend:
	find . -name private -prune -o -regex ".*\\.mli?" | xargs \
	ocamldep > .depend

include .depend
