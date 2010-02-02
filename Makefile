all: compile

OBJS=javascript.cmx js_output.cmx \
     util.cmx instr.cmx code.cmx deadcode.cmx flow.cmx control.cmx \
     generate.cmx parse.cmx main.cmx

compile: $(OBJS:cmx=cmo)
	ocamlc -o $@ $^

compile.opt: $(OBJS)
	ocamlopt -o $@ $^

%.cmx: %.ml
	ocamlopt -c $<

%.cmo: %.ml
	ocamlc -c $<

%.cmi: %.mli
	ocamlc -c $<

clean:
	rm -f *.cm[ix] *.o

depend:
	find . -name private -prune -o -regex ".*\\.mli?" | xargs \
	ocamldep > .depend

include .depend
