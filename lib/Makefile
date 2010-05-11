
OBJS=js.cmo nullable.cmo jsString.cmo lwt_js.cmo
LIB=ocaml2js

all: $(LIB).cma

$(LIB).cma: $(OBJS)
	ocamlmklib -o $(LIB) $(OBJS)

%.cmo: %.ml
	ocamlfind ocamlc -package lwt -c $<

%.cmi: %.mli
	ocamlc -c $<

depend:
	find . -maxdepth 1 -name private -prune -o -regex ".*\\.mli?" | \
	xargs ocamldep > .depend

include .depend
