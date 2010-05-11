
OBJS=js.cmo nullable.cmo jsString.cmo jsArray.cmo lwt_js.cmo stubs.o dom.cmo
LIB=ocaml2js

all: $(LIB).cma

$(LIB).cma: $(OBJS)
	ocamlmklib -o $(LIB) $(OBJS)

%.cmo: %.ml
	ocamlfind ocamlc -package lwt -c $<

%.cmi: %.mli
	ocamlc -c $<

%.o: %.c
	ocamlfind ocamlc -package lwt -c $<

depend:
	find . -maxdepth 1 -name private -prune -o -regex ".*\\.mli?" | \
	xargs ocamldep > .depend

include .depend
