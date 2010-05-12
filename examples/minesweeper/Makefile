
OCAML2JS=../../ocaml2js

OBJS=minesweeper.cmo main.cmo

minesweeper.js: minesweeper $(OCAML2JS)
	$(OCAML2JS) minesweeper

minesweeper: $(OBJS) ../../lib/ocaml2js.cma
	ocamlfind ocamlc -package lwt -linkpkg -o minesweeper \
		-I ../../lib ocaml2js.cma \
		$(OBJS)

%.cmo: %.ml
	ocamlc -I ../../lib -g -c $<

main.cmo: minesweeper.cmo

clean:
	rm -f *.cm[io] minesweeper minesweeper.js
