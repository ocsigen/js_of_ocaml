
OCAML2JS=../../ocaml2js

OBJS=minesweeper.cmo main.cmo

minesweeper.js: minesweeper $(OCAML2JS)
	$(OCAML2JS) minesweeper

minesweeper: $(OBJS)
	ocamlfind ocamlc -package lwt -linkpkg -o minesweeper \
		-I ../../lib ocaml2js.cma \
		$(OBJS)

%.cmo: %.ml ../../lib/ocaml2js.cma
	ocamlc -pp "camlp4o ../../lib/pa_js.cmo" -I ../../lib -g -c $<

main.cmo: minesweeper.cmo

clean:
	rm -f *.cm[io] minesweeper minesweeper.js
