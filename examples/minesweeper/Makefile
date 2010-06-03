
OCAML2JS=../../ocaml2js
JSFILES=../../runtime/runtime.js
OCAMLC=ocamlfind ocamlc -package lwt -pp "camlp4o ../../lib/pa_js.cmo" -I ../../lib
STDLIB=ocaml2js.cma

OBJS=minesweeper.cmo main.cmo

minesweeper.js: minesweeper $(OCAML2JS) $(JSFILES)
	$(OCAML2JS) $(JSFILES) minesweeper

minesweeper: $(OBJS)
	$(OCAMLC) -linkpkg -o $@ $(STDLIB) $^

%.cmo: %.ml ../../lib/ocaml2js.cma
	$(OCAMLC) -g -c $<

main.cmo: minesweeper.cmo

clean:
	rm -f *.cm[io] minesweeper minesweeper.js
