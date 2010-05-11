
OCAML2JS=../../ocaml2js

boulderdash.js: boulderdash $(OCAML2JS)
	$(OCAML2JS) boulderdash

boulderdash: boulderdash.ml ../../lib/ocaml2js.cma
	ocamlfind ocamlc -package lwt -linkpkg -o boulderdash \
		-I ../../lib ocaml2js.cma \
		boulderdash.ml

%.cmo: %.ml
	ocamlc -g -c $<

%.cmi: %.mli
	ocamlc -g -c $<

clean:
	rm -f *.cm[io] boulderdash boulderdash.js
