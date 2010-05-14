
OCAML2JS=../../ocaml2js

boulderdash.js: boulderdash $(OCAML2JS)
	$(OCAML2JS) boulderdash

boulderdash: boulderdash.cmo
	ocamlfind ocamlc -package lwt -linkpkg -o boulderdash \
		-I ../../lib ocaml2js.cma \
		boulderdash.cmo

%.cmo: %.ml ../../lib/ocaml2js.cma
	ocamlfind ocamlc -package lwt -pp "camlp4o ../../lib/pa_js.cmo" -I ../../lib -g -c $<

%.cmi: %.mli
	ocamlc -g -c $<

clean:
	rm -f *.cm[io] boulderdash boulderdash.js
