
OCAML2JS=../../ocaml2js
JSFILES=../../runtime/runtime.js
OCAMLC=ocamlfind ocamlc -package lwt -pp "camlp4o ../../lib/pa_js.cmo" -I ../../lib
STDLIB=ocaml2js.cma

boulderdash.js: boulderdash $(OCAML2JS) $(JSFILES)
	$(OCAML2JS) $(JSFILES) boulderdash

boulderdash: boulderdash.cmo
	$(OCAMLC) -linkpkg -o $@ $(STDLIB) $^

%.cmo: %.ml ../../lib/ocaml2js.cma
	$(OCAMLC) -g -c $<

%.cmi: %.mli
	ocamlc -g -c $<

clean:
	rm -f *.cm[io] boulderdash boulderdash.js
