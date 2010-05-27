
OCAML2JS=../../ocaml2js

cubes.js: cubes $(OCAML2JS)
	$(OCAML2JS) cubes -pretty

cubes: cubes.cmo
	ocamlfind ocamlc -package lwt -linkpkg -o cubes \
		-I ../../lib ocaml2js.cma \
		cubes.cmo

%.cmo: %.ml ../../lib/ocaml2js.cma
	ocamlfind ocamlc -package lwt -pp "camlp4o ../../lib/pa_js.cmo" -I ../../lib -g -c $<

%.cmi: %.mli
	ocamlc -g -c $<

clean:
	rm -f *.cm[io] cubes cubes.js
