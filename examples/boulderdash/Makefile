
boulderdash.js: boulderdash
	../../compile boulderdash

boulderdash: jSOO.ml js.ml boulderdash.ml stubs.c
	ocamlfind ocamlc -verbose -package lwt -linkpkg -custom -o boulderdash \
		-I ../../lib lwt_js.cmo \
		jSOO.ml js.ml boulderdash.ml stubs.c

clean:
	rm -f *.cm[io] *.o boulderdash boulderdash.js
