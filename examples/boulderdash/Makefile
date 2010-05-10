
boulderdash.js: boulderdash
	../../compile boulderdash

boulderdash: obrowser.cma boulderdash.ml
	ocamlfind ocamlc -package lwt -linkpkg -o boulderdash \
		-I ../../lib lwt_js.cmo \
		obrowser.cma boulderdash.ml

obrowser.cma: jSOO.cmo js.cmo stubs.o
	ocamlmklib -o obrowser jSOO.cmo js.cmo stubs.o

%.cmo: %.ml
	ocamlc -g -c $<

%.cmi: %.mli
	ocamlc -g -c $<

%.o: %.c
	ocamlc -g -c $<

clean:
	rm -f *.cm[aio] *.[oa] *.so boulderdash boulderdash.js
