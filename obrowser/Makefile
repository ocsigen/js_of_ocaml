JS= startup.js marshall.js obrowser.js ../runtime/runtime.js

all: lwt_obrowser.cma vm.js

jslib.cmo:
	ocamlc -pack -o $@ ../lib/js.cmo ../lib/dom.cmo ../lib/xmlHttpRequest.cmo ../lib/lwt_js.cmo

lwt_obrowser.cma: lwt_obrowser.mli lwt_obrowser.ml jslib.cmo stubs.o
	ocamlfind ocamlc -package lwt,obrowser -c lwt_obrowser.mli
	ocamlfind ocamlc  -pp "camlp4o ../lib/pa_js.cmo" -package lwt,obrowser -c lwt_obrowser.ml
	ocamlmklib -o lwt_obrowser jslib.cmo lwt_obrowser.cmo ../lib/stubs.o stubs.o

stubs.o: stubs.c
	ocamlc -c stubs.c

vm.js: $(JS)
	cat $(JS) > vm.js

clean:
	rm -f *.cm[aio] *.[ao] *.so
	rm -f vm.js
