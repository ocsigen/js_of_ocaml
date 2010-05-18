all: lwt_obrowser.cma

lwt_obrowser.cma: lwt_obrowser.mli lwt_obrowser.ml
	ocamlfind ocamlc -I ../lib -package lwt -c lwt_obrowser.mli
	ocamlfind ocamlc -I ../lib  -pp "camlp4o ../lib/pa_js.cmo" -package lwt -c lwt_obrowser.ml
	ocamlc -a -o  $@ lwt_obrowser.cmo