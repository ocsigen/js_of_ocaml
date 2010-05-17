all: lwt_obrowser.cma

lwt_obrowser.cma: lwt_obrowser.mli lwt_obrowser.ml
	ocamlfind ocamlc -package lwt -c lwt_obrowser.mli
	ocamlfind ocamlc -package lwt -c lwt_obrowser.ml
	ocamlc -a -o  $@ lwt_obrowser.cmo