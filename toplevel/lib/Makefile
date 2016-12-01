include ../../Makefile.conf
include ../../Makefile.filelist

PACKAGES=js_of_ocaml.ppx js_of_ocaml-compiler js_of_ocaml-compiler.findlib-support compiler-libs

OCAMLC=ocamlfind ocamlc     -w +A-4-7-9-37-38-41-44-45
OCAMLOPT=ocamlfind ocamlopt -w +A-4-7-9-37-38-41-44-45
OCAMLDEP=ocamlfind ocamldep 
all: $(TOPLEVEL_INSTALL)

jsooTop.cmo: jsooTop.ml
	$(OCAMLC) $(SAFESTRING) ${addprefix -package , $(PACKAGES)} \
	-I ../../compiler/lib -I ../../lib -I +compiler-libs \
	-c $< -o $@

%.cmo: %.ml
	$(OCAMLC) $(SAFESTRING) ${addprefix -package , $(PACKAGES)} \
	-I +compiler-libs -package camlp4 \
	-c $< -o $@

%.cmi: %.mli
	$(OCAMLC) $(SAFESTRING) ${addprefix -package , $(PACKAGES)} \
	-I +compiler-libs $< -o $@

clean:
	rm -f *.cm[xioa] *.[ao] *.so *.cmx[sa]
	rm -f jsooTopError.ml

depend: jsooTopError.ml
	$(OCAMLDEP) ${addprefix -package , $(PACKAGES)} \
	jsooTop.ml jsooTopCamlp4.ml jsooTopError.ml jsooTopPpx.ml *.mli > .depend

include .depend
